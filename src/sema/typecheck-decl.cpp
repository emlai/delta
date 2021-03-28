#include "typecheck.h"
#pragma warning(push, 0)
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/Support/SaveAndRestore.h>
#pragma warning(pop)
#include "c-import.h"
#include "../ast/module.h"

using namespace delta;

void Typechecker::typecheckType(Type type, AccessLevel userAccessLevel) {
    switch (type.getKind()) {
        case TypeKind::BasicType: {
            if (!type.isOptionalType() && type.isBuiltinType()) {
                validateGenericArgCount(0, type.getGenericArgs(), type.getName(), type.location);
                break;
            }

            auto* basicType = llvm::cast<BasicType>(type.base);

            for (auto genericArg : basicType->genericArgs) {
                typecheckType(genericArg, userAccessLevel);
            }

            auto decls = findDecls(basicType->getQualifiedName());
            Decl* decl;

            if (decls.empty()) {
                auto decls = findDecls(basicType->getName());

                if (decls.empty()) {
                    ERROR(type.location, "unknown type '" << type << "'");
                }

                ASSERT(decls.size() == 1);
                decl = decls[0];
                auto instantiation = llvm::cast<TypeTemplate>(decl)->instantiate(basicType->genericArgs);
                getCurrentModule()->addToSymbolTable(*instantiation);
                declsToTypecheck.push_back(instantiation);
            } else {
                ASSERT(decls.size() == 1);
                decl = decls[0];

                switch (decl->getKind()) {
                    case DeclKind::TypeDecl:
                    case DeclKind::EnumDecl:
                        break;
                    case DeclKind::TypeTemplate:
                        validateGenericArgCount(llvm::cast<TypeTemplate>(decl)->getGenericParams().size(), basicType->genericArgs, basicType->getName(),
                                                type.location);
                        break;
                    default:
                        ERROR(type.location, "'" << type << "' is not a type");
                }
            }

            checkHasAccess(*decl, type.location, userAccessLevel);
            break;
        }
        case TypeKind::ArrayType:
            if (type.isArrayWithRuntimeSize()) {
                auto qualifiedTypeName = getQualifiedTypeName("ArrayRef", type.getElementType());
                if (findDecls(qualifiedTypeName).empty()) {
                    auto* arrayRef = llvm::cast<TypeTemplate>(findDecl("ArrayRef", SourceLocation()));
                    auto* instantiation = arrayRef->instantiate({ type.getElementType() });
                    getCurrentModule()->addToSymbolTable(*instantiation);
                    declsToTypecheck.push_back(instantiation);
                }
            }
            typecheckType(type.getElementType(), userAccessLevel);
            break;
        case TypeKind::TupleType:
            for (auto& element : type.getTupleElements()) {
                typecheckType(element.type, userAccessLevel);
            }
            break;
        case TypeKind::FunctionType:
            for (auto paramType : type.getParamTypes()) {
                typecheckType(paramType, userAccessLevel);
            }
            typecheckType(type.getReturnType(), userAccessLevel);
            break;
        case TypeKind::PointerType: {
            if (type.getPointee().isArrayWithRuntimeSize()) {
                ERROR(type.location, "pointer to array reference is not yet implemented");
            } else {
                typecheckType(type.getPointee(), userAccessLevel);
            }
            break;
        }
        case TypeKind::UnresolvedType:
            llvm_unreachable("invalid unresolved type");
    }
}

void Typechecker::typecheckParamDecl(ParamDecl& decl, AccessLevel userAccessLevel) {
    if (auto existing = getCurrentModule()->getSymbolTable().findInCurrentScope(decl.getName()); !existing.empty()) {
        ERROR_WITH_NOTES(decl.location, getPreviousDefinitionNotes(existing), "redefinition of '" << decl.getName() << "'");
    }

    typecheckType(decl.type, userAccessLevel);
    getCurrentModule()->getSymbolTable().add(decl.getName(), &decl);
}

static bool allPathsReturn(llvm::ArrayRef<Stmt*> block) {
    if (block.empty()) return false;

    switch (block.back()->getKind()) {
        case StmtKind::ReturnStmt:
            return true;
        case StmtKind::ExprStmt: {
            auto& exprStmt = llvm::cast<ExprStmt>(*block.back());
            auto call = llvm::dyn_cast<CallExpr>(&exprStmt.getExpr());
            return call && call->type.isNeverType();
        }
        case StmtKind::IfStmt: {
            auto& ifStmt = llvm::cast<IfStmt>(*block.back());
            return allPathsReturn(ifStmt.getThenBody()) && allPathsReturn(ifStmt.getElseBody());
        }
        case StmtKind::SwitchStmt: {
            auto& switchStmt = llvm::cast<SwitchStmt>(*block.back());
            return llvm::all_of(switchStmt.getCases(), [](auto& c) { return allPathsReturn(c.getStmts()); }) && allPathsReturn(switchStmt.getDefaultStmts());
        }
        default:
            return false;
    }
}

void Typechecker::typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel) {
    for (auto& genericParam : genericParams) {
        if (auto existing = getCurrentModule()->getSymbolTable().find(genericParam.getName()); !existing.empty()) {
            ERROR_WITH_NOTES(genericParam.location, getPreviousDefinitionNotes(existing), "redefinition of '" << genericParam.getName() << "'");
        }

        for (Type constraint : genericParam.constraints) {
            try {
                typecheckType(constraint, userAccessLevel);

                if (!constraint.getDecl()->isInterface()) {
                    ERROR(constraint.location, "only interface types can be used as generic constraints");
                }
            } catch (const CompileError& error) {
                error.print();
            }
        }
    }
}

void Typechecker::typecheckParams(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel) {
    for (auto& param : params) {
        typecheckParamDecl(param, userAccessLevel);
    }
}

void Typechecker::typecheckFunctionDecl(FunctionDecl& decl) {
    if (decl.isTypechecked()) return;
    if (decl.isExtern()) return; // TODO: Typecheck parameters and return type of extern functions.

    TypeDecl* receiverTypeDecl = decl.getTypeDecl();

    Scope scope(&decl, &currentModule->getSymbolTable());
    llvm::SaveAndRestore setCurrentFunction(currentFunction, &decl);

    typecheckParams(decl.getParams(), decl.accessLevel);

    if (!decl.isConstructorDecl() && !decl.isDestructorDecl() && decl.getReturnType()) {
        typecheckType(decl.getReturnType(), decl.accessLevel);
    }

    if (!decl.isExtern()) {
        llvm::SmallPtrSet<FieldDecl*, 32> initializedFields;
        llvm::SaveAndRestore setInitializedFields(currentInitializedFields, &initializedFields);

        if (receiverTypeDecl) {
            Type thisType = receiverTypeDecl->getTypeForPassing();
            auto* varDecl = new VarDecl(thisType, "this", nullptr, &decl, AccessLevel::None, *getCurrentModule(), decl.location);
            getCurrentModule()->addToSymbolTable(varDecl);
        }

        bool delegatedInit = false;

        if (decl.hasBody()) {
            for (auto& stmt : decl.getBody()) {
                {
                    llvm::SaveAndRestore setCurrentStmt(currentStmt, &stmt);

                    if (!typecheckStmt(stmt) && !decl.getReturnType()) {
                        ASSERT(decl.isLambda());
                        throw CompileError();
                    }
                }

                if (decl.isConstructorDecl()) {
                    if (auto* exprStmt = llvm::dyn_cast<ExprStmt>(stmt)) {
                        if (auto* callExpr = llvm::dyn_cast<CallExpr>(&exprStmt->getExpr())) {
                            if (auto* constructorDecl = llvm::dyn_cast_or_null<ConstructorDecl>(callExpr->getCalleeDecl())) {
                                if (constructorDecl->getTypeDecl() == receiverTypeDecl || receiverTypeDecl->hasInterface(*constructorDecl->getTypeDecl())) {
                                    delegatedInit = true;
                                }
                            }
                        }
                    }
                }
            }

            ASSERT(decl.getReturnType());

            // This prevents creating destructors calls during codegen.
            for (auto* movedDecl : movedDecls) {
                switch (movedDecl->getKind()) {
                    case DeclKind::ParamDecl:
                        llvm::cast<ParamDecl>(movedDecl)->isMoved = true;
                        break;
                    case DeclKind::VarDecl:
                        llvm::cast<VarDecl>(movedDecl)->isMoved = true;
                        break;
                    default:
                        break;
                }
            }

            movedDecls.clear();
        }

        if (decl.isConstructorDecl() && !delegatedInit) {
            for (auto& field : decl.getTypeDecl()->fields) {
                if (!field.defaultValue && initializedFields.count(&field) == 0) {
                    WARN(decl.location, "constructor doesn't initialize member variable '" << field.getName() << "'");
                }
            }
        }
    }

    if ((!receiverTypeDecl || !receiverTypeDecl->isInterface()) && !decl.getReturnType().isVoid() && !allPathsReturn(decl.getBody())) {
        if (decl.getReturnType().isNeverType()) {
            WARN(decl.location, "'" << decl.getName() << "' is declared to never return but it does return");
        } else {
            REPORT_ERROR(decl.location, "'" << decl.getName() << "' is missing a return statement");
        }
    }

    decl.setTypechecked(true);
}

void Typechecker::typecheckFunctionTemplate(FunctionTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.accessLevel);
}

void Typechecker::typecheckTypeDecl(TypeDecl& decl) {
    for (Type interface : decl.interfaces) {
        typecheckType(interface, decl.accessLevel);
        auto* interfaceDecl = interface.getDecl();

        if (!interfaceDecl->isInterface()) {
            REPORT_ERROR(interface.location, "'" << interface << "' is not an interface");
            continue;
        }

        std::string errorReason;
        if (!providesInterfaceRequirements(decl, *interfaceDecl, &errorReason)) {
            REPORT_ERROR(decl.location, "'" << decl.getName() << "' " << errorReason << " required by interface '" << interfaceDecl->getName() << "'");
        }
    }

    TypeDecl* realDecl;

    if (decl.isInterface()) {
        // TODO: Move this to typecheckModule to the pre-typechecking phase?
        realDecl = llvm::cast<TypeDecl>(decl.instantiate({ { "This", decl.getType() } }, {}));
    } else {
        realDecl = &decl;
    }

    for (auto& fieldDecl : realDecl->fields) {
        typecheckFieldDecl(fieldDecl);
    }

    for (auto& methodDecl : realDecl->methods) {
        typecheckMethodDecl(*methodDecl);
    }
}

void Typechecker::typecheckTypeTemplate(TypeTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.accessLevel);
}

void Typechecker::typecheckEnumDecl(EnumDecl& decl) {
    std::vector<const EnumCase*> cases = map(decl.getCases(), [](const EnumCase& c) { return &c; });
    std::sort(cases.begin(), cases.end(), [](auto* a, auto* b) { return a->getName() < b->getName(); });
    auto it = std::adjacent_find(cases.begin(), cases.end(), [](auto* a, auto* b) { return a->getName() == b->getName(); });

    if (it != cases.end()) {
        ERROR((*it)->location, "duplicate enum case '" << (*it)->getName() << "'");
    }

    for (auto& enumCase : decl.getCases()) {
        typecheckExpr(*enumCase.getValue());

        if (enumCase.getAssociatedType()) {
            typecheckType(enumCase.getAssociatedType(), enumCase.accessLevel);
        }
    }
}

void Typechecker::typecheckVarDecl(VarDecl& decl) {
    Type declaredType = decl.type;
    if (declaredType) {
        typecheckType(declaredType, !decl.isGlobal() ? AccessLevel::None : decl.accessLevel);
    }

    if (decl.initializer) {
        try {
            typecheckExpr(*decl.initializer, false, declaredType);
        } catch (const CompileError&) {
            if (!decl.isGlobal()) getCurrentModule()->addToSymbolTable(decl);
            throw;
        }
    }

    if (!decl.isGlobal()) getCurrentModule()->addToSymbolTable(decl);
    if (!decl.initializer) return;
    Type initializerType = decl.initializer->type;
    if (!initializerType) return;

    if (declaredType) {
        if (auto converted = convert(decl.initializer, declaredType)) {
            decl.initializer = converted;
        } else {
            const char* hint = "";

            if (initializerType.isNull()) {
                ASSERT(!declaredType.isOptionalType());
                hint = " (add '?' to the type to make it nullable)";
            }

            ERROR(decl.initializer->location, "cannot assign '" << initializerType << "' to '" << declaredType << "'" << hint);
        }
    } else {
        if (initializerType.isNull()) {
            ERROR(decl.location, "couldn't infer type of '" << decl.getName() << "', add a type annotation");
        }

        decl.type = initializerType.withMutability(decl.type.getMutability());
    }

    if (!decl.type.isImplicitlyCopyable()) {
        setMoved(decl.initializer, true);
    }
}

void Typechecker::typecheckFieldDecl(FieldDecl& decl) {
    typecheckType(decl.type, std::min(decl.accessLevel, decl.parent->accessLevel));
}

void Typechecker::typecheckImportDecl(ImportDecl& decl, const PackageManifest* manifest) {
    // TODO: Print import search paths as part of the below error messages.

    if (decl.getTarget().endswith(".h")) {
        if (!importCHeader(*currentSourceFile, decl.getTarget(), options, decl.location)) {
            REPORT_ERROR(decl.location, "couldn't import C header file '" << decl.getTarget() << "'");
        }
    } else {
        auto module = importModule(currentSourceFile, manifest, decl.getTarget());
        if (!module) {
            REPORT_ERROR(decl.location, "couldn't import module '" << decl.getTarget() << "': " << module.getError().message());
        }
    }
}

void Typechecker::typecheckTopLevelDecl(Decl& decl, const PackageManifest* manifest) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::FunctionDecl:
            typecheckFunctionDecl(llvm::cast<FunctionDecl>(decl));
            break;
        case DeclKind::MethodDecl:
            llvm_unreachable("no top-level method declarations");
        case DeclKind::GenericParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::ConstructorDecl:
            llvm_unreachable("no top-level constructor declarations");
        case DeclKind::DestructorDecl:
            llvm_unreachable("no top-level destructor declarations");
        case DeclKind::FunctionTemplate:
            typecheckFunctionTemplate(llvm::cast<FunctionTemplate>(decl));
            break;
        case DeclKind::TypeDecl:
            typecheckTypeDecl(llvm::cast<TypeDecl>(decl));
            break;
        case DeclKind::TypeTemplate:
            typecheckTypeTemplate(llvm::cast<TypeTemplate>(decl));
            break;
        case DeclKind::EnumDecl:
            typecheckEnumDecl(llvm::cast<EnumDecl>(decl));
            break;
        case DeclKind::EnumCase:
            llvm_unreachable("no top-level enum case declarations");
        case DeclKind::VarDecl:
            typecheckVarDecl(llvm::cast<VarDecl>(decl));
            break;
        case DeclKind::FieldDecl:
            llvm_unreachable("no top-level field declarations");
        case DeclKind::ImportDecl:
            typecheckImportDecl(llvm::cast<ImportDecl>(decl), manifest);
            break;
    }
}

void Typechecker::typecheckMethodDecl(Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::MethodDecl:
        case DeclKind::ConstructorDecl:
        case DeclKind::DestructorDecl:
            typecheckFunctionDecl(llvm::cast<MethodDecl>(decl));
            break;
        case DeclKind::FunctionTemplate:
            typecheckFunctionTemplate(llvm::cast<FunctionTemplate>(decl));
            break;
        default:
            llvm_unreachable("invalid method declaration kind");
    }
}
