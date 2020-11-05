#include "irgen.h"
#pragma warning(push, 0)
#include <llvm/Support/SaveAndRestore.h>
#pragma warning(pop)
#include "../ast/mangle.h"

using namespace delta;

Function* IRGenerator::getFunctionProto(const FunctionDecl& decl) {
    auto mangledName = mangleFunctionDecl(decl);

    for (auto* function : module->functions) {
        if (function->mangledName == mangledName) {
            return function;
        }
    }

    auto params = map(decl.getParams(), [](const ParamDecl& p) { return Parameter{ValueKind::Parameter, getILType(p.getType()), p.getName()}; });

    if (decl.isMethodDecl()) {
        params.insert(params.begin(), Parameter{ValueKind::Parameter, getILType(decl.getTypeDecl()->getType().getPointerTo()), "this"});
    }

    auto returnType = getILType(decl.isMain() ? Type::getInt() : decl.getReturnType());
    auto function = new Function{
        ValueKind::Function, mangledName, returnType, std::move(params), {}, decl.isExtern(), decl.isVariadic(), decl.getLocation(),
    };
    module->functions.push_back(function);

    for (auto& instantiation : functionInstantiations) {
        if (instantiation.function->mangledName == mangledName) {
            return function;
        }
    }

    functionInstantiations.push_back({&decl, function});
    return function;
}

void IRGenerator::emitFunctionBody(const FunctionDecl& decl, Function& function) {
    nameCounter = 0;
    usedNames.clear();
    currentFunction = &function;
    setInsertPoint(new Block("", &function));
    beginScope();

    auto arg = function.params.begin();

    if (decl.getTypeDecl()) {
        setLocalValue(&*arg++, nullptr);
    }

    for (auto& param : decl.getParams()) {
        setLocalValue(&*arg++, &param);
    }

    if (decl.isDestructorDecl()) {
        for (auto& field : decl.getTypeDecl()->getFields()) {
            if (!field.getType().getDestructor()) continue;
            deferDestructorCall(emitMemberAccess(&function.params[0], &field), &field);
        }
    }

    for (auto& stmt : decl.getBody()) {
        emitStmt(*stmt);
    }

    endScope();

    // TODO: Remove unreachable final block here instead of emitting a dummy return void.
    //       Currently the LLVM backend handles the removal.
    if (insertBlock->insts.empty() || !llvm::isa<ReturnInst>(insertBlock->insts.back())) {
        createReturn(decl.isMain() ? createConstantInt(Type::getInt(), 0) : nullptr);
    }
}

void IRGenerator::emitFunctionDecl(const FunctionDecl& decl) {
    auto function = getFunctionProto(decl);

    if (!decl.isExtern() && function->body.empty() && decl.hasBody()) { // TODO(ir): Should hasBody check be here?
        emitFunctionBody(decl, *function);
    }
}

Value* IRGenerator::emitVarDecl(const VarDecl& decl) {
    if (decl.getName() == "this") {
        return getThis();
    }

    if (auto* value = getValueOrNull(&decl)) {
        return value;
    }

    ASSERT(decl.getInitializer());
    Value* value = emitExpr(*decl.getInitializer());

    if (decl.getType().isMutable()) {
        // TODO(ir) : add createGlobalVariable
        module->globalVariables.push_back(new GlobalVariable{ValueKind::GlobalVariable, value, decl.getName()});
        value = module->globalVariables.back();
    }

    auto it = globalScope().valuesByDecl.try_emplace(&decl, value);
    ASSERT(it.second);
    return value;
}

void IRGenerator::emitDecl(const Decl& decl) {
    llvm::SaveAndRestore setCurrentDecl(currentDecl, &decl);

    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            llvm_unreachable("handled via FunctionDecl");
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
        case DeclKind::ConstructorDecl:
        case DeclKind::DestructorDecl:
            emitFunctionDecl(llvm::cast<FunctionDecl>(decl));
            break;
        case DeclKind::GenericParamDecl:
            llvm_unreachable("cannot emit generic parameter declaration");
        case DeclKind::VarDecl:
            emitVarDecl(llvm::cast<VarDecl>(decl));
            break;
        case DeclKind::FieldDecl:
            llvm_unreachable("handled via TypeDecl");
        case DeclKind::TypeDecl:
        case DeclKind::ImportDecl:
        case DeclKind::FunctionTemplate:
        case DeclKind::TypeTemplate:
        case DeclKind::EnumDecl:
        case DeclKind::EnumCase:
            break;
    }
}
