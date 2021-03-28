#include "decl.h"
#pragma warning(push, 0)
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#pragma warning(pop)
#include "ast.h"

using namespace delta;

FunctionProto FunctionProto::instantiate(const llvm::StringMap<Type>& genericArgs) const {
    auto params = instantiateParams(getParams(), genericArgs);
    auto returnType = getReturnType().resolve(genericArgs);
    std::vector<GenericParamDecl> genericParams;
    return FunctionProto(getName().str(), std::move(params), returnType, isVarArg(), isExtern());
}

FunctionDecl* FunctionTemplate::instantiate(const llvm::StringMap<Type>& genericArgs) {
    ASSERT(!genericParams.empty() && !genericArgs.empty());

    auto orderedGenericArgs = map(genericParams, [&](auto& genericParam) { return genericArgs.find(genericParam.getName())->second; });

    auto it = instantiations.find(orderedGenericArgs);
    if (it != instantiations.end()) return it->second;
    auto instantiation = getFunctionDecl()->instantiate(genericArgs, orderedGenericArgs);
    return instantiations.emplace(std::move(orderedGenericArgs), instantiation).first->second;
}

std::string delta::getQualifiedFunctionName(Type receiver, llvm::StringRef name, llvm::ArrayRef<Type> genericArgs) {
    std::string result;

    if (receiver) {
        result = receiver.toString();
        result += '.';
    }

    result += name;
    appendGenericArgs(result, genericArgs);
    return result;
}

std::string FunctionDecl::getQualifiedName() const {
    Type receiver = getTypeDecl() ? getTypeDecl()->getType() : Type();
    return getQualifiedFunctionName(receiver, getName(), genericArgs);
}

FunctionType* FunctionDecl::getFunctionType() const {
    auto paramTypes = map(getParams(), [](const ParamDecl& p) { return p.type; });
    return llvm::cast<FunctionType>(FunctionType::get(getReturnType(), std::move(paramTypes)).base);
}

bool FunctionDecl::signatureMatches(const FunctionDecl& other, bool matchReceiver) const {
    if (getName() != other.getName()) return false;
    if (matchReceiver && getTypeDecl() != other.getTypeDecl()) return false;
    if (getReturnType() != other.getReturnType()) return false;
    if (getParams() != other.getParams()) return false;
    return true;
}

FunctionDecl* FunctionDecl::instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray) {
    auto proto = getProto().instantiate(genericArgs);
    auto body = ::instantiate(getBody(), genericArgs);

    FunctionDecl* instantiation;

    if (isMethodDecl()) {
        instantiation = new MethodDecl(std::move(proto), *getTypeDecl(), genericArgsArray, accessLevel, location);
    } else {
        instantiation = new FunctionDecl(std::move(proto), genericArgsArray, accessLevel, module, location);
    }

    instantiation->setBody(std::move(body));
    return instantiation;
}

bool FunctionTemplate::isReferenced() const {
    if (Decl::isReferenced()) {
        return true;
    }

    for (auto& instantiation : instantiations) {
        if (instantiation.second->isReferenced()) {
            return true;
        }
    }

    return false;
}

MethodDecl::MethodDecl(DeclKind kind, FunctionProto proto, TypeDecl& typeDecl, std::vector<Type>&& genericArgs, AccessLevel accessLevel, SourceLocation location)
: FunctionDecl(kind, std::move(proto), std::move(genericArgs), accessLevel, *typeDecl.getModule(), location), typeDecl(&typeDecl) {}

MethodDecl* MethodDecl::instantiate(const llvm::StringMap<Type>& genericArgs, TypeDecl& typeDecl) {
    switch (getKind()) {
        case DeclKind::MethodDecl: {
            auto* methodDecl = llvm::cast<MethodDecl>(this);
            auto proto = methodDecl->getProto().instantiate(genericArgs);
            auto instantiation = new MethodDecl(std::move(proto), typeDecl, std::vector<Type>(), accessLevel, methodDecl->location);
            if (methodDecl->hasBody()) {
                instantiation->setBody(::instantiate(methodDecl->getBody(), genericArgs));
            }
            return instantiation;
        }
        case DeclKind::ConstructorDecl: {
            auto* constructorDecl = llvm::cast<ConstructorDecl>(this);
            auto params = instantiateParams(constructorDecl->getParams(), genericArgs);
            auto instantiation = new ConstructorDecl(typeDecl, std::move(params), accessLevel, constructorDecl->location);
            instantiation->setBody(::instantiate(constructorDecl->getBody(), genericArgs));
            return instantiation;
        }
        case DeclKind::DestructorDecl: {
            auto* destructorDecl = llvm::cast<DestructorDecl>(this);
            auto instantiation = new DestructorDecl(typeDecl, destructorDecl->location);
            instantiation->setBody(::instantiate(destructorDecl->getBody(), genericArgs));
            return instantiation;
        }
        default:
            llvm_unreachable("invalid method decl");
    }
}

FieldDecl FieldDecl::instantiate(const llvm::StringMap<Type>& genericArgs, TypeDecl& typeDecl) const {
    return FieldDecl(type.resolve(genericArgs), getName().str(), defaultValue ? defaultValue->instantiate(genericArgs) : nullptr, typeDecl, accessLevel,
                     location);
}

std::vector<ParamDecl> delta::instantiateParams(llvm::ArrayRef<ParamDecl> params, const llvm::StringMap<Type>& genericArgs) {
    return map(params, [&](auto& param) { return ParamDecl(param.type.resolve(genericArgs), param.getName().str(), param.isPublic, param.location); });
}

std::string TypeDecl::getQualifiedName() const {
    return getQualifiedTypeName(getName(), genericArgs);
}

bool TypeDecl::hasInterface(const TypeDecl& interface) const {
    return llvm::any_of(interfaces, [&](Type type) { return type.getDecl() == &interface; });
}

bool TypeDecl::isCopyable() const {
    if (name == "Optional") return genericArgs.front().isImplicitlyCopyable();
    return llvm::any_of(interfaces, [&](Type type) { return type.getName() == "Copyable"; });
}

void TypeDecl::addField(FieldDecl&& field) {
    fields.emplace_back(std::move(field));
}

void TypeDecl::addMethod(Decl* decl) {
    methods.push_back(decl);
}

std::vector<ConstructorDecl*> TypeDecl::getConstructors() const {
    std::vector<ConstructorDecl*> constructors;

    for (auto& decl : methods) {
        if (auto* constructorDecl = llvm::dyn_cast<ConstructorDecl>(decl)) {
            constructors.push_back(constructorDecl);
        }
    }

    return constructors;
}

DestructorDecl* TypeDecl::getDestructor() const {
    for (auto& decl : methods) {
        if (auto* destructorDecl = llvm::dyn_cast<DestructorDecl>(decl)) {
            return destructorDecl;
        }
    }
    return nullptr;
}

Type TypeDecl::getType(Mutability mutability) const {
    return BasicType::get(name, genericArgs, mutability, location);
}

Type TypeDecl::getTypeForPassing() const {
    if ((tag == TypeTag::Struct && !isCopyable()) || tag == TypeTag::Interface) {
        return PointerType::get(getType()).withLocation(location);
    } else {
        return getType();
    }
}

unsigned TypeDecl::getFieldIndex(const FieldDecl* field) const {
    for (auto& p : llvm::enumerate(fields)) {
        if (&p.value() == field) {
            return static_cast<unsigned>(p.index());
        }
    }
    llvm_unreachable("unknown field");
}

TypeDecl* TypeTemplate::instantiate(const llvm::StringMap<Type>& genericArgs) {
    ASSERT(!genericParams.empty() && !genericArgs.empty());
    auto orderedGenericArgs = map(genericParams, [&](auto& genericParam) { return genericArgs.find(genericParam.getName())->second; });

    auto it = instantiations.find(orderedGenericArgs);
    if (it != instantiations.end()) return it->second;

    auto instantiation = llvm::cast<TypeDecl>(typeDecl->instantiate(genericArgs, orderedGenericArgs));
    return instantiations.emplace(std::move(orderedGenericArgs), instantiation).first->second;
}

TypeDecl* TypeTemplate::instantiate(llvm::ArrayRef<Type> genericArgs) {
    ASSERT(genericArgs.size() == genericParams.size());
    llvm::StringMap<Type> genericArgsMap;

    for (auto&& [genericArg, genericParam] : llvm::zip_first(genericArgs, genericParams)) {
        genericArgsMap[genericParam.getName()] = genericArg;
    }

    return instantiate(genericArgsMap);
}

EnumCase::EnumCase(std::string&& name, Expr* value, Type associatedType, AccessLevel accessLevel, SourceLocation location)
: VariableDecl(DeclKind::EnumCase, accessLevel, nullptr, Type() /* initialized by EnumDecl constructor */, location), name(std::move(name)), value(value),
  associatedType(associatedType) {}

EnumCase* EnumDecl::getCaseByName(llvm::StringRef name) {
    for (auto& enumCase : cases) {
        if (enumCase.getName() == name) {
            return &enumCase;
        }
    }
    return nullptr;
}

bool EnumDecl::hasAssociatedValues() const {
    for (auto& enumCase : cases) {
        if (enumCase.getAssociatedType()) {
            return true;
        }
    }
    return false;
}

std::string FieldDecl::getQualifiedName() const {
    return (llvm::cast<TypeDecl>(parent)->getQualifiedName() + "." + getName()).str();
}

bool Decl::hasBeenMoved() const {
    switch (getKind()) {
        case DeclKind::ParamDecl:
            return llvm::cast<ParamDecl>(this)->isMoved;
        case DeclKind::VarDecl:
            return llvm::cast<VarDecl>(this)->isMoved;
        default:
            return false;
    }
}

// TODO: Ensure that the same decl isn't instantiated multiple times with same generic args, to avoid duplicate work.
Decl* Decl::instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray) const {
    switch (getKind()) {
        case DeclKind::ParamDecl:
            llvm_unreachable("handled in FunctionProto::instantiate()");

        case DeclKind::GenericParamDecl:
            llvm_unreachable("cannot instantiate GenericParamDecl");

        case DeclKind::FunctionDecl:
            llvm_unreachable("handled by FunctionDecl::instantiate()");

        case DeclKind::MethodDecl:
        case DeclKind::ConstructorDecl:
        case DeclKind::DestructorDecl:
            llvm_unreachable("handled via TypeDecl");

        case DeclKind::FunctionTemplate:
            llvm_unreachable("handled via FunctionTemplate::instantiate()");

        case DeclKind::TypeDecl: {
            auto* typeDecl = llvm::cast<TypeDecl>(this);
            auto interfaces = map(typeDecl->interfaces, [&](Type type) { return type.resolve(genericArgs); });
            auto instantiation = new TypeDecl(typeDecl->tag, typeDecl->getName().str(), genericArgsArray, std::move(interfaces), accessLevel,
                                              *typeDecl->getModule(), typeDecl, typeDecl->location);
            for (auto& field : typeDecl->fields) {
                auto defaultValue = field.defaultValue ? field.defaultValue->instantiate(genericArgs) : nullptr;
                instantiation->addField(
                    FieldDecl(field.type.resolve(genericArgs), field.getName().str(), defaultValue, *instantiation, field.accessLevel, field.location));
            }

            for (auto& method : typeDecl->methods) {
                if (auto* nonTemplateMethod = llvm::dyn_cast<MethodDecl>(method)) {
                    instantiation->addMethod(nonTemplateMethod->instantiate(genericArgs, *instantiation));
                } else {
                    auto* functionTemplate = llvm::cast<FunctionTemplate>(method);
                    auto* methodDecl = llvm::cast<MethodDecl>(functionTemplate->getFunctionDecl());
                    auto methodInstantiation = methodDecl->MethodDecl::instantiate(genericArgs, *instantiation);

                    std::vector<GenericParamDecl> genericParams;
                    genericParams.reserve(functionTemplate->getGenericParams().size());

                    for (auto& genericParam : functionTemplate->getGenericParams()) {
                        genericParams.emplace_back(genericParam.getName().str(), genericParam.location);
                        genericParams.back().constraints = genericParam.constraints;
                    }

                    auto accessLevel = methodInstantiation->accessLevel;
                    instantiation->addMethod(new FunctionTemplate(std::move(genericParams), methodInstantiation, accessLevel));
                }
            }

            return instantiation;
        }
        case DeclKind::TypeTemplate:
            llvm_unreachable("handled via TypeTemplate::instantiate()");

        case DeclKind::EnumDecl:
        case DeclKind::EnumCase:
            llvm_unreachable("EnumDecl cannot be generic or declared inside another generic type");

        case DeclKind::VarDecl: {
            auto* varDecl = llvm::cast<VarDecl>(this);
            auto type = varDecl->type.resolve(genericArgs);
            auto initializer = varDecl->initializer ? varDecl->initializer->instantiate(genericArgs) : nullptr;
            return new VarDecl(type, varDecl->getName().str(), initializer, varDecl->parent, accessLevel, *varDecl->getModule(), varDecl->location);
        }
        case DeclKind::FieldDecl:
            llvm_unreachable("handled via TypeDecl");

        case DeclKind::ImportDecl:
            llvm_unreachable("cannot instantiate ImportDecl");
    }
    llvm_unreachable("all cases handled");
}

bool Decl::isGlobal() const {
    if (auto variableDecl = llvm::dyn_cast<VariableDecl>(this)) {
        return variableDecl->parent == nullptr;
    }
    return true;
}

ConstructorDecl::ConstructorDecl(TypeDecl& receiverTypeDecl, std::vector<ParamDecl>&& params, AccessLevel accessLevel, SourceLocation location)
: MethodDecl(DeclKind::ConstructorDecl, FunctionProto("init", std::move(params), Type::getVoid(), false, false), receiverTypeDecl, {}, accessLevel, location) {}

DestructorDecl::DestructorDecl(TypeDecl& receiverTypeDecl, SourceLocation location)
: MethodDecl(DeclKind::DestructorDecl, FunctionProto("deinit", {}, Type::getVoid(), false, false), receiverTypeDecl, {}, AccessLevel::None, location) {}

std::vector<Note> delta::getPreviousDefinitionNotes(llvm::ArrayRef<Decl*> decls) {
    return map(decls, [](Decl* decl) { return Note { decl->location, "previous definition here" }; });
}
