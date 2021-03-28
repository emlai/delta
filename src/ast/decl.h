#pragma once

#include <string>
#include <unordered_map>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Casting.h>
#pragma warning(pop)
#include "expr.h"
#include "location.h"
#include "stmt.h"
#include "type.h"
#include "../support/utility.h"

namespace std {
template<>
struct hash<std::vector<delta::Type>> {
    size_t operator()(llvm::ArrayRef<delta::Type> types) const {
        ASSERT(!types.empty());
        size_t hashValue = reinterpret_cast<size_t>(types[0].base);

        for (auto type : types.drop_front()) {
            hashValue ^= reinterpret_cast<size_t>(type.base);
        }

        return hashValue;
    }
};
} // namespace std

namespace llvm {
class StringRef;
}

namespace delta {

struct Module;
struct TypeDecl;
struct FieldDecl;
struct FunctionDecl;
struct EnumDecl;

enum class DeclKind {
    GenericParamDecl,
    FunctionDecl,
    MethodDecl,
    ConstructorDecl,
    DestructorDecl,
    FunctionTemplate,
    TypeDecl,
    TypeTemplate,
    EnumDecl,
    EnumCase,
    VarDecl,
    FieldDecl,
    ParamDecl,
    ImportDecl,
};

enum class AccessLevel {
    None,
    Private,
    Default,
};

inline llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, AccessLevel accessLevel) {
    switch (accessLevel) {
        case AccessLevel::None:
            llvm_unreachable("invalid access level");
        case AccessLevel::Private:
            return stream << "private";
        case AccessLevel::Default:
            // TODO: Rename to "internal" when public access level is added.
            return stream << "public";
    }
    llvm_unreachable("all cases handled");
}

struct Decl {
    virtual ~Decl() = 0;
    bool isVariableDecl() const { return getKind() >= DeclKind::VarDecl && getKind() <= DeclKind::ParamDecl; }
    bool isParamDecl() const { return getKind() == DeclKind::ParamDecl; }
    bool isFunctionDecl() const { return getKind() >= DeclKind::FunctionDecl && getKind() <= DeclKind::DestructorDecl; }
    bool isMethodDecl() const { return getKind() >= DeclKind::MethodDecl && getKind() <= DeclKind::DestructorDecl; }
    bool isGenericParamDecl() const { return getKind() == DeclKind::GenericParamDecl; }
    bool isConstructorDecl() const { return getKind() == DeclKind::ConstructorDecl; }
    bool isDestructorDecl() const { return getKind() == DeclKind::DestructorDecl; }
    bool isFunctionTemplate() const { return getKind() == DeclKind::FunctionTemplate; }
    bool isTypeDecl() const { return getKind() == DeclKind::TypeDecl || getKind() == DeclKind::EnumDecl; }
    bool isTypeTemplate() const { return getKind() == DeclKind::TypeTemplate; }
    bool isEnumDecl() const { return getKind() == DeclKind::EnumDecl; }
    bool isEnumCaseDecl() const { return getKind() == DeclKind::EnumCase; }
    bool isVarDecl() const { return getKind() == DeclKind::VarDecl; }
    bool isFieldDecl() const { return getKind() == DeclKind::FieldDecl; }
    bool isImportDecl() const { return getKind() == DeclKind::ImportDecl; }
    DeclKind getKind() const { return kind; }
    virtual Module* getModule() const = 0;
    virtual llvm::StringRef getName() const = 0;
    bool isMain() const { return getName() == "main"; }
    bool isLambda() const { return isFunctionDecl() && getName().startswith("__lambda"); }
    virtual bool isGlobal() const;
    virtual bool isReferenced() const { return referenced; }
    void setReferenced(bool referenced) { this->referenced = referenced; }
    bool hasBeenMoved() const;
    Decl* instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray) const;
    Decl(DeclKind kind, AccessLevel accessLevel, SourceLocation location) : kind(kind), accessLevel(accessLevel), referenced(false), location(location) {}

    DeclKind kind;
    AccessLevel accessLevel;
    bool referenced;
    SourceLocation location;
};

inline Decl::~Decl() {}

/// Represents any variable declaration, including local variables, global variables, member variables, and parameters.
struct VariableDecl : Decl {
    static bool classof(const Decl* d) { return d->isVariableDecl(); }
    VariableDecl(DeclKind kind, AccessLevel accessLevel, Decl* parent, Type type, SourceLocation location)
    : Decl(kind, accessLevel, location), parent(parent), type(type) {}

    Decl* parent;
    Type type;
};

struct ParamDecl : VariableDecl {
    ParamDecl(Type type, std::string&& name, bool isPublic, SourceLocation location)
    : VariableDecl(DeclKind::ParamDecl, AccessLevel::None, nullptr /* initialized by FunctionDecl constructor */, type, location), name(std::move(name)),
      isPublic(isPublic) {}
    llvm::StringRef getName() const override { return name; }
    Module* getModule() const override { return nullptr; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::ParamDecl; }
    bool operator==(const ParamDecl& other) const { return type == other.type && getName() == other.getName(); }

    std::string name;
    bool isPublic;
    bool isMoved = false;
};

std::vector<ParamDecl> instantiateParams(llvm::ArrayRef<ParamDecl> params, const llvm::StringMap<Type>& genericArgs);

struct GenericParamDecl : Decl {
    GenericParamDecl(std::string&& name, SourceLocation location) : Decl(DeclKind::GenericParamDecl, AccessLevel::None, location), name(std::move(name)) {}
    llvm::StringRef getName() const override { return name; }
    Module* getModule() const override { return nullptr; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::GenericParamDecl; }

    std::string name;
    llvm::SmallVector<Type, 1> constraints;
};

struct FunctionProto {
    FunctionProto(std::string&& name, std::vector<ParamDecl>&& params, Type returnType, bool isVarArg, bool isExtern)
    : name(std::move(name)), params(std::move(params)), returnType(returnType), varArg(isVarArg), external(isExtern) {}
    llvm::StringRef getName() const { return name; }
    llvm::ArrayRef<ParamDecl> getParams() const { return params; }
    llvm::MutableArrayRef<ParamDecl> getParams() { return params; }
    Type getReturnType() const { return returnType; }
    void setReturnType(Type type) { returnType = type; }
    bool isVarArg() const { return varArg; }
    bool isExtern() const { return external; }
    FunctionProto instantiate(const llvm::StringMap<Type>& genericArgs) const;

    std::string name;
    std::vector<ParamDecl> params;
    Type returnType;
    bool varArg;
    bool external;
};

std::string getQualifiedFunctionName(Type receiver, llvm::StringRef name, llvm::ArrayRef<Type> genericArgs);

struct FunctionDecl : Decl {
    FunctionDecl(FunctionProto&& proto, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Module& module, SourceLocation location)
    : FunctionDecl(DeclKind::FunctionDecl, std::move(proto), std::move(genericArgs), accessLevel, module, location) {
        for (auto& param : getParams()) {
            param.parent = this;
        }
    }
    bool isExtern() const { return getProto().isExtern(); }
    bool isVariadic() const { return getProto().isVarArg(); }
    llvm::StringRef getName() const override { return getProto().getName(); }
    std::string getQualifiedName() const;
    Type getReturnType() const { return getProto().getReturnType(); }
    llvm::ArrayRef<ParamDecl> getParams() const { return getProto().getParams(); }
    llvm::MutableArrayRef<ParamDecl> getParams() { return getProto().getParams(); }
    const FunctionProto& getProto() const { return proto; }
    FunctionProto& getProto() { return proto; }
    virtual TypeDecl* getTypeDecl() const { return nullptr; }
    bool hasBody() const { return body.hasValue(); }
    llvm::ArrayRef<Stmt*> getBody() const { return *body; }
    llvm::MutableArrayRef<Stmt*> getBody() { return *body; }
    void setBody(std::vector<Stmt*>&& body) { this->body = std::move(body); }
    FunctionType* getFunctionType() const;
    bool signatureMatches(const FunctionDecl& other, bool matchReceiver = true) const;
    Module* getModule() const override { return &module; }
    FunctionDecl* instantiate(const llvm::StringMap<Type>& genericArgs, llvm::ArrayRef<Type> genericArgsArray);
    bool isTypechecked() const { return typechecked; }
    void setTypechecked(bool typechecked) { this->typechecked = typechecked; }
    static bool classof(const Decl* d) { return d->isFunctionDecl(); }
    FunctionDecl(DeclKind kind, FunctionProto&& proto, std::vector<Type>&& genericArgs, AccessLevel accessLevel, Module& module, SourceLocation location)
    : Decl(kind, accessLevel, location), proto(std::move(proto)), genericArgs(std::move(genericArgs)), module(module), typechecked(false) {}

    FunctionProto proto;
    std::vector<Type> genericArgs;
    llvm::Optional<std::vector<Stmt*>> body;
    Module& module;
    bool typechecked;
};

struct MethodDecl : FunctionDecl {
    MethodDecl(FunctionProto proto, TypeDecl& receiverTypeDecl, std::vector<Type>&& genericArgs, AccessLevel accessLevel, SourceLocation location)
    : MethodDecl(DeclKind::MethodDecl, std::move(proto), receiverTypeDecl, std::move(genericArgs), accessLevel, location) {}
    TypeDecl* getTypeDecl() const override { return typeDecl; }
    MethodDecl* instantiate(const llvm::StringMap<Type>& genericArgs, TypeDecl& typeDecl);
    static bool classof(const Decl* d) { return d->isMethodDecl(); }
    MethodDecl(DeclKind kind, FunctionProto proto, TypeDecl& typeDecl, std::vector<Type>&& genericArgs, AccessLevel accessLevel, SourceLocation location);

    TypeDecl* typeDecl;
};

struct ConstructorDecl : MethodDecl {
    ConstructorDecl(TypeDecl& receiverTypeDecl, std::vector<ParamDecl>&& params, AccessLevel accessLevel, SourceLocation location);
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::ConstructorDecl; }
};

struct DestructorDecl : MethodDecl {
    DestructorDecl(TypeDecl& receiverTypeDecl, SourceLocation location);
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::DestructorDecl; }
};

struct FunctionTemplate : Decl {
    FunctionTemplate(std::vector<GenericParamDecl>&& genericParams, FunctionDecl* functionDecl, AccessLevel accessLevel)
    : Decl(DeclKind::FunctionTemplate, accessLevel, functionDecl->location), genericParams(std::move(genericParams)), functionDecl(functionDecl) {}
    llvm::StringRef getName() const override { return getFunctionDecl()->getName(); }
    std::string getQualifiedName() const { return getFunctionDecl()->getQualifiedName(); }
    bool isReferenced() const override;
    static bool classof(const Decl* d) { return d->isFunctionTemplate(); }
    llvm::ArrayRef<GenericParamDecl> getGenericParams() const { return genericParams; }
    FunctionDecl* getFunctionDecl() const { return functionDecl; }
    FunctionDecl* instantiate(const llvm::StringMap<Type>& genericArgs);
    Module* getModule() const override { return functionDecl->getModule(); }

    std::vector<GenericParamDecl> genericParams;
    FunctionDecl* functionDecl;
    std::unordered_map<std::vector<Type>, FunctionDecl*> instantiations;
};

enum class TypeTag { Struct, Interface, Union, Enum };

/// A non-template function declaration or a function template instantiation.
struct TypeDecl : Decl {
    TypeDecl(TypeTag tag, std::string&& name, std::vector<Type>&& genericArgs, std::vector<Type>&& interfaces, AccessLevel accessLevel, Module& module,
             const TypeDecl* instantiatedFrom, SourceLocation location)
    : Decl(DeclKind::TypeDecl, accessLevel, location), tag(tag), name(std::move(name)), genericArgs(std::move(genericArgs)), interfaces(std::move(interfaces)),
      module(module), instantiatedFrom(instantiatedFrom) {}
    llvm::StringRef getName() const override { return name; }
    std::string getQualifiedName() const;
    bool hasInterface(const TypeDecl& interface) const;
    bool isCopyable() const;
    void addField(FieldDecl&& field);
    void addMethod(Decl* decl);
    std::vector<ConstructorDecl*> getConstructors() const;
    DestructorDecl* getDestructor() const;
    Type getType(Mutability mutability = Mutability::Mutable) const;
    Type getTypeForPassing() const;
    bool passByValue() const { return (isStruct() && isCopyable()) || isUnion(); }
    bool isStruct() const { return tag == TypeTag::Struct; }
    bool isInterface() const { return tag == TypeTag::Interface; }
    bool isUnion() const { return tag == TypeTag::Union; }
    unsigned getFieldIndex(const FieldDecl* field) const;
    Module* getModule() const override { return &module; }
    static bool classof(const Decl* d) { return d->isTypeDecl(); }
    TypeDecl(DeclKind kind, TypeTag tag, std::string&& name, AccessLevel accessLevel, Module& module, const TypeDecl* instantiatedFrom, SourceLocation location)
    : Decl(kind, accessLevel, location), tag(tag), name(std::move(name)), module(module), instantiatedFrom(instantiatedFrom) {}

    TypeTag tag;
    std::string name;
    std::vector<Type> genericArgs;
    std::vector<Type> interfaces;
    std::vector<FieldDecl> fields;
    std::vector<Decl*> methods;
    Module& module;
    const TypeDecl* instantiatedFrom;
};

struct TypeTemplate : Decl {
    TypeTemplate(std::vector<GenericParamDecl>&& genericParams, TypeDecl* typeDecl, AccessLevel accessLevel)
    : Decl(DeclKind::TypeTemplate, accessLevel, typeDecl->location), genericParams(std::move(genericParams)), typeDecl(typeDecl) {}
    llvm::ArrayRef<GenericParamDecl> getGenericParams() const { return genericParams; }
    llvm::StringRef getName() const override { return typeDecl->getName(); }
    TypeDecl* instantiate(const llvm::StringMap<Type>& genericArgs);
    TypeDecl* instantiate(llvm::ArrayRef<Type> genericArgs);
    Module* getModule() const override { return typeDecl->getModule(); }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::TypeTemplate; }

    std::vector<GenericParamDecl> genericParams;
    TypeDecl* typeDecl;
    std::unordered_map<std::vector<Type>, TypeDecl*> instantiations;
};

struct EnumCase : VariableDecl {
    EnumCase(std::string&& name, Expr* value, Type associatedType, AccessLevel accessLevel, SourceLocation location);
    llvm::StringRef getName() const override { return name; }
    Expr* getValue() const { return value; }
    Type getAssociatedType() const { return associatedType; }
    EnumDecl* getEnumDecl() const { return llvm::cast<EnumDecl>(parent); }
    Module* getModule() const override { return parent->getModule(); }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::EnumCase; }

    std::string name;
    Expr* value;
    Type associatedType;
};

struct EnumDecl : TypeDecl {
    EnumDecl(std::string&& name, std::vector<EnumCase>&& cases, AccessLevel accessLevel, Module& module, const TypeDecl* instantiatedFrom, SourceLocation location)
    : TypeDecl(DeclKind::EnumDecl, TypeTag::Enum, std::move(name), accessLevel, module, instantiatedFrom, location), cases(std::move(cases)) {
        for (auto& enumCase : this->cases) {
            enumCase.parent = this;
            enumCase.type = getType();
        }
    }
    bool hasAssociatedValues() const;
    llvm::ArrayRef<EnumCase> getCases() const { return cases; }
    EnumCase* getCaseByName(llvm::StringRef name);
    // TODO: Select tag type to be able to hold all enum values.
    Type getTagType() const { return Type::getInt(); }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::EnumDecl; }

    std::vector<EnumCase> cases;
};

struct VarDecl : VariableDecl {
    VarDecl(Type type, std::string&& name, Expr* initializer, Decl* parent, AccessLevel accessLevel, Module& module, SourceLocation location)
    : VariableDecl(DeclKind::VarDecl, accessLevel, parent, type, location), name(std::move(name)), initializer(initializer), module(module) {}
    llvm::StringRef getName() const override { return name; }
    Module* getModule() const override { return &module; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::VarDecl; }

    std::string name;
    Expr* initializer;
    Module& module;
    bool isMoved = false;
};

struct FieldDecl : VariableDecl {
    FieldDecl(Type type, std::string&& name, Expr* defaultValue, TypeDecl& parent, AccessLevel accessLevel, SourceLocation location)
    : VariableDecl(DeclKind::FieldDecl, accessLevel, &parent, type, location), name(std::move(name)), defaultValue(defaultValue) {}
    llvm::StringRef getName() const override { return name; }
    std::string getQualifiedName() const;
    Module* getModule() const override { return parent->getModule(); }
    FieldDecl instantiate(const llvm::StringMap<Type>& genericArgs, TypeDecl& typeDecl) const;
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::FieldDecl; }

    std::string name;
    Expr* defaultValue;
};

struct ImportDecl : Decl {
    ImportDecl(std::string&& target, Module& module, SourceLocation location)
    : Decl(DeclKind::ImportDecl, AccessLevel::None, location), target(std::move(target)), module(module) {}
    llvm::StringRef getName() const override { return ""; }
    llvm::StringRef getTarget() const { return target; }
    Module* getModule() const override { return &module; }
    static bool classof(const Decl* d) { return d->getKind() == DeclKind::ImportDecl; }

    std::string target;
    Module& module;
};

std::vector<Note> getPreviousDefinitionNotes(llvm::ArrayRef<Decl*> decls);

} // namespace delta
