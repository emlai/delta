#pragma once

#include <ostream>
#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>
#pragma warning(pop)
#include "../support/utility.h"

namespace delta {

class ParamDecl;
class TypeDecl;
class DestructorDecl;
class TupleElement;

enum class Mutability { Mutable, Const };

enum class TypeKind {
    BasicType,
    ArrayType,
    TupleType,
    FunctionType,
    PointerType,
    UnresolvedType, // Placeholder for unresolved generic parameters
};

class TypeBase {
public:
    virtual ~TypeBase() = 0;
    TypeKind getKind() const { return kind; }

protected:
    TypeBase(TypeKind kind) : kind(kind) {}

private:
    const TypeKind kind;
};

inline TypeBase::~TypeBase() {}

struct Type {
public:
    Type() : typeBase(nullptr), mutability(Mutability::Mutable) {}
    Type(TypeBase* typeBase, Mutability mutability, SourceLocation location) : typeBase(typeBase), mutability(mutability), location(location) {}
    TypeBase& operator*() const { return *typeBase; }
    explicit operator bool() const { return typeBase != nullptr; }
    TypeBase* getBase() const { return typeBase; }
    SourceLocation getLocation() const { return location; }
    void setLocation(SourceLocation location) { this->location = location; }
    Type withLocation(SourceLocation location) const { return Type(typeBase, mutability, location); }

    bool isBasicType() const { return getKind() == TypeKind::BasicType; }
    bool isArrayType() const { return getKind() == TypeKind::ArrayType; }
    bool isRangeType() const { return isBasicType() && (getName() == "Range" || getName() == "ClosedRange"); }
    bool isTupleType() const { return getKind() == TypeKind::TupleType; }
    bool isFunctionType() const { return getKind() == TypeKind::FunctionType; }
    bool isPointerType() const { return getKind() == TypeKind::PointerType; }
    // TODO(ir) can be removed?
    bool isImplementedAsPointer() const;
    bool isUnresolvedType() const { return getKind() == TypeKind::UnresolvedType; }
    bool isOptionalType() const { return isBasicType() && getName() == "Optional"; }
    bool isBuiltinType() const { return (isBasicType() && isBuiltinScalar(getName())) || isPointerType() || isNull() || isVoid(); }
    bool isImplicitlyCopyable() const;
    bool isArrayWithConstantSize() const;
    bool isArrayWithRuntimeSize() const;
    bool isArrayWithUnknownSize() const;
    bool isFloatingPoint() const { return isFloat() || isFloat32() || isFloat64() || isFloat80(); }
    bool isEnumType() const;
    bool isIterable() const { return isRangeType(); }
    bool isIncrementable() const { return isInteger() || isFloatingPoint() || isArrayWithUnknownSize(); }
    bool isDecrementable() const { return isInteger() || isFloatingPoint() || isArrayWithUnknownSize(); }
    bool isVoid() const;
    bool isBool() const;
    bool isInt() const;
    bool isInt8() const;
    bool isInt16() const;
    bool isInt32() const;
    bool isInt64() const;
    bool isUInt() const;
    bool isUInt8() const;
    bool isUInt16() const;
    bool isUInt32() const;
    bool isUInt64() const;
    bool isFloat() const;
    bool isFloat32() const;
    bool isFloat64() const;
    bool isFloat80() const;
    bool isChar() const;
    bool isNull() const;
    bool isUndefined() const;

    Type resolve(const llvm::StringMap<Type>& replacements) const;
    bool isInteger() const;
    bool isSigned() const;
    bool isUnsigned() const;
    bool isSignedInteger() const { return isInteger() && isSigned(); }
    bool isUnsignedInteger() const { return isInteger() && isUnsigned(); }
    int getIntegerBitWidth() const;
    bool isMutable() const { return mutability == Mutability::Mutable; }
    Mutability getMutability() const { return mutability; }
    Type withMutability(Mutability m) const { return Type(typeBase, m, location); }
    Type getPointerTo() const;
    Type removePointer() const { return isPointerType() ? getPointee() : *this; }
    Type removeOptional() const { return isOptionalType() ? getWrappedType() : *this; }
    Type removeArrayWithUnknownSize() const { return isArrayWithUnknownSize() ? getElementType() : *this; }
    TypeKind getKind() const { return typeBase->getKind(); }
    TypeDecl* getDecl() const;
    DestructorDecl* getDestructor() const;
    bool equalsIgnoreTopLevelMutable(Type) const;
    bool containsUnresolvedPlaceholder() const;
    void printTo(std::ostream& stream) const;
    std::string toString() const;

    llvm::StringRef getName() const;
    std::string getQualifiedTypeName() const;
    Type getElementType() const;
    int64_t getArraySize() const;
    llvm::ArrayRef<TupleElement> getTupleElements() const;
    llvm::ArrayRef<Type> getGenericArgs() const;
    Type getReturnType() const;
    llvm::ArrayRef<Type> getParamTypes() const;
    Type getPointee() const;
    Type getWrappedType() const;

    static Type getVoid(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getBool(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getInt(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getInt8(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getInt16(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getInt32(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getInt64(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUInt(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUInt8(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUInt16(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUInt32(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUInt64(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    // TODO: Return correct uintptr type by checking target platform pointer size.
    static Type getUIntPtr(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation()) { return getUInt64(mutability, location); }
    static Type getFloat(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getFloat32(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getFloat64(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getFloat80(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getChar(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getNull(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static Type getUndefined(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());

    static bool isBuiltinScalar(llvm::StringRef typeName);

private:
    TypeBase* typeBase;
    Mutability mutability;
    // TODO: Add a dedicated class hierarchy for storing source locations with types, like TypeLoc in Clang and Swift.
    SourceLocation location;
};

void appendGenericArgs(std::string& typeName, llvm::ArrayRef<Type> genericArgs);
std::string getQualifiedTypeName(llvm::StringRef typeName, llvm::ArrayRef<Type> genericArgs);

class BasicType : public TypeBase {
public:
    llvm::ArrayRef<Type> getGenericArgs() const { return genericArgs; }
    llvm::StringRef getName() const { return name; }
    std::string getQualifiedName() const { return getQualifiedTypeName(name, genericArgs); }
    TypeDecl* getDecl() const { return decl; }
    void setDecl(TypeDecl* decl) { this->decl = NOTNULL(decl); }
    static Type get(llvm::StringRef name, llvm::ArrayRef<Type> genericArgs, Mutability mutability = Mutability::Mutable,
                    SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::BasicType; }

private:
    friend Type;
    BasicType(llvm::StringRef name, std::vector<Type>&& genericArgs)
    : TypeBase(TypeKind::BasicType), name(name), genericArgs(std::move(genericArgs)), decl(nullptr) {
        // TODO: Enable this assert.
        // ASSERT(!name.empty());
    }

private:
    std::string name;
    std::vector<Type> genericArgs;
    TypeDecl* decl;
};

class ArrayType : public TypeBase {
public:
    Type getElementType() const { return elementType; }
    int64_t getSize() const { return size; }
    bool hasStaticSize() const { return size >= 0; }
    bool hasRuntimeSize() const { return size == runtimeSize; }
    bool hasUnknownSize() const { return size == unknownSize; }
    static Type getIndexType() { return Type::getInt(); }
    static const int64_t runtimeSize = -1;
    static const int64_t unknownSize = -2;
    static Type get(Type type, int64_t size, Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::ArrayType; }

private:
    ArrayType(Type type, int64_t size) : TypeBase(TypeKind::ArrayType), elementType(type), size(size) {}

private:
    Type elementType;
    int64_t size; ///< Equal to ArrayType::runtimeSize if values of this type know their size at
                  ///< runtime (i.e. this is an ArrayRef), or ArrayType::unknownSize if values of
                  /// this type never know their size.
};

class TupleElement {
public:
    std::string name;
    Type type;
};

bool operator==(const TupleElement&, const TupleElement&);

class TupleType : public TypeBase {
public:
    llvm::ArrayRef<TupleElement> getElements() const { return elements; }
    static Type get(std::vector<TupleElement>&& elements, Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::TupleType; }

private:
    TupleType(std::vector<TupleElement>&& elements) : TypeBase(TypeKind::TupleType), elements(std::move(elements)) {}

private:
    std::vector<TupleElement> elements;
};

class FunctionType : public TypeBase {
public:
    Type getReturnType() const { return returnType; }
    llvm::ArrayRef<Type> getParamTypes() const { return paramTypes; }
    std::vector<ParamDecl> getParamDecls(SourceLocation location = SourceLocation()) const;
    static Type get(Type returnType, std::vector<Type>&& paramTypes, Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::FunctionType; }

private:
    FunctionType(Type returnType, std::vector<Type>&& paramTypes)
    : TypeBase(TypeKind::FunctionType), returnType(returnType), paramTypes(std::move(paramTypes)) {}

private:
    Type returnType;
    std::vector<Type> paramTypes;
};

class PointerType : public TypeBase {
public:
    Type getPointeeType() const { return pointeeType; }
    static Type get(Type pointeeType, Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::PointerType; }

private:
    PointerType(Type pointeeType) : TypeBase(TypeKind::PointerType), pointeeType(pointeeType) {}

private:
    Type pointeeType;
};

namespace OptionalType {
Type get(Type wrappedType, Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
};

class UnresolvedType : public TypeBase {
public:
    static Type get(Mutability mutability = Mutability::Mutable, SourceLocation location = SourceLocation());
    static bool classof(const TypeBase* t) { return t->getKind() == TypeKind::UnresolvedType; }

private:
    UnresolvedType() : TypeBase(TypeKind::UnresolvedType) {}
};

bool operator==(Type, Type);
bool operator!=(Type, Type);
std::ostream& operator<<(std::ostream&, Type);
llvm::raw_ostream& operator<<(llvm::raw_ostream&, Type);

} // namespace delta
