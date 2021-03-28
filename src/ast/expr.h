#pragma once

#include <string>
#include <utility>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#include <llvm/Support/Casting.h>
#pragma warning(pop)
#include "location.h"
#include "token.h"
#include "type.h"

namespace delta {

struct Decl;
struct FieldDecl;
struct FunctionDecl;
struct Module;

enum class ExprKind {
    VarExpr,
    StringLiteralExpr,
    CharacterLiteralExpr,
    IntLiteralExpr,
    FloatLiteralExpr,
    BoolLiteralExpr,
    NullLiteralExpr,
    UndefinedLiteralExpr,
    ArrayLiteralExpr,
    TupleExpr,
    UnaryExpr,
    BinaryExpr,
    CallExpr,
    SizeofExpr,
    AddressofExpr,
    MemberExpr,
    IndexExpr,
    IndexAssignmentExpr,
    UnwrapExpr,
    LambdaExpr,
    IfExpr,
    ImplicitCastExpr
};

struct Expr {
    virtual ~Expr() = 0;
    bool isVarExpr() const { return getKind() == ExprKind::VarExpr; }
    bool isStringLiteralExpr() const { return getKind() == ExprKind::StringLiteralExpr; }
    bool isCharacterLiteralExpr() const { return getKind() == ExprKind::CharacterLiteralExpr; }
    bool isIntLiteralExpr() const { return getKind() == ExprKind::IntLiteralExpr; }
    bool isFloatLiteralExpr() const { return getKind() == ExprKind::FloatLiteralExpr; }
    bool isBoolLiteralExpr() const { return getKind() == ExprKind::BoolLiteralExpr; }
    bool isNullLiteralExpr() const { return getKind() == ExprKind::NullLiteralExpr; }
    bool isUndefinedLiteralExpr() const { return getKind() == ExprKind::UndefinedLiteralExpr; }
    bool isArrayLiteralExpr() const { return getKind() == ExprKind::ArrayLiteralExpr; }
    bool isTupleExpr() const { return getKind() == ExprKind::TupleExpr; }
    bool isUnaryExpr() const { return getKind() == ExprKind::UnaryExpr; }
    bool isBinaryExpr() const { return getKind() == ExprKind::BinaryExpr; }
    bool isCallExpr() const { return getKind() == ExprKind::CallExpr; }
    bool isSizeofExpr() const { return getKind() == ExprKind::SizeofExpr; }
    bool isAddressofExpr() const { return getKind() == ExprKind::AddressofExpr; }
    bool isMemberExpr() const { return getKind() == ExprKind::MemberExpr; }
    bool isIndexExpr() const { return getKind() == ExprKind::IndexExpr; }
    bool isUnwrapExpr() const { return getKind() == ExprKind::UnwrapExpr; }
    bool isLambdaExpr() const { return getKind() == ExprKind::LambdaExpr; }
    bool isIfExpr() const { return getKind() == ExprKind::IfExpr; }
    bool isImplicitCastExpr() const { return getKind() == ExprKind::ImplicitCastExpr; }
    ExprKind getKind() const { return kind; }
    bool isAssignment() const;
    bool isIncrementOrDecrementExpr() const;
    bool isReferenceExpr() const;
    bool isConstant() const;
    llvm::APSInt getConstantIntegerValue() const;
    bool isLvalue() const;
    Expr* instantiate(const llvm::StringMap<Type>& genericArgs) const;
    std::vector<const Expr*> getSubExprs() const;
    FieldDecl* getFieldDecl() const;
    const Expr* withoutImplicitCast() const;
    bool isThis() const;
    Expr(ExprKind kind, SourceLocation location) : kind(kind), location(location) {}

    ExprKind kind;
    Type type;
    Type assignableType;
    SourceLocation location;
};

inline Expr::~Expr() {}

struct VarExpr : Expr {
    VarExpr(std::string&& identifier, SourceLocation location) : Expr(ExprKind::VarExpr, location), decl(nullptr), identifier(std::move(identifier)) {}
    Decl* getDecl() const { return decl; }
    void setDecl(Decl* newDecl) { decl = newDecl; }
    llvm::StringRef getIdentifier() const { return identifier; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::VarExpr; }

    Decl* decl;
    std::string identifier;
};

struct StringLiteralExpr : Expr {
    StringLiteralExpr(std::string&& value, SourceLocation location) : Expr(ExprKind::StringLiteralExpr, location), value(std::move(value)) {}
    llvm::StringRef getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::StringLiteralExpr; }

    std::string value;
};

struct CharacterLiteralExpr : Expr {
    CharacterLiteralExpr(char value, SourceLocation location) : Expr(ExprKind::CharacterLiteralExpr, location), value(value) {}
    char getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::CharacterLiteralExpr; }

    char value;
};

struct IntLiteralExpr : Expr {
    IntLiteralExpr(llvm::APSInt value, SourceLocation location) : Expr(ExprKind::IntLiteralExpr, location), value(std::move(value)) {}
    const llvm::APSInt& getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IntLiteralExpr; }

    llvm::APSInt value;
};

struct FloatLiteralExpr : Expr {
    FloatLiteralExpr(llvm::APFloat value, SourceLocation location) : Expr(ExprKind::FloatLiteralExpr, location), value(std::move(value)) {}
    const llvm::APFloat& getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::FloatLiteralExpr; }

    llvm::APFloat value;
};

struct BoolLiteralExpr : Expr {
    BoolLiteralExpr(bool value, SourceLocation location) : Expr(ExprKind::BoolLiteralExpr, location), value(value) {}
    bool getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::BoolLiteralExpr; }

    bool value;
};

struct NullLiteralExpr : Expr {
    NullLiteralExpr(SourceLocation location) : Expr(ExprKind::NullLiteralExpr, location) {}
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::NullLiteralExpr; }
};

struct UndefinedLiteralExpr : Expr {
    UndefinedLiteralExpr(SourceLocation location) : Expr(ExprKind::UndefinedLiteralExpr, location) {}
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UndefinedLiteralExpr; }
};

struct ArrayLiteralExpr : Expr {
    ArrayLiteralExpr(std::vector<Expr*>&& elements, SourceLocation location) : Expr(ExprKind::ArrayLiteralExpr, location), elements(std::move(elements)) {}
    llvm::ArrayRef<Expr*> getElements() const { return elements; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::ArrayLiteralExpr; }

    std::vector<Expr*> elements;
};

struct NamedValue {
    NamedValue(Expr* value) : NamedValue("", value) {}
    NamedValue(std::string&& name, Expr* value, SourceLocation location = SourceLocation())
    : name(std::move(name)), value(value), location(location.isValid() ? location : this->value->location) {}
    llvm::StringRef getName() const { return name; }
    void setName(std::string&& newName) { name = newName; }
    Expr* getValue() { return value; }
    const Expr* getValue() const { return value; }
    void setValue(Expr* expr) { value = expr; }

    std::string name; // Empty if no name specified.
    Expr* value;
    SourceLocation location;
};

struct TupleExpr : Expr {
    TupleExpr(std::vector<NamedValue>&& elements, SourceLocation location) : Expr(ExprKind::TupleExpr, location), elements(std::move(elements)) {}
    llvm::ArrayRef<NamedValue> getElements() const { return elements; }
    llvm::MutableArrayRef<NamedValue> getElements() { return elements; }
    const Expr* getElementByName(llvm::StringRef name) const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::TupleExpr; }

    std::vector<NamedValue> elements;
};

struct CallExpr : Expr {
    CallExpr(Expr* callee, std::vector<NamedValue>&& args, std::vector<Type>&& genericArgs, SourceLocation location)
    : Expr(ExprKind::CallExpr, location), callee(callee), args(std::move(args)), genericArgs(std::move(genericArgs)), calleeDecl(nullptr) {}
    bool callsNamedFunction() const { return callee->isVarExpr() || callee->isMemberExpr(); }
    llvm::StringRef getFunctionName() const;
    std::string getQualifiedFunctionName() const;
    bool isMethodCall() const { return callee->isMemberExpr(); }
    bool isBuiltinConversion() const { return Type::isBuiltinScalar(getFunctionName()); }
    bool isBuiltinCast() const { return getFunctionName() == "cast"; }
    bool isMoveInit() const;
    const Expr* getReceiver() const;
    Expr* getReceiver();
    Type getReceiverType() const { return receiverType; }
    void setReceiverType(Type type) { receiverType = type; }
    Decl* getCalleeDecl() const { return calleeDecl; }
    void setCalleeDecl(Decl* callee) { calleeDecl = NOTNULL(callee); }
    const Expr& getCallee() const { return *callee; }
    Expr& getCallee() { return *callee; }
    llvm::ArrayRef<NamedValue> getArgs() const { return args; }
    llvm::MutableArrayRef<NamedValue> getArgs() { return args; }
    void setGenericArgs(std::vector<Type>&& types) { genericArgs = std::move(types); }
    static bool classof(const Expr* e) {
        switch (e->getKind()) {
            case ExprKind::CallExpr:
            case ExprKind::UnaryExpr:
            case ExprKind::BinaryExpr:
            case ExprKind::IndexExpr:
            case ExprKind::IndexAssignmentExpr:
                return true;
            default:
                return false;
        }
    }
    CallExpr(ExprKind kind, Expr* callee, std::vector<NamedValue>&& args, SourceLocation location)
    : Expr(kind, location), callee(callee), args(std::move(args)), calleeDecl(nullptr) {}

    Expr* callee;
    std::vector<NamedValue> args;
    std::vector<Type> genericArgs;
    Type receiverType;
    Decl* calleeDecl;
};

struct UnaryExpr : CallExpr {
    UnaryExpr(UnaryOperator op, Expr* operand, SourceLocation location)
    : CallExpr(ExprKind::UnaryExpr, new VarExpr(toString(op), location), { NamedValue(operand) }, location), op(op) {}
    UnaryOperator getOperator() const { return op; }
    Expr& getOperand() { return *getArgs()[0].getValue(); }
    const Expr& getOperand() const { return *getArgs()[0].getValue(); }
    llvm::APSInt getConstantIntegerValue() const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UnaryExpr; }

    UnaryOperator op;
};

struct BinaryExpr : CallExpr {
    BinaryExpr(BinaryOperator op, Expr* left, Expr* right, SourceLocation location)
    : CallExpr(ExprKind::BinaryExpr, new VarExpr(delta::getFunctionName(op), location), { NamedValue(left), NamedValue(right) }, location), op(op) {}
    BinaryOperator getOperator() const { return op; }
    const Expr& getLHS() const { return *getArgs()[0].getValue(); }
    const Expr& getRHS() const { return *getArgs()[1].getValue(); }
    Expr& getLHS() { return *getArgs()[0].getValue(); }
    Expr& getRHS() { return *getArgs()[1].getValue(); }
    void setLHS(Expr* expr) { getArgs()[0].setValue(expr); }
    void setRHS(Expr* expr) { getArgs()[1].setValue(expr); }
    llvm::APSInt getConstantIntegerValue() const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::BinaryExpr; }

    BinaryOperator op;
};

bool isBuiltinOp(Token::Kind op, Type lhs, Type rhs);

/// A compile-time expression returning the size of a given type in bytes, e.g. 'sizeof(int)'.
struct SizeofExpr : Expr {
    SizeofExpr(Type type, SourceLocation location) : Expr(ExprKind::SizeofExpr, location), type(type) {}
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::SizeofExpr; }

    Type type;
};

/// An expression that returns the memory address stored in a pointer (non-null or nullable)
/// as an unsigned integer, e.g. 'addressof(ptr)'.
struct AddressofExpr : Expr {
    AddressofExpr(Expr* operand, SourceLocation location) : Expr(ExprKind::AddressofExpr, location), operand(operand) {}
    const Expr& getOperand() const { return *operand; }
    Expr& getOperand() { return *operand; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::AddressofExpr; }

    Expr* operand;
};

/// A member access expression using the dot syntax, such as 'a.b'.
struct MemberExpr : Expr {
    MemberExpr(Expr* base, std::string&& member, SourceLocation location) : Expr(ExprKind::MemberExpr, location), base(base), member(std::move(member)) {}
    const Expr* getBaseExpr() const { return base; }
    Expr* getBaseExpr() { return base; }
    llvm::StringRef getMemberName() const { return member; }
    Decl* getDecl() const { return decl; }
    void setDecl(Decl& d) { decl = &d; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::MemberExpr; }

    Expr* base;
    std::string member;
    Decl* decl = nullptr;
};

/// An element access expression using the element's index in brackets: 'base[index]'.
struct IndexExpr : CallExpr {
    IndexExpr(Expr* base, Expr* index, SourceLocation location)
    : CallExpr(ExprKind::IndexExpr, new MemberExpr(base, "[]", location), { NamedValue("", index) }, location) {}
    IndexExpr(Expr* base, Expr* index, Expr* value, SourceLocation location)
    : CallExpr(ExprKind::IndexAssignmentExpr, new MemberExpr(base, "[]=", location), { NamedValue("", index), NamedValue("", value) }, location) {}
    const Expr* getBase() const { return getReceiver(); }
    const Expr* getIndex() const { return getArgs()[0].getValue(); }
    Expr* getBase() { return getReceiver(); }
    Expr* getIndex() { return getArgs()[0].getValue(); }
    void setIndex(Expr* expr) { getArgs()[0].setValue(expr); }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IndexExpr; }
};

/// An assignment to an indexed access: 'base[index] = value'.
struct IndexAssignmentExpr : IndexExpr {
    IndexAssignmentExpr(Expr* base, Expr* index, Expr* value, SourceLocation location) : IndexExpr(base, index, value, location) {}
    const Expr* getValue() const { return getArgs()[1].getValue(); }
    Expr* getValue() { return getArgs()[1].getValue(); }
    void setValue(Expr* expr) { getArgs()[1].setValue(expr); }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IndexAssignmentExpr; }
};

/// A postfix expression that unwraps an optional (nullable) value, yielding the value wrapped by
/// the optional, for example 'foo!'. If the optional is null, the operation triggers an assertion
/// error (by default), or causes undefined behavior (in unchecked mode).
struct UnwrapExpr : Expr {
    UnwrapExpr(Expr* operand, SourceLocation location) : Expr(ExprKind::UnwrapExpr, location), operand(operand) {}
    Expr& getOperand() const { return *operand; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UnwrapExpr; }

    Expr* operand;
};

struct LambdaExpr : Expr {
    LambdaExpr(std::vector<ParamDecl>&& params, Module* module, SourceLocation location);
    FunctionDecl* getFunctionDecl() const { return functionDecl; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::LambdaExpr; }

    FunctionDecl* functionDecl;
};

struct IfExpr : Expr {
    IfExpr(Expr* condition, Expr* thenExpr, Expr* elseExpr, SourceLocation location)
    : Expr(ExprKind::IfExpr, location), condition(condition), thenExpr(thenExpr), elseExpr(elseExpr) {}
    Expr* getCondition() { return condition; }
    Expr* getThenExpr() { return thenExpr; }
    Expr* getElseExpr() { return elseExpr; }
    const Expr* getCondition() const { return condition; }
    const Expr* getThenExpr() const { return thenExpr; }
    const Expr* getElseExpr() const { return elseExpr; }
    void setThenExpr(Expr* expr) { thenExpr = expr; }
    void setElseExpr(Expr* expr) { elseExpr = expr; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IfExpr; }

    Expr* condition;
    Expr* thenExpr;
    Expr* elseExpr;
};

struct ImplicitCastExpr : Expr {
    ImplicitCastExpr(Expr* operand, Type targetType) : Expr(ExprKind::ImplicitCastExpr, operand->location), operand(operand) {
        type = targetType;
        assignableType = targetType;
    }
    Expr* getOperand() const { return operand; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::ImplicitCastExpr; }

    Expr* operand;
};

} // namespace delta
