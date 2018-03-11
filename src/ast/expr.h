#pragma once

#include <memory>
#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/Casting.h>
#pragma warning(pop)
#include "location.h"
#include "token.h"
#include "type.h"

namespace delta {

class Decl;
class FunctionDecl;
class Module;

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
    SubscriptExpr,
    UnwrapExpr,
    LambdaExpr,
    IfExpr
};

class Expr {
public:
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
    bool isSubscriptExpr() const { return getKind() == ExprKind::SubscriptExpr; }
    bool isUnwrapExpr() const { return getKind() == ExprKind::UnwrapExpr; }
    bool isLambdaExpr() const { return getKind() == ExprKind::LambdaExpr; }
    bool isIfExpr() const { return getKind() == ExprKind::IfExpr; }

    ExprKind getKind() const { return kind; }
    bool hasType() const { return type.getBase() != nullptr; }
    Type getType() const { return ASSERT(type), type; }
    Type getAssignableType() const { return ASSERT(assignableType), assignableType; }
    void setType(Type type) { ASSERT(type), this->type = type; }
    void setAssignableType(Type type) { ASSERT(type), assignableType = type; }
    bool isAssignment() const;
    bool isIncrementOrDecrementExpr() const;
    bool isPointerOffset() const;
    bool isConstant() const;
    // TODO: Use llvm::APSInt instead of int64_t.
    int64_t getConstantIntegerValue() const;
    bool isLocalVariable() const;
    bool isLvalue() const;
    bool isRvalue() const { return !isLvalue(); }
    SourceLocation getLocation() const { return location; }
    bool canDestructivelyMove() const;
    void setMoved(bool moved);
    std::unique_ptr<Expr> instantiate(const llvm::StringMap<Type>& genericArgs) const;
    std::vector<const Expr*> getSubExprs() const;

protected:
    Expr(ExprKind kind, SourceLocation location) : kind(kind), location(location) {}

private:
    const ExprKind kind;
    Type type;
    Type assignableType;

public:
    const SourceLocation location;
};

inline Expr::~Expr() {}

class VarExpr : public Expr {
public:
    VarExpr(std::string&& identifier, SourceLocation location)
    : Expr(ExprKind::VarExpr, location), decl(nullptr), identifier(std::move(identifier)) {}
    Decl* getDecl() const { return decl; }
    void setDecl(Decl* newDecl) { decl = newDecl; }
    llvm::StringRef getIdentifier() const { return identifier; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::VarExpr; }

private:
    Decl* decl;
    std::string identifier;
};

class StringLiteralExpr : public Expr {
public:
    StringLiteralExpr(std::string&& value, SourceLocation location)
    : Expr(ExprKind::StringLiteralExpr, location), value(std::move(value)) {}
    llvm::StringRef getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::StringLiteralExpr; }

private:
    std::string value;
};

class CharacterLiteralExpr : public Expr {
public:
    CharacterLiteralExpr(char value, SourceLocation location)
    : Expr(ExprKind::CharacterLiteralExpr, location), value(value) {}
    char getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::CharacterLiteralExpr; }

private:
    char value;
};

class IntLiteralExpr : public Expr {
public:
    IntLiteralExpr(int64_t value, SourceLocation location) : Expr(ExprKind::IntLiteralExpr, location), value(value) {}
    int64_t getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IntLiteralExpr; }

private:
    int64_t value;
};

class FloatLiteralExpr : public Expr {
public:
    FloatLiteralExpr(long double value, SourceLocation location)
    : Expr(ExprKind::FloatLiteralExpr, location), value(value) {}
    long double getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::FloatLiteralExpr; }

private:
    long double value;
};

class BoolLiteralExpr : public Expr {
public:
    BoolLiteralExpr(bool value, SourceLocation location) : Expr(ExprKind::BoolLiteralExpr, location), value(value) {}
    bool getValue() const { return value; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::BoolLiteralExpr; }

private:
    bool value;
};

class NullLiteralExpr : public Expr {
public:
    NullLiteralExpr(SourceLocation location) : Expr(ExprKind::NullLiteralExpr, location) {}
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::NullLiteralExpr; }
};

class UndefinedLiteralExpr : public Expr {
public:
    UndefinedLiteralExpr(SourceLocation location) : Expr(ExprKind::UndefinedLiteralExpr, location) {}
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UndefinedLiteralExpr; }
};

class ArrayLiteralExpr : public Expr {
public:
    ArrayLiteralExpr(std::vector<std::unique_ptr<Expr>>&& elements, SourceLocation location)
    : Expr(ExprKind::ArrayLiteralExpr, location), elements(std::move(elements)) {}
    llvm::ArrayRef<std::unique_ptr<Expr>> getElements() const { return elements; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::ArrayLiteralExpr; }

private:
    std::vector<std::unique_ptr<Expr>> elements;
};

class NamedValue {
public:
    NamedValue(std::string&& name, std::unique_ptr<Expr> value, SourceLocation location = SourceLocation())
    : name(std::move(name)), value(std::move(value)),
      location(location.isValid() ? location : this->value->getLocation()) {}
    llvm::StringRef getName() const { return name; }
    void setName(std::string&& newName) { name = newName; }
    Expr* getValue() { return value.get(); }
    const Expr* getValue() const { return value.get(); }
    SourceLocation getLocation() const { return location; }

private:
    std::string name; // Empty if no name specified.
    std::shared_ptr<Expr> value;
    SourceLocation location;
};

class TupleExpr : public Expr {
public:
    TupleExpr(std::vector<NamedValue>&& elements, SourceLocation location)
    : Expr(ExprKind::TupleExpr, location), elements(std::move(elements)) {}
    llvm::ArrayRef<NamedValue> getElements() const { return elements; }
    llvm::MutableArrayRef<NamedValue> getElements() { return elements; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::TupleExpr; }

private:
    std::vector<NamedValue> elements;
};

class CallExpr : public Expr {
public:
    CallExpr(std::unique_ptr<Expr> callee, std::vector<NamedValue>&& args, std::vector<Type>&& genericArgs,
             SourceLocation location)
    : Expr(ExprKind::CallExpr, location), callee(std::move(callee)), args(std::move(args)),
      genericArgs(std::move(genericArgs)), calleeDecl(nullptr) {}
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
    void setCalleeDecl(Decl* callee) { ASSERT(callee), calleeDecl = callee; }
    const Expr& getCallee() const { return *callee; }
    Expr& getCallee() { return *callee; }
    llvm::ArrayRef<NamedValue> getArgs() const { return args; }
    llvm::MutableArrayRef<NamedValue> getArgs() { return args; }
    llvm::ArrayRef<Type> getGenericArgs() const { return genericArgs; }
    void setGenericArgs(std::vector<Type>&& types) { genericArgs = std::move(types); }
    static bool classof(const Expr* e) {
        switch (e->getKind()) {
            case ExprKind::CallExpr:
            case ExprKind::UnaryExpr:
            case ExprKind::BinaryExpr:
            case ExprKind::SubscriptExpr:
                return true;
            default:
                return false;
        }
    }

protected:
    CallExpr(ExprKind kind, std::unique_ptr<Expr> callee, std::vector<NamedValue>&& args, SourceLocation location)
    : Expr(kind, location), callee(std::move(callee)), args(std::move(args)), calleeDecl(nullptr) {}

private:
    std::unique_ptr<Expr> callee;
    std::vector<NamedValue> args;
    std::vector<Type> genericArgs;
    Type receiverType;
    Decl* calleeDecl;
};

inline std::vector<NamedValue> addArg(std::vector<NamedValue>&& args, std::unique_ptr<Expr> arg) {
    args.push_back({ "", std::move(arg), SourceLocation() });
    return std::move(args);
}

class UnaryExpr : public CallExpr {
public:
    UnaryExpr(UnaryOperator op, std::unique_ptr<Expr> operand, SourceLocation location)
    : CallExpr(ExprKind::UnaryExpr, llvm::make_unique<VarExpr>(toString(op.getKind()), location),
               addArg({}, std::move(operand)), location),
      op(op) {}
    UnaryOperator getOperator() const { return op; }
    Expr& getOperand() { return *getArgs()[0].getValue(); }
    const Expr& getOperand() const { return *getArgs()[0].getValue(); }
    int64_t getConstantIntegerValue() const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UnaryExpr; }

private:
    UnaryOperator op;
};

class BinaryExpr : public CallExpr {
public:
    BinaryExpr(BinaryOperator op, std::unique_ptr<Expr> left, std::unique_ptr<Expr> right, SourceLocation location)
    : CallExpr(ExprKind::BinaryExpr, llvm::make_unique<VarExpr>(delta::getFunctionName(op), location),
               addArg(addArg({}, std::move(left)), std::move(right)), location),
      op(op) {}
    BinaryOperator getOperator() const { return op; }
    const Expr& getLHS() const { return *getArgs()[0].getValue(); }
    const Expr& getRHS() const { return *getArgs()[1].getValue(); }
    Expr& getLHS() { return *getArgs()[0].getValue(); }
    Expr& getRHS() { return *getArgs()[1].getValue(); }
    int64_t getConstantIntegerValue() const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::BinaryExpr; }

private:
    BinaryOperator op;
};

bool isBuiltinOp(Token::Kind op, Type lhs, Type rhs);

/// A compile-time expression returning the size of a given type in bytes, e.g. 'sizeof(int)'.
class SizeofExpr : public Expr {
public:
    SizeofExpr(Type type, SourceLocation location) : Expr(ExprKind::SizeofExpr, location), type(type) {}
    Type getType() const { return type; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::SizeofExpr; }

private:
    Type type;
};

/// An expression that returns the memory address stored in a pointer (non-null or nullable)
/// as an unsigned integer, e.g. 'addressof(ptr)'.
class AddressofExpr : public Expr {
public:
    AddressofExpr(std::unique_ptr<Expr> operand, SourceLocation location)
    : Expr(ExprKind::AddressofExpr, location), operand(std::move(operand)) {}
    const Expr& getOperand() const { return *operand; }
    Expr& getOperand() { return *operand; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::AddressofExpr; }

private:
    std::unique_ptr<Expr> operand;
};

/// A member access expression using the dot syntax, such as 'a.b'.
class MemberExpr : public Expr {
public:
    MemberExpr(std::unique_ptr<Expr> base, std::string&& member, SourceLocation location)
    : Expr(ExprKind::MemberExpr, location), base(std::move(base)), member(std::move(member)) {}
    const Expr* getBaseExpr() const { return base.get(); }
    Expr* getBaseExpr() { return base.get(); }
    llvm::StringRef getMemberName() const { return member; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::MemberExpr; }

private:
    std::unique_ptr<Expr> base;
    std::string member;
};

/// An array element access expression using the element's index in brackets, e.g. 'array[index]'.
class SubscriptExpr : public CallExpr {
public:
    SubscriptExpr(std::unique_ptr<Expr> array, std::unique_ptr<Expr> index, SourceLocation location)
    : CallExpr(ExprKind::SubscriptExpr, llvm::make_unique<MemberExpr>(std::move(array), "[]", location),
               { NamedValue("", std::move(index)) }, location) {}
    const Expr* getBaseExpr() const { return getReceiver(); }
    const Expr* getIndexExpr() const { return getArgs()[0].getValue(); }
    Expr* getBaseExpr() { return getReceiver(); }
    Expr* getIndexExpr() { return getArgs()[0].getValue(); }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::SubscriptExpr; }
};

/// A postfix expression that unwraps an optional (nullable) value, yielding the value wrapped by
/// the optional, for example 'foo!'. If the optional is null, the operation triggers an assertion
/// error (by default), or causes undefined behavior (in unchecked mode).
class UnwrapExpr : public Expr {
public:
    UnwrapExpr(std::unique_ptr<Expr> operand, SourceLocation location)
    : Expr(ExprKind::UnwrapExpr, location), operand(std::move(operand)) {}
    Expr& getOperand() const { return *operand; }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::UnwrapExpr; }

private:
    std::unique_ptr<Expr> operand;
};

class LambdaExpr : public Expr {
public:
    LambdaExpr(std::vector<ParamDecl>&& params, std::unique_ptr<Expr> body, SourceLocation location)
    : Expr(ExprKind::LambdaExpr, location), params(std::move(params)), body(std::move(body)) {}
    llvm::ArrayRef<ParamDecl> getParams() const { return params; }
    llvm::MutableArrayRef<ParamDecl> getParams() { return params; }
    Expr* getBody() const { return body.get(); }
    std::unique_ptr<FunctionDecl> lower(Module& module) const;
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::LambdaExpr; }

private:
    std::vector<ParamDecl> params;
    std::unique_ptr<Expr> body;
};

class IfExpr : public Expr {
public:
    IfExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> thenExpr, std::unique_ptr<Expr> elseExpr,
           SourceLocation location)
    : Expr(ExprKind::IfExpr, location), condition(std::move(condition)), thenExpr(std::move(thenExpr)),
      elseExpr(std::move(elseExpr)) {}
    Expr* getCondition() { return condition.get(); }
    Expr* getThenExpr() { return thenExpr.get(); }
    Expr* getElseExpr() { return elseExpr.get(); }
    const Expr* getCondition() const { return condition.get(); }
    const Expr* getThenExpr() const { return thenExpr.get(); }
    const Expr* getElseExpr() const { return elseExpr.get(); }
    static bool classof(const Expr* e) { return e->getKind() == ExprKind::IfExpr; }

private:
    std::unique_ptr<Expr> condition;
    std::unique_ptr<Expr> thenExpr;
    std::unique_ptr<Expr> elseExpr;
};

} // namespace delta
