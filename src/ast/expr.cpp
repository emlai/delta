#include "expr.h"
#pragma warning(push, 0)
#include <llvm/Support/ErrorHandling.h>
#pragma warning(pop)
#include "decl.h"
#include "token.h"

using namespace delta;

bool Expr::isAssignment() const {
    auto* binaryExpr = llvm::dyn_cast<BinaryExpr>(this);
    return binaryExpr && isAssignmentOperator(binaryExpr->getOperator());
}

bool Expr::isIncrementOrDecrementExpr() const {
    auto* unaryExpr = llvm::dyn_cast<UnaryExpr>(this);
    return unaryExpr && (unaryExpr->getOperator() == Token::Increment || unaryExpr->getOperator() == Token::Decrement);
}

bool Expr::isConstant() const {
    switch (getKind()) {
        case ExprKind::VarExpr: {
            auto* decl = llvm::cast<VarExpr>(this)->getDecl();

            if (auto* varDecl = llvm::dyn_cast<VarDecl>(decl)) {
                if (!varDecl->getType().isMutable() && varDecl->getInitializer()) {
                    return varDecl->getInitializer()->isConstant();
                }
            }

            return false;
        }

        case ExprKind::StringLiteralExpr:
        case ExprKind::CharacterLiteralExpr:
        case ExprKind::IntLiteralExpr:
        case ExprKind::FloatLiteralExpr:
        case ExprKind::BoolLiteralExpr:
        case ExprKind::NullLiteralExpr:
        case ExprKind::UndefinedLiteralExpr:
            return true;

        case ExprKind::ArrayLiteralExpr:
            for (auto& element : llvm::cast<ArrayLiteralExpr>(this)->getElements()) {
                if (!element->isConstant()) {
                    return false;
                }
            }
            return true;

        case ExprKind::TupleExpr:
            for (auto& element : llvm::cast<TupleExpr>(this)->getElements()) {
                if (!element.getValue()->isConstant()) {
                    return false;
                }
            }
            return true;

        case ExprKind::UnaryExpr:
            return llvm::cast<UnaryExpr>(this)->getOperand().isConstant();

        case ExprKind::BinaryExpr:
            return llvm::cast<BinaryExpr>(this)->getLHS().isConstant() && llvm::cast<BinaryExpr>(this)->getRHS().isConstant();

        case ExprKind::CallExpr:
        case ExprKind::SizeofExpr: // TODO: sizeof should be a constant expression.
        case ExprKind::AddressofExpr:
        case ExprKind::MemberExpr:
        case ExprKind::IndexExpr:
        case ExprKind::UnwrapExpr:
        case ExprKind::LambdaExpr:
            return false;

        case ExprKind::ImplicitCastExpr:
            return llvm::cast<ImplicitCastExpr>(this)->getOperand()->isConstant();

        case ExprKind::IfExpr:
            return llvm::cast<IfExpr>(this)->getCondition()->isConstant() && llvm::cast<IfExpr>(this)->getThenExpr()->isConstant() &&
                   llvm::cast<IfExpr>(this)->getElseExpr()->isConstant();
    }

    llvm_unreachable("all cases handled");
}

llvm::APSInt Expr::getConstantIntegerValue() const {
    switch (getKind()) {
        case ExprKind::VarExpr: {
            auto* decl = llvm::cast<VarExpr>(this)->getDecl();

            if (auto* varDecl = llvm::dyn_cast<VarDecl>(decl)) {
                if (!varDecl->getType().isMutable() && varDecl->getInitializer()) {
                    return varDecl->getInitializer()->getConstantIntegerValue();
                }
            }

            llvm_unreachable("not a constant integer");
        }

        case ExprKind::CharacterLiteralExpr:
            return llvm::APSInt::get(llvm::cast<CharacterLiteralExpr>(this)->getValue());

        case ExprKind::IntLiteralExpr: {
            auto& value = llvm::cast<IntLiteralExpr>(this)->getValue();
            return llvm::APSInt(value, value.isSignBitSet());
        }

        case ExprKind::UnaryExpr:
            return llvm::cast<UnaryExpr>(this)->getConstantIntegerValue();

        case ExprKind::BinaryExpr:
            return llvm::cast<BinaryExpr>(this)->getConstantIntegerValue();

        case ExprKind::SizeofExpr:
        case ExprKind::IfExpr:
            llvm_unreachable("unimplemented");

        default:
            llvm_unreachable("not a constant integer");
    }
}

bool Expr::isLvalue() const {
    switch (getKind()) {
        case ExprKind::VarExpr:
        case ExprKind::MemberExpr:
        case ExprKind::IndexExpr:
            return true;
        case ExprKind::UnaryExpr:
            return llvm::cast<UnaryExpr>(this)->getOperator() == Token::Star;
        default:
            return false;
    }
}

Expr* Expr::instantiate(const llvm::StringMap<Type>& genericArgs) const {
    Expr* instantiation = nullptr;

    switch (getKind()) {
        case ExprKind::VarExpr: {
            auto* varExpr = llvm::cast<VarExpr>(this);
            std::string identifier;

            auto it = genericArgs.find(varExpr->getIdentifier());
            if (it != genericArgs.end()) {
                identifier = it->second.getName();
            } else {
                identifier = varExpr->getIdentifier();
            }

            instantiation = new VarExpr(std::move(identifier), varExpr->getLocation());
            break;
        }
        case ExprKind::StringLiteralExpr: {
            auto* stringLiteralExpr = llvm::cast<StringLiteralExpr>(this);
            instantiation = new StringLiteralExpr(stringLiteralExpr->getValue(), stringLiteralExpr->getLocation());
            break;
        }
        case ExprKind::CharacterLiteralExpr: {
            auto* characterLiteralExpr = llvm::cast<CharacterLiteralExpr>(this);
            instantiation = new CharacterLiteralExpr(characterLiteralExpr->getValue(), characterLiteralExpr->getLocation());
            break;
        }
        case ExprKind::IntLiteralExpr: {
            auto* intLiteralExpr = llvm::cast<IntLiteralExpr>(this);
            instantiation = new IntLiteralExpr(intLiteralExpr->getValue(), intLiteralExpr->getLocation());
            break;
        }
        case ExprKind::FloatLiteralExpr: {
            auto* floatLiteralExpr = llvm::cast<FloatLiteralExpr>(this);
            instantiation = new FloatLiteralExpr(floatLiteralExpr->getValue(), floatLiteralExpr->getLocation());
            break;
        }
        case ExprKind::BoolLiteralExpr: {
            auto* boolLiteralExpr = llvm::cast<BoolLiteralExpr>(this);
            instantiation = new BoolLiteralExpr(boolLiteralExpr->getValue(), boolLiteralExpr->getLocation());
            break;
        }
        case ExprKind::NullLiteralExpr: {
            auto* nullLiteralExpr = llvm::cast<NullLiteralExpr>(this);
            instantiation = new NullLiteralExpr(nullLiteralExpr->getLocation());
            break;
        }
        case ExprKind::UndefinedLiteralExpr: {
            auto* undefinedLiteralExpr = llvm::cast<UndefinedLiteralExpr>(this);
            instantiation = new UndefinedLiteralExpr(undefinedLiteralExpr->getLocation());
            break;
        }
        case ExprKind::ArrayLiteralExpr: {
            auto* arrayLiteralExpr = llvm::cast<ArrayLiteralExpr>(this);
            auto elements = ::instantiate(arrayLiteralExpr->getElements(), genericArgs);
            instantiation = new ArrayLiteralExpr(std::move(elements), arrayLiteralExpr->getLocation());
            break;
        }
        case ExprKind::TupleExpr: {
            auto* tupleExpr = llvm::cast<TupleExpr>(this);
            auto elements = map(tupleExpr->getElements(),
                                [&](const NamedValue& element) { return NamedValue(element.getName(), element.getValue()->instantiate(genericArgs)); });
            instantiation = new TupleExpr(std::move(elements), tupleExpr->getLocation());
            break;
        }
        case ExprKind::UnaryExpr: {
            auto* unaryExpr = llvm::cast<UnaryExpr>(this);
            auto operand = unaryExpr->getOperand().instantiate(genericArgs);
            instantiation = new UnaryExpr(unaryExpr->getOperator(), operand, unaryExpr->getLocation());
            break;
        }
        case ExprKind::BinaryExpr: {
            auto* binaryExpr = llvm::cast<BinaryExpr>(this);
            auto lhs = binaryExpr->getLHS().instantiate(genericArgs);
            auto rhs = binaryExpr->getRHS().instantiate(genericArgs);
            instantiation = new BinaryExpr(binaryExpr->getOperator(), lhs, rhs, binaryExpr->getLocation());
            break;
        }
        case ExprKind::CallExpr: {
            auto* callExpr = llvm::cast<CallExpr>(this);
            auto callee = callExpr->getCallee().instantiate(genericArgs);
            auto args = map(callExpr->getArgs(), [&](auto& arg) { return NamedValue(arg.getName(), arg.getValue()->instantiate(genericArgs)); });
            auto callGenericArgs = map(callExpr->getGenericArgs(), [&](Type type) { return type.resolve(genericArgs); });
            instantiation = new CallExpr(callee, std::move(args), std::move(callGenericArgs), callExpr->getLocation());
            break;
        }
        case ExprKind::SizeofExpr: {
            auto* sizeofExpr = llvm::cast<SizeofExpr>(this);
            auto type = sizeofExpr->getType().resolve(genericArgs);
            instantiation = new SizeofExpr(type, sizeofExpr->getLocation());
            break;
        }
        case ExprKind::AddressofExpr: {
            auto* addressofExpr = llvm::cast<AddressofExpr>(this);
            auto operand = addressofExpr->getOperand().instantiate(genericArgs);
            instantiation = new AddressofExpr(operand, addressofExpr->getLocation());
            break;
        }
        case ExprKind::MemberExpr: {
            auto* memberExpr = llvm::cast<MemberExpr>(this);
            auto base = memberExpr->getBaseExpr()->instantiate(genericArgs);
            instantiation = new MemberExpr(base, memberExpr->getMemberName(), memberExpr->getLocation());
            break;
        }
        case ExprKind::IndexExpr: {
            auto* indexExpr = llvm::cast<IndexExpr>(this);
            auto base = indexExpr->getBase()->instantiate(genericArgs);
            auto index = indexExpr->getIndex()->instantiate(genericArgs);
            instantiation = new IndexExpr(base, index, indexExpr->getLocation());
            break;
        }
        case ExprKind::UnwrapExpr: {
            auto* unwrapExpr = llvm::cast<UnwrapExpr>(this);
            auto operand = unwrapExpr->getOperand().instantiate(genericArgs);
            instantiation = new UnwrapExpr(operand, unwrapExpr->getLocation());
            break;
        }
        case ExprKind::LambdaExpr: {
            auto* lambdaExpr = llvm::cast<LambdaExpr>(this);
            auto params = instantiateParams(lambdaExpr->getFunctionDecl()->getParams(), genericArgs);
            ASSERT(lambdaExpr->getFunctionDecl()->getBody().size() == 1);
            auto body = llvm::cast<ReturnStmt>(*lambdaExpr->getFunctionDecl()->getBody().front()).getReturnValue()->instantiate(genericArgs);
            instantiation = new LambdaExpr(std::move(params), body, lambdaExpr->getFunctionDecl()->getModule(), lambdaExpr->getLocation());
            break;
        }
        case ExprKind::IfExpr: {
            auto* ifExpr = llvm::cast<IfExpr>(this);
            auto condition = ifExpr->getCondition()->instantiate(genericArgs);
            auto thenExpr = ifExpr->getThenExpr()->instantiate(genericArgs);
            auto elseExpr = ifExpr->getElseExpr()->instantiate(genericArgs);
            instantiation = new IfExpr(condition, thenExpr, elseExpr, ifExpr->getLocation());
            break;
        }
        case ExprKind::ImplicitCastExpr: {
            auto implicitCastExpr = llvm::cast<ImplicitCastExpr>(this);
            instantiation = new ImplicitCastExpr(implicitCastExpr->getOperand()->instantiate(genericArgs), implicitCastExpr->getType().resolve(genericArgs));
            break;
        }
    }

    return instantiation;
}

std::vector<const Expr*> Expr::getSubExprs() const {
    std::vector<const Expr*> subExprs;

    switch (getKind()) {
        case ExprKind::VarExpr:
        case ExprKind::StringLiteralExpr:
        case ExprKind::CharacterLiteralExpr:
        case ExprKind::IntLiteralExpr:
        case ExprKind::FloatLiteralExpr:
        case ExprKind::BoolLiteralExpr:
        case ExprKind::NullLiteralExpr:
        case ExprKind::UndefinedLiteralExpr:
            break;

        case ExprKind::ArrayLiteralExpr: {
            auto* arrayLiteralExpr = llvm::cast<ArrayLiteralExpr>(this);
            for (auto& element : arrayLiteralExpr->getElements()) {
                subExprs.push_back(element);
            }
            break;
        }
        case ExprKind::TupleExpr: {
            auto* tupleExpr = llvm::cast<TupleExpr>(this);
            for (auto& element : tupleExpr->getElements()) {
                subExprs.push_back(element.getValue());
            }
            break;
        }
        case ExprKind::UnaryExpr:
        case ExprKind::BinaryExpr:
        case ExprKind::IndexExpr:
        case ExprKind::CallExpr: {
            auto* callExpr = llvm::cast<CallExpr>(this);
            subExprs.push_back(&callExpr->getCallee());
            for (auto& arg : callExpr->getArgs()) {
                subExprs.push_back(arg.getValue());
            }
            break;
        }
        case ExprKind::SizeofExpr:
            break;

        case ExprKind::AddressofExpr: {
            auto* addressofExpr = llvm::cast<AddressofExpr>(this);
            subExprs.push_back(&addressofExpr->getOperand());
            break;
        }
        case ExprKind::MemberExpr: {
            auto* memberExpr = llvm::cast<MemberExpr>(this);
            subExprs.push_back(memberExpr->getBaseExpr());
            break;
        }
        case ExprKind::UnwrapExpr: {
            auto* unwrapExpr = llvm::cast<UnwrapExpr>(this);
            subExprs.push_back(&unwrapExpr->getOperand());
            break;
        }
        case ExprKind::LambdaExpr:
            break;

        case ExprKind::IfExpr: {
            auto* ifExpr = llvm::cast<IfExpr>(this);
            subExprs.push_back(ifExpr->getCondition());
            subExprs.push_back(ifExpr->getThenExpr());
            subExprs.push_back(ifExpr->getElseExpr());
            break;
        }
        case ExprKind::ImplicitCastExpr: {
            auto* implicitCastExpr = llvm::cast<ImplicitCastExpr>(this);
            subExprs.push_back(implicitCastExpr->getOperand());
            break;
        }
    }

    return subExprs;
}

FieldDecl* Expr::getFieldDecl() const {
    switch (getKind()) {
        case ExprKind::VarExpr:
            return llvm::dyn_cast<FieldDecl>(llvm::cast<VarExpr>(this)->getDecl());
        case ExprKind::MemberExpr:
            return llvm::dyn_cast_or_null<FieldDecl>(llvm::cast<MemberExpr>(this)->getDecl());
        default:
            return nullptr;
    }
}

const Expr* Expr::withoutImplicitCast() const {
    if (auto implicitCastExpr = llvm::dyn_cast<ImplicitCastExpr>(this)) {
        return implicitCastExpr->getOperand();
    }
    return this;
}

bool Expr::isThis() const {
    return isVarExpr() && llvm::cast<VarExpr>(this)->getIdentifier() == "this";
}

llvm::StringRef CallExpr::getFunctionName() const {
    switch (getCallee().getKind()) {
        case ExprKind::VarExpr:
            return llvm::cast<VarExpr>(getCallee()).getIdentifier();
        case ExprKind::MemberExpr:
            return llvm::cast<MemberExpr>(getCallee()).getMemberName();
        default:
            return "(anonymous function)";
    }
}

static Type getReceiverType(const CallExpr& call) {
    if (call.getCallee().isMemberExpr()) {
        return call.getReceiver()->getType().removeOptional().removePointer();
    }
    return Type();
}

std::string CallExpr::getQualifiedFunctionName() const {
    return ::getQualifiedFunctionName(::getReceiverType(*this), getFunctionName(), {});
}

bool CallExpr::isMoveInit() const {
    if (getFunctionName() != "init") return false;
    if (getArgs().size() != 1) return false;

    if (Type receiverType = ::getReceiverType(*this)) {
        return getArgs()[0].getValue()->getType().equalsIgnoreTopLevelMutable(receiverType);
    }

    return false;
}

const Expr* CallExpr::getReceiver() const {
    if (!isMethodCall()) return nullptr;
    return llvm::cast<MemberExpr>(getCallee()).getBaseExpr();
}

Expr* CallExpr::getReceiver() {
    if (!isMethodCall()) return nullptr;
    return llvm::cast<MemberExpr>(getCallee()).getBaseExpr();
}

llvm::APSInt UnaryExpr::getConstantIntegerValue() const {
    auto operand = getOperand().getConstantIntegerValue();

    switch (getOperator()) {
        case Token::Plus:
            return operand;
        case Token::Minus:
            return -operand;
        case Token::Tilde:
            return ~operand;
        default:
            llvm_unreachable("invalid constant integer prefix operator");
    }
}

bool delta::isBuiltinOp(Token::Kind op, Type left, Type right) {
    if (op == Token::Assignment) return true;
    if (op == Token::DotDot || op == Token::DotDotDot) return false;
    if (op == Token::PointerEqual || op == Token::PointerNotEqual) return true;
    if (left.isPointerType() && right.isPointerType()) return false;
    if (left.isEnumType() && left.equalsIgnoreTopLevelMutable(right)) return true;
    if (left.isEnumType() && right.isInteger()) return true;
    if (left.isInteger() && right.isEnumType()) return true;
    if (left.isImplementedAsPointer() && right.isImplementedAsPointer()) return true;
    return left.isBuiltinType() && right.isBuiltinType();
}

llvm::APSInt BinaryExpr::getConstantIntegerValue() const {
    // TODO: Add overflow checks.
    // TODO: Handle signedness for '>>' operator;

    auto lhs = getLHS().getConstantIntegerValue();
    auto rhs = getRHS().getConstantIntegerValue();

    switch (getOperator()) {
        case Token::Plus:
            return lhs + rhs;
        case Token::Minus:
            return lhs - rhs;
        case Token::Star:
            return lhs * rhs;
        case Token::Slash:
            return lhs / rhs;
        case Token::Modulo:
            return lhs % rhs;
        case Token::And:
            return lhs & rhs;
        case Token::Or:
            return lhs | rhs;
        case Token::Xor:
            return lhs ^ rhs;
        case Token::LeftShift:
            return lhs << static_cast<unsigned>(rhs.getZExtValue());
        case Token::RightShift:
            return lhs >> static_cast<unsigned>(rhs.getZExtValue());
        default:
            llvm_unreachable("invalid constant integer binary operator");
    }
}

LambdaExpr::LambdaExpr(std::vector<ParamDecl>&& params, Expr* body, Module* module, SourceLocation location) : Expr(ExprKind::LambdaExpr, location) {
    static uint64_t nameCounter = 0;
    FunctionProto proto("__lambda" + std::to_string(nameCounter++), std::move(params), Type(), false, false);
    this->functionDecl = new FunctionDecl(std::move(proto), std::vector<Type>(), AccessLevel::Private, *module, getLocation());
    std::vector<Stmt*> stmts;
    stmts.push_back(new ReturnStmt(body, body->getLocation()));
    this->functionDecl->setBody(std::move(stmts));
}

const Expr* TupleExpr::getElementByName(llvm::StringRef name) const {
    for (auto& element : getElements()) {
        if (element.getName() == name) {
            return element.getValue();
        }
    }
    return nullptr;
}
