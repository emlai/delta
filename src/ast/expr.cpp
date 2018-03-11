#include "expr.h"
#pragma warning(push, 0)
#include <llvm/Support/ErrorHandling.h>
#pragma warning(pop)
#include "decl.h"
#include "mangle.h"
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

bool Expr::isPointerOffset() const {
    auto* binaryExpr = llvm::dyn_cast<BinaryExpr>(this);
    return binaryExpr && binaryExpr->getLHS().getType().isPointerType() && binaryExpr->getRHS().getType().isInteger() &&
           (binaryExpr->getOperator() == Token::Plus || binaryExpr->getOperator() == Token::Minus);
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
            return llvm::cast<BinaryExpr>(this)->getLHS().isConstant() &&
                   llvm::cast<BinaryExpr>(this)->getRHS().isConstant();

        case ExprKind::CallExpr:
            return false;

        case ExprKind::SizeofExpr:
            return false; // TODO: sizeof should be a constant expression.

        case ExprKind::AddressofExpr:
        case ExprKind::MemberExpr:
        case ExprKind::SubscriptExpr:
        case ExprKind::UnwrapExpr:
        case ExprKind::LambdaExpr:
            return false;

        case ExprKind::IfExpr:
            return llvm::cast<IfExpr>(this)->getCondition()->isConstant() &&
                   llvm::cast<IfExpr>(this)->getThenExpr()->isConstant() &&
                   llvm::cast<IfExpr>(this)->getElseExpr()->isConstant();
    }

    llvm_unreachable("all cases handled");
}

int64_t Expr::getConstantIntegerValue() const {
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
            return static_cast<unsigned char>(llvm::cast<CharacterLiteralExpr>(this)->getValue());

        case ExprKind::IntLiteralExpr:
            return llvm::cast<IntLiteralExpr>(this)->getValue();

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

bool Expr::isLocalVariable() const {
    auto* varExpr = llvm::dyn_cast<VarExpr>(this);
    return varExpr && (varExpr->getDecl()->isParamDecl() || varExpr->getDecl()->isVarDecl());
}

bool Expr::isLvalue() const {
    switch (getKind()) {
        case ExprKind::VarExpr:
        case ExprKind::StringLiteralExpr:
        case ExprKind::CharacterLiteralExpr:
        case ExprKind::ArrayLiteralExpr:
        case ExprKind::TupleExpr:
        case ExprKind::MemberExpr:
        case ExprKind::SubscriptExpr:
        case ExprKind::IfExpr:
            return true;
        case ExprKind::IntLiteralExpr:
        case ExprKind::FloatLiteralExpr:
        case ExprKind::BoolLiteralExpr:
        case ExprKind::NullLiteralExpr:
        case ExprKind::UndefinedLiteralExpr:
        case ExprKind::SizeofExpr:
        case ExprKind::AddressofExpr:
        case ExprKind::UnwrapExpr:
        case ExprKind::BinaryExpr:
        case ExprKind::CallExpr:
        case ExprKind::LambdaExpr:
            return false;
        case ExprKind::UnaryExpr:
            return llvm::cast<UnaryExpr>(this)->getOperator() == Token::Star;
    }
    llvm_unreachable("all cases handled");
}

bool Expr::canDestructivelyMove() const {
    return isLocalVariable() || isRvalue();
}

void Expr::setMoved(bool moved) {
    if (getType().isImplicitlyCopyable()) return;

    if (auto* varExpr = llvm::dyn_cast<VarExpr>(this)) {
        switch (varExpr->getDecl()->getKind()) {
            case DeclKind::ParamDecl:
                llvm::cast<ParamDecl>(varExpr->getDecl())->setMoved(moved);
                break;
            case DeclKind::VarDecl:
                llvm::cast<VarDecl>(varExpr->getDecl())->setMoved(moved);
                break;
            default:
                ASSERT(!moved, "only local variables can be moved from");
        }
    }
}

std::unique_ptr<Expr> Expr::instantiate(const llvm::StringMap<Type>& genericArgs) const {
    std::unique_ptr<Expr> instantiation;

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

            instantiation = llvm::make_unique<VarExpr>(std::move(identifier), varExpr->getLocation());
            llvm::cast<VarExpr>(*instantiation).setDecl(varExpr->getDecl());
            break;
        }
        case ExprKind::StringLiteralExpr: {
            auto* stringLiteralExpr = llvm::cast<StringLiteralExpr>(this);
            instantiation = llvm::make_unique<StringLiteralExpr>(stringLiteralExpr->getValue(),
                                                                 stringLiteralExpr->getLocation());
            break;
        }
        case ExprKind::CharacterLiteralExpr: {
            auto* characterLiteralExpr = llvm::cast<CharacterLiteralExpr>(this);
            instantiation = llvm::make_unique<CharacterLiteralExpr>(characterLiteralExpr->getValue(),
                                                                    characterLiteralExpr->getLocation());
            break;
        }
        case ExprKind::IntLiteralExpr: {
            auto* intLiteralExpr = llvm::cast<IntLiteralExpr>(this);
            instantiation = llvm::make_unique<IntLiteralExpr>(intLiteralExpr->getValue(), intLiteralExpr->getLocation());
            break;
        }
        case ExprKind::FloatLiteralExpr: {
            auto* floatLiteralExpr = llvm::cast<FloatLiteralExpr>(this);
            instantiation = llvm::make_unique<FloatLiteralExpr>(floatLiteralExpr->getValue(),
                                                                floatLiteralExpr->getLocation());
            break;
        }
        case ExprKind::BoolLiteralExpr: {
            auto* boolLiteralExpr = llvm::cast<BoolLiteralExpr>(this);
            instantiation = llvm::make_unique<BoolLiteralExpr>(boolLiteralExpr->getValue(), boolLiteralExpr->getLocation());
            break;
        }
        case ExprKind::NullLiteralExpr: {
            auto* nullLiteralExpr = llvm::cast<NullLiteralExpr>(this);
            instantiation = llvm::make_unique<NullLiteralExpr>(nullLiteralExpr->getLocation());
            break;
        }
        case ExprKind::UndefinedLiteralExpr: {
            auto* undefinedLiteralExpr = llvm::cast<UndefinedLiteralExpr>(this);
            instantiation = llvm::make_unique<UndefinedLiteralExpr>(undefinedLiteralExpr->getLocation());
            break;
        }
        case ExprKind::ArrayLiteralExpr: {
            auto* arrayLiteralExpr = llvm::cast<ArrayLiteralExpr>(this);
            auto elements = ::instantiate(arrayLiteralExpr->getElements(), genericArgs);
            instantiation = llvm::make_unique<ArrayLiteralExpr>(std::move(elements), arrayLiteralExpr->getLocation());
            break;
        }
        case ExprKind::TupleExpr: {
            auto* tupleExpr = llvm::cast<TupleExpr>(this);
            auto elements = map(tupleExpr->getElements(), [&](const NamedValue& element) {
                return NamedValue(element.getName(), element.getValue()->instantiate(genericArgs));
            });
            instantiation = llvm::make_unique<TupleExpr>(std::move(elements), tupleExpr->getLocation());
            break;
        }
        case ExprKind::UnaryExpr: {
            auto* unaryExpr = llvm::cast<UnaryExpr>(this);
            auto operand = unaryExpr->getOperand().instantiate(genericArgs);
            instantiation = llvm::make_unique<UnaryExpr>(unaryExpr->getOperator(), std::move(operand),
                                                         unaryExpr->getLocation());
            break;
        }
        case ExprKind::BinaryExpr: {
            auto* binaryExpr = llvm::cast<BinaryExpr>(this);
            auto lhs = binaryExpr->getLHS().instantiate(genericArgs);
            auto rhs = binaryExpr->getRHS().instantiate(genericArgs);
            instantiation = llvm::make_unique<BinaryExpr>(binaryExpr->getOperator(), std::move(lhs), std::move(rhs),
                                                          binaryExpr->getLocation());
            break;
        }
        case ExprKind::CallExpr: {
            auto* callExpr = llvm::cast<CallExpr>(this);
            auto callee = callExpr->getCallee().instantiate(genericArgs);
            auto args = map(callExpr->getArgs(), [&](const NamedValue& arg) {
                return NamedValue(arg.getName(), arg.getValue()->instantiate(genericArgs));
            });
            auto callGenericArgs = map(callExpr->getGenericArgs(), [&](Type type) { return type.resolve(genericArgs); });
            instantiation = llvm::make_unique<CallExpr>(std::move(callee), std::move(args), std::move(callGenericArgs),
                                                        callExpr->getLocation());
            if (auto* callee = callExpr->getCalleeDecl()) {
                llvm::cast<CallExpr>(*instantiation).setCalleeDecl(callee);
            }
            Type receiverType = callExpr->getReceiverType().resolve(genericArgs);
            llvm::cast<CallExpr>(*instantiation).setReceiverType(receiverType);
            break;
        }
        case ExprKind::SizeofExpr: {
            auto* sizeofExpr = llvm::cast<SizeofExpr>(this);
            auto type = sizeofExpr->getType().resolve(genericArgs);
            instantiation = llvm::make_unique<SizeofExpr>(type, sizeofExpr->getLocation());
            break;
        }
        case ExprKind::AddressofExpr: {
            auto* addressofExpr = llvm::cast<AddressofExpr>(this);
            auto operand = addressofExpr->getOperand().instantiate(genericArgs);
            instantiation = llvm::make_unique<AddressofExpr>(std::move(operand), addressofExpr->getLocation());
            break;
        }
        case ExprKind::MemberExpr: {
            auto* memberExpr = llvm::cast<MemberExpr>(this);
            auto base = memberExpr->getBaseExpr()->instantiate(genericArgs);
            instantiation = llvm::make_unique<MemberExpr>(std::move(base), memberExpr->getMemberName(),
                                                          memberExpr->getLocation());
            break;
        }
        case ExprKind::SubscriptExpr: {
            auto* subscriptExpr = llvm::cast<SubscriptExpr>(this);
            auto base = subscriptExpr->getBaseExpr()->instantiate(genericArgs);
            auto index = subscriptExpr->getIndexExpr()->instantiate(genericArgs);
            instantiation = llvm::make_unique<SubscriptExpr>(std::move(base), std::move(index),
                                                             subscriptExpr->getLocation());
            break;
        }
        case ExprKind::UnwrapExpr: {
            auto* unwrapExpr = llvm::cast<UnwrapExpr>(this);
            auto operand = unwrapExpr->getOperand().instantiate(genericArgs);
            instantiation = llvm::make_unique<UnwrapExpr>(std::move(operand), unwrapExpr->getLocation());
            break;
        }
        case ExprKind::LambdaExpr: {
            auto* lambdaExpr = llvm::cast<LambdaExpr>(this);
            auto params = instantiateParams(lambdaExpr->getParams(), genericArgs);
            auto body = lambdaExpr->getBody()->instantiate(genericArgs);
            instantiation = llvm::make_unique<LambdaExpr>(std::move(params), std::move(body), lambdaExpr->getLocation());
            break;
        }
        case ExprKind::IfExpr: {
            auto* ifExpr = llvm::cast<IfExpr>(this);
            auto condition = ifExpr->getCondition()->instantiate(genericArgs);
            auto thenExpr = ifExpr->getThenExpr()->instantiate(genericArgs);
            auto elseExpr = ifExpr->getElseExpr()->instantiate(genericArgs);
            instantiation = llvm::make_unique<IfExpr>(std::move(condition), std::move(thenExpr), std::move(elseExpr),
                                                      ifExpr->getLocation());
        }
    }

    if (hasType()) {
        instantiation->setType(getType());
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
                subExprs.push_back(element.get());
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
        case ExprKind::SubscriptExpr:
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
    }

    return subExprs;
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
        return getArgs()[0].getValue()->getType().asImmutable() == receiverType.asImmutable();
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

int64_t UnaryExpr::getConstantIntegerValue() const {
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
    if (left.isEnumType() && left == right) return true;
    return left.isBuiltinType() && right.isBuiltinType();
}

int64_t BinaryExpr::getConstantIntegerValue() const {
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
            return lhs << rhs;
        case Token::RightShift:
            return lhs >> rhs;
        default:
            llvm_unreachable("invalid constant integer binary operator");
    }
}

std::unique_ptr<FunctionDecl> LambdaExpr::lower(Module& module) const {
    static uint64_t nameCounter = 0;

    FunctionProto proto("__lambda" + std::to_string(nameCounter++), std::vector<ParamDecl>(params), body->getType(),
                        false, false);
    auto functionDecl = llvm::make_unique<FunctionDecl>(std::move(proto), std::vector<Type>(), AccessLevel::Private,
                                                        module, getLocation());
    std::vector<std::unique_ptr<Stmt>> body;
    auto returnValue = getBody()->instantiate({});
    body.push_back(llvm::make_unique<ReturnStmt>(std::move(returnValue), getBody()->getLocation()));
    functionDecl->setBody(std::move(body));
    return functionDecl;
}
