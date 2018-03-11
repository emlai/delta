#include "typecheck.h"
#include <algorithm>
#include <limits>
#include <memory>
#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/APSInt.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/StringExtras.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/ErrorHandling.h>
#pragma warning(push, 0)
#include "../ast/decl.h"
#include "../ast/expr.h"
#include "../ast/mangle.h"
#include "../ast/module.h"
#include "../ast/type.h"
#include "../support/utility.h"

using namespace delta;

static void checkNotMoved(const Decl& decl, const VarExpr& expr) {
    const Movable* movable;

    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            movable = &llvm::cast<ParamDecl>(decl);
            break;
        case DeclKind::VarDecl:
            movable = &llvm::cast<VarDecl>(decl);
            break;
        default:
            return;
    }

    if (movable->isMoved()) {
        std::string typeInfo;

        if (expr.hasType()) {
            typeInfo = " of type '" + expr.getType().toString() + "'";
        }

        error(expr.getLocation(), "use of moved value '", expr.getIdentifier(), "'", typeInfo);
    }
}

void Typechecker::checkHasAccess(const Decl& decl, SourceLocation location, AccessLevel userAccessLevel) {
    // FIXME: Compare SourceFile objects instead of file path strings.
    if (decl.getAccessLevel() == AccessLevel::Private && strcmp(decl.getLocation().file, location.file) != 0) {
        warning(location, "'", decl.getName(), "' is private");
    } else if (userAccessLevel != AccessLevel::None && decl.getAccessLevel() < userAccessLevel) {
        warning(location, "using ", decl.getAccessLevel(), " type '", decl.getName(), "' in ", userAccessLevel,
                " declaration");
    }
}

Type Typechecker::typecheckVarExpr(VarExpr& expr, bool useIsWriteOnly) {
    Decl& decl = getCurrentModule()->findDecl(expr.getIdentifier(), expr.getLocation(), currentSourceFile, currentFieldDecls);
    checkHasAccess(decl, expr.getLocation(), AccessLevel::None);
    decl.setReferenced(true);
    expr.setDecl(&decl);

    switch (decl.getKind()) {
        case DeclKind::VarDecl:
            if (!useIsWriteOnly) checkNotMoved(decl, expr);
            return llvm::cast<VarDecl>(decl).getType();
        case DeclKind::ParamDecl:
            if (!useIsWriteOnly) checkNotMoved(decl, expr);
            return llvm::cast<ParamDecl>(decl).getType();
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
            return Type(llvm::cast<FunctionDecl>(decl).getFunctionType(), false, SourceLocation());
        case DeclKind::GenericParamDecl:
            llvm_unreachable("cannot refer to generic parameters yet");
        case DeclKind::InitDecl:
            llvm_unreachable("cannot refer to initializers yet");
        case DeclKind::DeinitDecl:
            llvm_unreachable("cannot refer to deinitializers yet");
        case DeclKind::FunctionTemplate:
            llvm_unreachable("cannot refer to generic functions yet");
        case DeclKind::TypeDecl:
            error(expr.getLocation(), "'", expr.getIdentifier(), "' is not a variable");
        case DeclKind::TypeTemplate:
            llvm_unreachable("cannot refer to generic types yet");
        case DeclKind::EnumDecl:
            error(expr.getLocation(), "'", expr.getIdentifier(), "' is not a variable");
        case DeclKind::FieldDecl:
            if (currentFunction->isInitDecl() || currentFunction->isDeinitDecl()) {
                return llvm::cast<FieldDecl>(decl).getType().asMutable();
            } else if (currentFunction->isMutating()) {
                return llvm::cast<FieldDecl>(decl).getType();
            } else {
                return llvm::cast<FieldDecl>(decl).getType().asImmutable();
            }
        case DeclKind::ImportDecl:
            llvm_unreachable("import statement validation not implemented yet");
    }
    llvm_unreachable("all cases handled");
}

Type typecheckStringLiteralExpr(StringLiteralExpr&) {
    return BasicType::get("StringRef", {});
}

Type typecheckCharacterLiteralExpr(CharacterLiteralExpr&) {
    return Type::getChar();
}

Type typecheckIntLiteralExpr(IntLiteralExpr& expr) {
    if (expr.getValue() >= std::numeric_limits<int32_t>::min() && expr.getValue() <= std::numeric_limits<int32_t>::max()) {
        return Type::getInt();
    } else if (expr.getValue() >= std::numeric_limits<int64_t>::min() &&
               expr.getValue() <= std::numeric_limits<int64_t>::max()) {
        return Type::getInt64();
    }
    error(expr.getLocation(), "integer literal is too large");
}

Type typecheckFloatLiteralExpr(FloatLiteralExpr&) {
    return Type::getFloat64();
}

Type typecheckBoolLiteralExpr(BoolLiteralExpr&) {
    return Type::getBool();
}

Type typecheckNullLiteralExpr(NullLiteralExpr&) {
    return Type::getNull();
}

Type typecheckUndefinedLiteralExpr(UndefinedLiteralExpr&) {
    return Type::getUndefined();
}

Type Typechecker::typecheckArrayLiteralExpr(ArrayLiteralExpr& array) {
    Type firstType = typecheckExpr(*array.getElements()[0]);
    for (auto& element : llvm::drop_begin(array.getElements(), 1)) {
        Type type = typecheckExpr(*element);
        if (type != firstType) {
            error(element->getLocation(), "mixed element types in array literal (expected '", firstType, "', found '",
                  type, "')");
        }
    }
    return ArrayType::get(firstType, int64_t(array.getElements().size()));
}

Type Typechecker::typecheckTupleExpr(TupleExpr& expr) {
    auto elements = map(expr.getElements(), [&](NamedValue& namedValue) {
        return TupleElement{ namedValue.getName(), typecheckExpr(*namedValue.getValue()) };
    });
    return TupleType::get(std::move(elements));
}

Type Typechecker::typecheckUnaryExpr(UnaryExpr& expr) {
    Type operandType = typecheckExpr(expr.getOperand());

    if (expr.getOperator() == Token::Not) {
        if (!operandType.isBool() && !operandType.isOptionalType()) {
            error(expr.getOperand().getLocation(), "invalid operand type '", operandType, "' to logical not");
        }
        return Type::getBool();
    }
    if (expr.getOperator() == Token::Star) { // Dereference operation
        if (operandType.isOptionalType() && operandType.getWrappedType().isPointerType()) {
            warning(expr.getOperand().getLocation(), "dereferencing value of optional type '", operandType,
                    "' which may be null; unwrap the value with a postfix '!' to silence this "
                    "warning");
            operandType = operandType.getWrappedType();
        } else if (!operandType.isPointerType()) {
            error(expr.getOperand().getLocation(), "cannot dereference non-pointer type '", operandType, "'");
        }
        return operandType.getPointee();
    }
    if (expr.getOperator() == Token::And) { // Address-of operation
        return PointerType::get(operandType);
    }
    if (expr.getOperator() == Token::Increment) {
        if (!operandType.isMutable()) {
            error(expr.getLocation(), "cannot increment immutable value");
        }
        // TODO: check that operand supports increment operation.
        return Type::getVoid();
    }
    if (expr.getOperator() == Token::Decrement) {
        if (!operandType.isMutable()) {
            error(expr.getLocation(), "cannot decrement immutable value");
        }
        // TODO: check that operand supports decrement operation.
        return Type::getVoid();
    }
    return operandType;
}

static void invalidOperandsToBinaryExpr(const BinaryExpr& expr, Token::Kind op) {
    std::string hint;

    if ((expr.getRHS().isNullLiteralExpr() || expr.getLHS().isNullLiteralExpr()) &&
        (op == Token::Equal || op == Token::NotEqual)) {
        hint += " (non-optional type '";
        if (expr.getRHS().isNullLiteralExpr()) {
            hint += expr.getLHS().getType().toString();
        } else {
            hint += expr.getRHS().getType().toString();
        }
        hint += "' cannot be null)";
    } else {
        hint = "";
    }

    error(expr.getLocation(), "invalid operands '", expr.getLHS().getType(), "' and '", expr.getRHS().getType(),
          "' to '", toString(op), "'", hint);
}

static bool allowAssignmentOfUndefined(const Expr& lhs, const FunctionDecl* currentFunction) {
    if (auto* initDecl = llvm::dyn_cast<InitDecl>(currentFunction)) {
        switch (lhs.getKind()) {
            case ExprKind::VarExpr: {
                auto* fieldDecl = llvm::dyn_cast<FieldDecl>(llvm::cast<VarExpr>(lhs).getDecl());
                return fieldDecl && fieldDecl->getParent() == initDecl->getTypeDecl();
            }
            case ExprKind::MemberExpr: {
                auto* varExpr = llvm::dyn_cast<VarExpr>(llvm::cast<MemberExpr>(lhs).getBaseExpr());
                return varExpr && varExpr->getIdentifier() == "this";
            }
            default:
                return false;
        }
    }
    return false;
}

Type Typechecker::typecheckBinaryExpr(BinaryExpr& expr) {
    if (expr.getOperator() == Token::Assignment) {
        typecheckAssignment(expr.getLHS(), &expr.getRHS(), Type(), expr.getLocation());
        return Type::getVoid();
    }

    if (isCompoundAssignmentOperator(expr.getOperator())) {
        Type rightType = typecheckBinaryExpr(expr, withoutCompoundEqSuffix(expr.getOperator()));
        typecheckAssignment(expr.getLHS(), nullptr, rightType, expr.getLocation());
        return Type::getVoid();
    }

    return typecheckBinaryExpr(expr, expr.getOperator());
}

Type Typechecker::typecheckBinaryExpr(BinaryExpr& expr, Token::Kind op) {
    ASSERT(!isAssignmentOperator(op));
    Type leftType = typecheckExpr(expr.getLHS());
    Type rightType = typecheckExpr(expr.getRHS());

    if (!isBuiltinOp(op, leftType, rightType)) {
        return typecheckCallExpr(expr);
    }

    if (op == Token::AndAnd || op == Token::OrOr) {
        if (leftType.isBool() && rightType.isBool()) {
            return Type::getBool();
        }
        invalidOperandsToBinaryExpr(expr, op);
    }

    if (op == Token::PointerEqual || op == Token::PointerNotEqual) {
        if (!leftType.removeOptional().isPointerType() || !rightType.removeOptional().isPointerType()) {
            error(expr.getLocation(), "both operands to pointer comparison operator must have pointer type");
        }
        if (leftType.removeOptional().removePointer().asImmutable() !=
            rightType.removeOptional().removePointer().asImmutable()) {
            warning(expr.getLocation(), "pointers to different types are not allowed to be equal");
        }
    }

    if (op == Token::Plus || op == Token::Minus) {
        if (leftType.isPointerType() && rightType.isInteger()) {
            return leftType;
        } else if (leftType.isInteger() && rightType.isPointerType()) {
            invalidOperandsToBinaryExpr(expr, op);
        }
    }

    if (isBitwiseOperator(op) && (leftType.isFloatingPoint() || rightType.isFloatingPoint())) {
        invalidOperandsToBinaryExpr(expr, op);
    }

    Type convertedType;

    if (isImplicitlyConvertible(&expr.getRHS(), rightType, leftType, &convertedType)) {
        expr.getRHS().setType(convertedType ? convertedType : rightType);
    } else if (isImplicitlyConvertible(&expr.getLHS(), leftType, rightType, &convertedType)) {
        expr.getLHS().setType(convertedType ? convertedType : leftType);
    } else if (!leftType.removeOptional().isPointerType() || !rightType.removeOptional().isPointerType()) {
        invalidOperandsToBinaryExpr(expr, op);
    }

    return isComparisonOperator(op) ? Type::getBool() : expr.getLHS().getType();
}

void Typechecker::typecheckAssignment(Expr& lhs, Expr* rhs, Type rightType, SourceLocation location) {
    typecheckExpr(lhs, true);
    Type lhsType = lhs.getAssignableType();
    Type rhsType = rightType ? rightType : (rhs->isUndefinedLiteralExpr() ? Type() : typecheckExpr(*rhs));

    if (rhs && rhs->isUndefinedLiteralExpr() && !allowAssignmentOfUndefined(lhs, currentFunction)) {
        error(location, "'undefined' is only allowed as an initial value");
    }

    if (!rhs || !rhs->isUndefinedLiteralExpr()) {
        Type convertedType;

        if (isImplicitlyConvertible(rhs, rhsType, lhsType, &convertedType)) {
            if (rhs) rhs->setType(convertedType ? convertedType : rhsType);
        } else {
            error(location, "cannot assign '", rhsType, "' to variable of type '", lhsType, "'");
        }
    }

    if (!lhsType.isMutable()) {
        switch (lhs.getKind()) {
            case ExprKind::VarExpr:
                error(location, "cannot assign to immutable variable '", llvm::cast<VarExpr>(lhs).getIdentifier(), "'");
            case ExprKind::MemberExpr:
                error(location, "cannot assign to immutable variable '", llvm::cast<MemberExpr>(lhs).getMemberName(),
                      "'");
            default:
                error(location, "cannot assign to immutable expression");
        }
    }

    if (rhsType && !rhsType.isImplicitlyCopyable() && !lhsType.removeOptional().isPointerType()) {
        if (rhs) {
            checkCanMove(*rhs);
            rhs->setMoved(true);
        }
        lhs.setMoved(false);
    }

    markFieldAsInitialized(lhs);
}

template<int bitWidth, bool isSigned>
bool checkRange(const Expr& expr, int64_t value, Type type, Type* convertedType) {
    if ((isSigned && !llvm::APSInt::get(value).isSignedIntN(bitWidth)) ||
        (!isSigned && !llvm::APSInt::get(value).isIntN(bitWidth))) {
        error(expr.getLocation(), value, " is out of range for type '", type, "'");
    }

    if (convertedType) *convertedType = type;
    return true;
}

bool Typechecker::isInterface(Type type) {
    if (!type.isBasicType() || type.isBuiltinType()) return false;
    auto* typeDecl = getTypeDecl(llvm::cast<BasicType>(*type));
    return typeDecl && typeDecl->isInterface();
}

static bool hasField(TypeDecl& type, const FieldDecl& field) {
    return llvm::any_of(type.getFields(), [&](const FieldDecl& ownField) {
        return ownField.getName() == field.getName() && ownField.getType() == field.getType();
    });
}

bool Typechecker::hasMethod(TypeDecl& type, FunctionDecl& functionDecl) const {
    auto decls = getCurrentModule()->findDecls(getQualifiedFunctionName(type.getType(), functionDecl.getName(), {}),
                                               currentSourceFile, currentFunction);
    for (Decl* decl : decls) {
        if (!decl->isFunctionDecl()) continue;
        if (!llvm::cast<FunctionDecl>(decl)->getTypeDecl()) continue;
        if (llvm::cast<FunctionDecl>(decl)->getTypeDecl()->getName() != type.getName()) continue;
        if (!llvm::cast<FunctionDecl>(decl)->signatureMatches(functionDecl, /* matchReceiver: */ false)) continue;
        return true;
    }
    return false;
}

bool Typechecker::providesInterfaceRequirements(TypeDecl& type, TypeDecl& interface, std::string* errorReason) const {
    auto thisTypeResolvedInterface = llvm::cast<TypeDecl>(interface.instantiate({ { "This", type.getType() } }, {}));

    for (auto& fieldRequirement : thisTypeResolvedInterface->getFields()) {
        if (!hasField(type, fieldRequirement)) {
            if (errorReason) {
                *errorReason = ("doesn't have field '" + fieldRequirement.getName() + "'").str();
            }
            return false;
        }
    }

    for (auto& requiredMethod : thisTypeResolvedInterface->getMethods()) {
        if (auto* functionDecl = llvm::dyn_cast<FunctionDecl>(requiredMethod.get())) {
            if (functionDecl->hasBody()) continue;

            if (!hasMethod(type, *functionDecl)) {
                if (errorReason) {
                    *errorReason = ("doesn't have member function '" + functionDecl->getName() + "'").str();
                }
                return false;
            }
        } else {
            error(requiredMethod->getLocation(), "non-function interface member requirements are not supported yet");
        }
    }

    return true;
}

bool Typechecker::isImplicitlyConvertible(const Expr* expr, Type source, Type target, Type* convertedType) const {
    bool isNullLiteral = expr && expr->isNullLiteralExpr();

    if (target.isOptionalType() && !isNullLiteral) {
        if (isImplicitlyConvertible(expr, source, target.getWrappedType(), convertedType)) {
            return true;
        }
    }

    switch (source.getKind()) {
        case TypeKind::BasicType:
            if (target.isBasicType() && source.getName() == target.getName() &&
                source.getGenericArgs() == target.getGenericArgs()) {
                return true;
            }
            break;
        case TypeKind::ArrayType:
            if (target.isArrayType() && (source.getArraySize() == target.getArraySize() || target.isArrayWithRuntimeSize()) &&
                isImplicitlyConvertible(nullptr, source.getElementType(), target.getElementType(), nullptr)) {
                return true;
            }
            break;
        case TypeKind::TupleType:
            if (target.isTupleType() && source.getTupleElements() == target.getTupleElements()) {
                return true;
            }
            break;
        case TypeKind::FunctionType:
            if (target.isFunctionType() && source.getReturnType() == target.getReturnType() &&
                source.getParamTypes() == target.getParamTypes()) {
                return true;
            }
            break;
        case TypeKind::PointerType:
            if (target.isPointerType() && (source.getPointee().isMutable() || !target.getPointee().isMutable()) &&
                (isImplicitlyConvertible(nullptr, source.getPointee(), target.getPointee(), nullptr) ||
                 target.getPointee().isVoid())) {
                return true;
            }
            break;
        case TypeKind::OptionalType:
            if (target.isOptionalType() && (source.getWrappedType().isMutable() || !target.getWrappedType().isMutable()) &&
                isImplicitlyConvertible(nullptr, source.getWrappedType(), target.getWrappedType(), nullptr)) {
                return true;
            }
            break;
    }

    if (expr) {
        // Autocast integer constants to parameter type if within range, error out if not within range.
        if ((expr->getType().isInteger() || expr->getType().isChar()) && expr->isConstant() && target.isBasicType()) {
            auto value = expr->getConstantIntegerValue();

            if (target.isInteger()) {
                if (target.isInt()) return checkRange<32, true>(*expr, value, target, convertedType);
                if (target.isUInt()) return checkRange<32, false>(*expr, value, target, convertedType);
                if (target.isInt8()) return checkRange<8, true>(*expr, value, target, convertedType);
                if (target.isInt16()) return checkRange<16, true>(*expr, value, target, convertedType);
                if (target.isInt32()) return checkRange<32, true>(*expr, value, target, convertedType);
                if (target.isInt64()) return checkRange<64, true>(*expr, value, target, convertedType);
                if (target.isUInt8()) return checkRange<8, false>(*expr, value, target, convertedType);
                if (target.isUInt16()) return checkRange<16, false>(*expr, value, target, convertedType);
                if (target.isUInt32()) return checkRange<32, false>(*expr, value, target, convertedType);
                if (target.isUInt64()) return checkRange<64, false>(*expr, value, target, convertedType);
            }

            if (target.isFloatingPoint() && expr->isIntLiteralExpr()) {
                // TODO: Check that the integer value is losslessly convertible to the target type?
                if (convertedType) *convertedType = target;
                return true;
            }
        } else if (expr->getType().isFloatingPoint() && expr->isConstant() && target.isFloatingPoint()) {
            // TODO: Check that the floating-point value is losslessly convertible to the target type?
            if (convertedType) *convertedType = target;
            return true;
        } else if (expr->isNullLiteralExpr() && target.isOptionalType()) {
            if (convertedType) *convertedType = target;
            return true;
        } else if (expr->isUndefinedLiteralExpr()) {
            if (convertedType) *convertedType = target;
            return true;
        } else if (expr->isStringLiteralExpr() && target.removeOptional().isPointerType() &&
                   target.removeOptional().getPointee().isChar() && !target.removeOptional().getPointee().isMutable()) {
            // Special case: allow passing string literals as C-strings (const char*).
            if (convertedType) *convertedType = target;
            return true;
        } else if (expr->isArrayLiteralExpr() && target.isArrayType()) {
            bool isConvertible = llvm::all_of(llvm::cast<ArrayLiteralExpr>(expr)->getElements(),
                                              [&](const std::unique_ptr<Expr>& element) {
                                                  return isImplicitlyConvertible(element.get(), source.getElementType(),
                                                                                 target.getElementType(), nullptr);
                                              });

            if (isConvertible) {
                for (auto& element : llvm::cast<ArrayLiteralExpr>(expr)->getElements()) {
                    element->setType(target.getElementType());
                }
                if (convertedType) *convertedType = target;
                return true;
            }
        }
    }

    if (source.isBasicType() && target.removeOptional().isPointerType() &&
        isImplicitlyConvertible(expr, source, target.removeOptional().getPointee(), nullptr)) {
        if (convertedType) *convertedType = source;
        return true;
    } else if (source.isArrayType() && target.removeOptional().isPointerType() &&
               target.removeOptional().getPointee().isArrayType() &&
               isImplicitlyConvertible(nullptr, source.getElementType(),
                                       target.removeOptional().getPointee().getElementType(), nullptr)) {
        if (convertedType) *convertedType = source;
        return true;
    } else if (source.isArrayType() && target.removeOptional().isPointerType() &&
               isImplicitlyConvertible(nullptr, source.getElementType(), target.removeOptional().getPointee(), nullptr)) {
        if (convertedType) *convertedType = source;
        return true;
    } else if (source.isPointerType() && source.getPointee().isArrayType() && target.removeOptional().isPointerType() &&
               isImplicitlyConvertible(nullptr, source.getPointee().getElementType(),
                                       target.removeOptional().getPointee(), nullptr)) {
        if (convertedType) *convertedType = source;
        return true;
    } else if (source.isPointerType() && target.removeOptional().isPointerType() &&
               target.removeOptional().getPointee().isArrayWithUnknownSize()) {
        return true;
    } else if (source.isTupleType() && target.removePointer().isTupleType()) {
        target = target.removePointer();
        auto* tupleExpr = llvm::dyn_cast_or_null<TupleExpr>(expr);
        auto elements = source.getTupleElements();
        std::vector<TupleElement> convertedElements;

        for (size_t i = 0; i < elements.size(); ++i) {
            if (elements[i].name != target.getTupleElements()[i].name) return false;
            Type convertedType;
            auto* elementValue = tupleExpr ? tupleExpr->getElements()[i].getValue() : nullptr;

            if (!isImplicitlyConvertible(elementValue, elements[i].type, target.getTupleElements()[i].type, &convertedType)) {
                return false;
            }

            if (convertedType) {
                convertedElements.push_back({ elements[i].name, convertedType });
            } else {
                convertedElements.push_back(elements[i]);
            }
        }

        if (convertedType) *convertedType = TupleType::get(std::move(convertedElements));
        return true;
    }

    return false;
}

static bool containsGenericParam(Type type, llvm::StringRef genericParam) {
    switch (type.getKind()) {
        case TypeKind::BasicType:
            for (Type genericArg : type.getGenericArgs()) {
                if (containsGenericParam(genericArg, genericParam)) {
                    return true;
                }
            }
            return type.getName() == genericParam;

        case TypeKind::ArrayType:
            return containsGenericParam(type.getElementType(), genericParam);

        case TypeKind::TupleType:
            llvm_unreachable("unimplemented");

        case TypeKind::FunctionType:
            for (Type paramType : type.getParamTypes()) {
                if (containsGenericParam(paramType, genericParam)) {
                    return true;
                }
            }
            return containsGenericParam(type.getReturnType(), genericParam);

        case TypeKind::PointerType:
            return containsGenericParam(type.getPointee(), genericParam);

        case TypeKind::OptionalType:
            return containsGenericParam(type.getWrappedType(), genericParam);
    }

    llvm_unreachable("all cases handled");
}

static Type findGenericArg(Type argType, Type paramType, llvm::StringRef genericParam) {
    if (paramType.isBasicType() && paramType.getName() == genericParam) {
        return argType;
    }

    switch (argType.getKind()) {
        case TypeKind::BasicType:
            if (!argType.getGenericArgs().empty() && paramType.isBasicType() && paramType.getName() == argType.getName()) {
                ASSERT(argType.getGenericArgs().size() == paramType.getGenericArgs().size());
                for (auto t : llvm::zip_first(argType.getGenericArgs(), paramType.getGenericArgs())) {
                    if (Type type = findGenericArg(std::get<0>(t), std::get<1>(t), genericParam)) {
                        return type;
                    }
                }
            }
            break;

        case TypeKind::ArrayType:
            if (paramType.isArrayType()) {
                return findGenericArg(argType.getElementType(), paramType.getElementType(), genericParam);
            }
            break;

        case TypeKind::TupleType:
            llvm_unreachable("unimplemented");

        case TypeKind::FunctionType:
            if (paramType.isFunctionType()) {
                for (auto t : llvm::zip_first(argType.getParamTypes(), paramType.getParamTypes())) {
                    if (Type type = findGenericArg(std::get<0>(t), std::get<1>(t), genericParam)) {
                        return type;
                    }
                }
                return findGenericArg(argType.getReturnType(), paramType.getReturnType(), genericParam);
            }
            break;

        case TypeKind::PointerType:
            if (paramType.isPointerType()) {
                return findGenericArg(argType.getPointee(), paramType.getPointee(), genericParam);
            }
            break;

        case TypeKind::OptionalType:
            if (paramType.isOptionalType()) {
                return findGenericArg(argType.getWrappedType(), paramType.getWrappedType(), genericParam);
            }
            break;
    }

    if (paramType.removeOptional().isPointerType()) {
        return findGenericArg(argType, paramType.removeOptional().getPointee(), genericParam);
    }

    return Type();
}

std::vector<Type> Typechecker::inferGenericArgs(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call,
                                                llvm::ArrayRef<ParamDecl> params, bool returnOnError) {
    if (call.getArgs().size() != params.size()) return {};

    std::vector<Type> inferredGenericArgs;

    for (auto& genericParam : genericParams) {
        Type genericArg;
        Expr* genericArgValue;

        for (auto tuple : llvm::zip_first(params, call.getArgs())) {
            Type paramType = std::get<0>(tuple).getType();

            if (containsGenericParam(paramType, genericParam.getName())) {
                // FIXME: The args will also be typechecked by validateArgs()
                // after this function. Get rid of this duplicated typechecking.
                auto* argValue = std::get<1>(tuple).getValue();
                Type argType = typecheckExpr(*argValue);
                Type maybeGenericArg = findGenericArg(argType, paramType, genericParam.getName());
                if (!maybeGenericArg) continue;
                Type convertedType;

                if (!genericArg) {
                    genericArg = maybeGenericArg;
                    genericArgValue = argValue;
                } else {
                    Type paramTypeWithGenericArg = paramType.resolve({ { genericParam.getName(), genericArg } });
                    Type paramTypeWithMaybeGenericArg = paramType.resolve({ { genericParam.getName(), maybeGenericArg } });

                    if (isImplicitlyConvertible(argValue, argType, paramTypeWithGenericArg, &convertedType)) {
                        argValue->setType(convertedType ? convertedType : argType);
                        continue;
                    } else if (isImplicitlyConvertible(genericArgValue, genericArgValue->getType(),
                                                       paramTypeWithMaybeGenericArg, &convertedType)) {
                        argValue->setType(convertedType ? convertedType : genericArgValue->getType());
                        genericArg = maybeGenericArg;
                        genericArgValue = argValue;
                    } else {
                        error(call.getLocation(), "couldn't infer generic parameter '", genericParam.getName(),
                              "' of '", call.getFunctionName(), "' because of conflicting argument types '", genericArg,
                              "' and '", maybeGenericArg, "'");
                    }
                }
            }
        }

        if (genericArg) {
            inferredGenericArgs.push_back(genericArg.asImmutable());
        } else {
            return {};
        }
    }

    ASSERT(genericParams.size() == inferredGenericArgs.size());

    for (auto t : llvm::zip(genericParams, inferredGenericArgs)) {
        auto& genericParam = std::get<0>(t);
        auto& genericArg = std::get<1>(t);

        if (!genericParam.getConstraints().empty()) {
            ASSERT(genericParam.getConstraints().size() == 1, "cannot have multiple generic constraints yet");

            auto* interface = getTypeDecl(*llvm::cast<BasicType>(genericParam.getConstraints()[0].getBase()));
            std::string errorReason;

            if (genericArg.isBasicType()) {
                auto* typeDecl = getTypeDecl(*llvm::cast<BasicType>(genericArg.getBase()));

                if (!typeDecl || !typeDecl->hasInterface(*interface)) {
                    if (returnOnError) {
                        return {};
                    } else {
                        error(call.getLocation(), "type '", genericArg, "' doesn't implement interface '",
                              interface->getName(), "'");
                    }
                }
            }
        }
    }

    return inferredGenericArgs;
}

void validateGenericArgCount(size_t genericParamCount, llvm::ArrayRef<Type> genericArgs, llvm::StringRef name,
                             SourceLocation location) {
    if (genericArgs.size() < genericParamCount) {
        error(location, "too few generic arguments to '", name, "', expected ", genericParamCount);
    } else if (genericArgs.size() > genericParamCount) {
        error(location, "too many generic arguments to '", name, "', expected ", genericParamCount);
    }
}

void validateArgCount(size_t paramCount, size_t argCount, bool isVariadic, llvm::StringRef name, SourceLocation location) {
    if (argCount < paramCount) {
        error(location, "too few arguments to '", name, "', expected ", isVariadic ? "at least " : "", paramCount);
    } else if (!isVariadic && argCount > paramCount) {
        error(location, "too many arguments to '", name, "', expected ", paramCount);
    }
}

llvm::StringMap<Type> Typechecker::getGenericArgsForCall(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call,
                                                         llvm::ArrayRef<ParamDecl> params, bool returnOnError) {
    ASSERT(!genericParams.empty());

    if (call.getGenericArgs().empty()) {
        if (call.getArgs().empty()) {
            error(call.getLocation(), "can't infer generic parameters without function arguments");
        }

        auto inferredGenericArgs = inferGenericArgs(genericParams, call, params, returnOnError);
        if (inferredGenericArgs.empty()) return {};
        call.setGenericArgs(std::move(inferredGenericArgs));
        ASSERT(call.getGenericArgs().size() == genericParams.size());
    }

    llvm::StringMap<Type> genericArgs;
    auto genericArg = call.getGenericArgs().begin();

    for (const GenericParamDecl& genericParam : genericParams) {
        genericArgs.try_emplace(genericParam.getName(), *genericArg++);
    }

    return genericArgs;
}

Type Typechecker::typecheckBuiltinConversion(CallExpr& expr) {
    if (expr.getArgs().size() != 1) {
        error(expr.getLocation(), "expected single argument to converting initializer");
    }
    if (!expr.getGenericArgs().empty()) {
        error(expr.getLocation(), "expected no generic arguments to converting initializer");
    }
    if (!expr.getArgs().front().getName().empty()) {
        error(expr.getLocation(), "expected unnamed argument to converting initializer");
    }
    typecheckExpr(*expr.getArgs().front().getValue());
    expr.setType(BasicType::get(expr.getFunctionName(), {}));
    return expr.getType();
}

static std::vector<Note> getCandidateNotes(llvm::ArrayRef<Decl*> candidates) {
    return map(candidates, [](Decl* candidate) { return Note(candidate->getLocation(), "candidate function:"); });
}

Decl* Typechecker::resolveOverload(llvm::ArrayRef<Decl*> decls, CallExpr& expr, llvm::StringRef callee,
                                   bool returnNullOnError) {
    llvm::SmallVector<Decl*, 1> matches;
    std::vector<Decl*> initDecls;
    bool isInitCall = false;

    for (Decl* decl : decls) {
        switch (decl->getKind()) {
            case DeclKind::FunctionTemplate: {
                auto* functionTemplate = llvm::cast<FunctionTemplate>(decl);
                auto genericParams = functionTemplate->getGenericParams();

                if (!expr.getGenericArgs().empty() && expr.getGenericArgs().size() != genericParams.size()) {
                    if (decls.size() == 1) {
                        validateGenericArgCount(genericParams.size(), expr.getGenericArgs(), expr.getFunctionName(),
                                                expr.getLocation());
                    }
                    continue;
                }

                auto params = functionTemplate->getFunctionDecl()->getParams();
                auto genericArgs = getGenericArgsForCall(genericParams, expr, params, decls.size() != 1);
                if (genericArgs.empty()) continue; // Couldn't infer generic arguments.

                auto* functionDecl = functionTemplate->instantiate(genericArgs);
                declsToTypecheck.emplace_back(functionDecl);

                if (decls.size() == 1 && !returnNullOnError) {
                    validateArgs(expr, *functionDecl, callee, expr.getCallee().getLocation());
                    return functionDecl;
                }
                if (argumentsMatch(expr, functionDecl)) {
                    matches.push_back(functionDecl);
                }
                break;
            }
            case DeclKind::FunctionDecl:
            case DeclKind::MethodDecl:
            case DeclKind::InitDecl: {
                auto& functionDecl = llvm::cast<FunctionDecl>(*decl);

                if (decls.size() == 1 && !returnNullOnError) {
                    validateGenericArgCount(0, expr.getGenericArgs(), expr.getFunctionName(), expr.getLocation());
                    validateArgs(expr, functionDecl, callee, expr.getCallee().getLocation());
                    return &functionDecl;
                }
                if (argumentsMatch(expr, &functionDecl)) {
                    matches.push_back(&functionDecl);
                }
                break;
            }
            case DeclKind::TypeDecl: {
                isInitCall = true;
                validateGenericArgCount(0, expr.getGenericArgs(), expr.getFunctionName(), expr.getLocation());
                auto qualifiedName = getQualifiedFunctionName(llvm::cast<TypeDecl>(decl)->getType(), "init", {});
                initDecls = getCurrentModule()->findDecls(qualifiedName, currentSourceFile, currentFunction);
                ASSERT(decls.size() == 1);
                decls = initDecls;

                for (Decl* decl : initDecls) {
                    InitDecl& initDecl = llvm::cast<InitDecl>(*decl);

                    if (initDecls.size() == 1 && !returnNullOnError) {
                        validateArgs(expr, initDecl, callee, expr.getCallee().getLocation());
                        return &initDecl;
                    }
                    if (argumentsMatch(expr, &initDecl)) {
                        matches.push_back(&initDecl);
                    }
                }
                break;
            }
            case DeclKind::TypeTemplate: {
                auto* typeTemplate = llvm::cast<TypeTemplate>(decl);
                isInitCall = true;

                std::vector<InitDecl*> instantiatedInitDecls;

                for (auto& method : typeTemplate->getTypeDecl()->getMethods()) {
                    auto* initDecl = llvm::dyn_cast<InitDecl>(method.get());
                    if (!initDecl) continue;
                    initDecls.push_back(initDecl);
                }

                ASSERT(decls.size() == 1);
                decls = initDecls;

                for (auto* decl : initDecls) {
                    auto* initDecl = llvm::cast<InitDecl>(decl);
                    auto genericArgs = getGenericArgsForCall(typeTemplate->getGenericParams(), expr,
                                                             initDecl->getParams(), initDecls.size() != 1);
                    if (genericArgs.empty()) continue; // Couldn't infer generic arguments.

                    TypeDecl* typeDecl = nullptr;

                    auto decls = getCurrentModule()->findDecls(getQualifiedTypeName(typeTemplate->getTypeDecl()->getName(),
                                                                                    expr.getGenericArgs()),
                                                               currentSourceFile, currentFunction);
                    if (decls.empty()) {
                        typeDecl = typeTemplate->instantiate(genericArgs);
                        getCurrentModule()->addToSymbolTable(*typeDecl);
                        declsToTypecheck.push_back(typeDecl);
                    } else {
                        typeDecl = llvm::cast<TypeDecl>(decls[0]);
                    }

                    for (auto& method : typeDecl->getMethods()) {
                        auto* initDecl = llvm::dyn_cast<InitDecl>(method.get());
                        if (!initDecl) continue;
                        instantiatedInitDecls.push_back(initDecl);
                    }
                }

                for (auto* instantiatedInitDecl : instantiatedInitDecls) {
                    if (initDecls.size() == 1 && !returnNullOnError) {
                        validateArgs(expr, *instantiatedInitDecl, callee, expr.getCallee().getLocation());
                        return instantiatedInitDecl;
                    }
                    if (argumentsMatch(expr, instantiatedInitDecl)) {
                        matches.push_back(instantiatedInitDecl);
                    }
                }
                break;
            }
            case DeclKind::VarDecl: {
                auto* varDecl = llvm::cast<VarDecl>(decl);

                if (auto* functionType = llvm::dyn_cast<FunctionType>(varDecl->getType().getBase())) {
                    auto paramDecls = functionType->getParamDecls(varDecl->getLocation());

                    if (decls.size() == 1 && !returnNullOnError) {
                        validateArgs(expr, false, paramDecls, false, callee, expr.getCallee().getLocation());
                        return varDecl;
                    }
                    if (argumentsMatch(expr, nullptr, paramDecls)) {
                        matches.push_back(varDecl);
                    }
                }
                break;
            }
            case DeclKind::ParamDecl: {
                auto* paramDecl = llvm::cast<ParamDecl>(decl);

                if (auto* functionType = llvm::dyn_cast<FunctionType>(paramDecl->getType().getBase())) {
                    auto paramDecls = functionType->getParamDecls(paramDecl->getLocation());

                    if (decls.size() == 1 && !returnNullOnError) {
                        validateArgs(expr, false, paramDecls, false, callee, expr.getCallee().getLocation());
                        return paramDecl;
                    }
                    if (argumentsMatch(expr, nullptr, paramDecls)) {
                        matches.push_back(paramDecl);
                    }
                }
                break;
            }
            case DeclKind::FieldDecl: {
                auto* fieldDecl = llvm::cast<FieldDecl>(decl);

                if (auto* functionType = llvm::dyn_cast<FunctionType>(fieldDecl->getType().getBase())) {
                    auto paramDecls = functionType->getParamDecls(fieldDecl->getLocation());

                    if (decls.size() == 1 && !returnNullOnError) {
                        validateArgs(expr, false, paramDecls, false, callee, expr.getCallee().getLocation());
                        return fieldDecl;
                    }
                    if (argumentsMatch(expr, nullptr, paramDecls)) {
                        matches.push_back(fieldDecl);
                    }
                }
                break;
            }
            case DeclKind::DeinitDecl:
                matches.push_back(decl);
                break;

            default:
                continue;
        }
    }

    switch (matches.size()) {
        case 1:
            validateArgs(expr, *matches.front());
            return matches.front();

        case 0: {
            if (returnNullOnError) {
                return nullptr;
            }

            if (decls.size() == 0) {
                error(expr.getCallee().getLocation(), "unknown identifier '", callee, "'");
            }

            bool atLeastOneFunction = llvm::any_of(decls, [](Decl* decl) {
                return decl->isFunctionDecl() || decl->isFunctionTemplate() || decl->isTypeDecl() || decl->isTypeTemplate();
            });

            if (atLeastOneFunction) {
                auto argTypeStrings = map(expr.getArgs(), [](const NamedValue& arg) {
                    auto type = arg.getValue()->getType();
                    return type ? type.toString() : "???";
                });

                errorWithNotes(expr.getCallee().getLocation(), getCandidateNotes(decls), "no matching ",
                               isInitCall ? "initializer for '" : "function for call to '", callee,
                               "' with argument list of type '(", llvm::join(argTypeStrings, ", "), ")'");
            } else {
                error(expr.getCallee().getLocation(), "'", callee, "' is not a function");
            }
        }
        default:
            if (returnNullOnError) {
                return nullptr;
            }

            if (expr.getReceiver() && expr.getReceiverType().removePointer().isMutable()) {
                llvm::SmallVector<Decl*, 1> mutatingMatches;

                for (auto* match : matches) {
                    if (!match->isMethodDecl() || llvm::cast<MethodDecl>(match)->isMutating()) {
                        mutatingMatches.push_back(match);
                    }
                }

                if (mutatingMatches.size() == 1) {
                    validateArgs(expr, *mutatingMatches.front());
                    return mutatingMatches.front();
                }
            }

            bool allMatchesAreFromC = llvm::all_of(matches, [](Decl* match) {
                return match->getModule() && match->getModule()->getName().endswith_lower(".h");
            });

            if (allMatchesAreFromC) {
                validateArgs(expr, *matches.front());
                return matches.front();
            }

            for (auto* match : matches) {
                if (match->getModule() && match->getModule()->getName() == "std") {
                    validateArgs(expr, *match);
                    return match;
                }
            }

            errorWithNotes(expr.getCallee().getLocation(), getCandidateNotes(decls), "ambiguous reference to '", callee,
                           isInitCall ? ".init'" : "'");
    }
}

std::vector<Decl*> Typechecker::findCalleeCandidates(const CallExpr& expr, llvm::StringRef callee) {
    TypeDecl* receiverTypeDecl;

    if (expr.getReceiverType() && expr.getReceiverType().removePointer().isBasicType()) {
        receiverTypeDecl = getTypeDecl(*llvm::cast<BasicType>(expr.getReceiverType().removePointer().getBase()));
    } else {
        receiverTypeDecl = nullptr;
    }

    return getCurrentModule()->findDecls(callee, isPostProcessing ? nullptr : currentSourceFile, currentFunction,
                                         receiverTypeDecl);
}

Type Typechecker::typecheckCallExpr(CallExpr& expr) {
    if (!expr.callsNamedFunction()) {
        error(expr.getLocation(), "anonymous function calls not implemented yet");
    }

    if (Type::isBuiltinScalar(expr.getFunctionName())) {
        return typecheckBuiltinConversion(expr);
    }

    if (expr.isBuiltinCast()) {
        return typecheckBuiltinCast(expr);
    }

    for (auto& arg : expr.getArgs()) {
        typecheckExpr(*arg.getValue());
    }

    if (expr.getFunctionName() == "assert") {
        ParamDecl assertParam(Type::getBool(), "", SourceLocation());
        validateArgs(expr, false, assertParam, false, expr.getFunctionName(), expr.getLocation());
        validateGenericArgCount(0, expr.getGenericArgs(), expr.getFunctionName(), expr.getLocation());
        return Type::getVoid();
    }

    Decl* decl;

    if (expr.getCallee().isMemberExpr()) {
        Type receiverType = typecheckExpr(*expr.getReceiver());
        expr.setReceiverType(receiverType);

        if (receiverType.isOptionalType()) {
            warning(expr.getReceiver()->getLocation(), "calling member function through value of optional type '",
                    receiverType, "' which may be null; unwrap the value with a postfix '!' to silence this warning");
        } else if (receiverType.removePointer().isArrayType()) {
            if (expr.getFunctionName() == "size") {
                validateArgs(expr, false, {}, false, expr.getFunctionName(), expr.getLocation());
                validateGenericArgCount(0, expr.getGenericArgs(), expr.getFunctionName(), expr.getLocation());
                return Type::getUInt();
            }

            error(expr.getReceiver()->getLocation(), "type '", receiverType, "' has no member function '",
                  expr.getFunctionName(), "'");
        } else if (receiverType.removePointer().isBuiltinType() && expr.getFunctionName() == "deinit") {
            return Type::getVoid();
        }

        auto callee = expr.getQualifiedFunctionName();
        auto decls = findCalleeCandidates(expr, callee);

        if (expr.isMoveInit()) {
            checkCanMove(*expr.getArgs()[0].getValue());
            expr.getArgs()[0].getValue()->setMoved(true);
            return Type::getVoid();
        }

        if (decls.empty() && expr.getFunctionName() == "deinit") {
            return Type::getVoid();
        }

        decl = resolveOverload(decls, expr, callee);
    } else {
        auto callee = expr.getFunctionName();
        auto decls = findCalleeCandidates(expr, callee);
        // TODO: Prevent duplicates and remove this call:
        decls.erase(std::unique(decls.begin(), decls.end()), decls.end());
        decl = resolveOverload(decls, expr, callee);

        if (auto* initDecl = llvm::dyn_cast<InitDecl>(decl)) {
            expr.setReceiverType(initDecl->getTypeDecl()->getType());
        } else if (decl->isMethodDecl()) {
            auto& varDecl = llvm::cast<VarDecl>(getCurrentModule()->findDecl("this", expr.getCallee().getLocation(),
                                                                             currentSourceFile, currentFieldDecls));
            expr.setReceiverType(varDecl.getType());
        }
    }

    checkHasAccess(*decl, expr.getCallee().getLocation(), AccessLevel::None);
    if (auto* initDecl = llvm::dyn_cast<InitDecl>(decl)) {
        checkHasAccess(*initDecl->getTypeDecl(), expr.getCallee().getLocation(), AccessLevel::None);
    }

    std::vector<ParamDecl> paramsStorage;
    llvm::ArrayRef<ParamDecl> params;

    switch (decl->getKind()) {
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
        case DeclKind::InitDecl:
        case DeclKind::DeinitDecl:
            params = llvm::cast<FunctionDecl>(decl)->getParams();
            break;

        case DeclKind::VarDecl: {
            auto type = llvm::cast<VarDecl>(decl)->getType();
            paramsStorage = llvm::cast<FunctionType>(type.getBase())->getParamDecls();
            params = paramsStorage;
            break;
        }
        case DeclKind::ParamDecl: {
            auto type = llvm::cast<ParamDecl>(decl)->getType();
            paramsStorage = llvm::cast<FunctionType>(type.getBase())->getParamDecls();
            params = paramsStorage;
            break;
        }
        case DeclKind::FieldDecl: {
            auto type = llvm::cast<FieldDecl>(decl)->getType();
            paramsStorage = llvm::cast<FunctionType>(type.getBase())->getParamDecls();
            params = paramsStorage;
            break;
        }
        default:
            llvm_unreachable("invalid callee decl");
    }

    for (auto t : llvm::zip_first(params, expr.getArgs())) {
        if (!std::get<0>(t).getType().isImplicitlyCopyable()) {
            checkCanMove(*std::get<1>(t).getValue());
            std::get<1>(t).getValue()->setMoved(true);
        }
    }

    if (decl->isMethodDecl() && !decl->isInitDecl()) {
        ASSERT(expr.getReceiverType());
        auto* methodDecl = llvm::cast<MethodDecl>(decl);

        if (!decl->isDeinitDecl() && !expr.getReceiverType().removePointer().isMutable() && methodDecl->isMutating()) {
            error(expr.getCallee().getLocation(), "cannot call mutating function '",
                  methodDecl->getTypeDecl()->getName(), ".", methodDecl->getName(), "' on immutable receiver");
        }
    }

    expr.setCalleeDecl(decl);
    decl->setReferenced(true);

    switch (decl->getKind()) {
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
        case DeclKind::DeinitDecl:
            return llvm::cast<FunctionDecl>(decl)->getFunctionType()->getReturnType();

        case DeclKind::InitDecl:
            return llvm::cast<InitDecl>(decl)->getTypeDecl()->getType();

        case DeclKind::VarDecl:
            return llvm::cast<FunctionType>(llvm::cast<VarDecl>(decl)->getType().getBase())->getReturnType();

        case DeclKind::ParamDecl:
            return llvm::cast<FunctionType>(llvm::cast<ParamDecl>(decl)->getType().getBase())->getReturnType();

        case DeclKind::FieldDecl:
            return llvm::cast<FunctionType>(llvm::cast<FieldDecl>(decl)->getType().getBase())->getReturnType();

        default:
            llvm_unreachable("invalid callee decl");
    }
}

bool Typechecker::argumentsMatch(const CallExpr& expr, const FunctionDecl* functionDecl,
                                 llvm::ArrayRef<ParamDecl> params) const {
    bool isMutating;
    bool isVariadic;

    if (functionDecl) {
        isMutating = functionDecl->isMutating();
        isVariadic = functionDecl->isVariadic();
        params = functionDecl->getParams();
    } else {
        isMutating = false;
        isVariadic = false;
    }

    auto args = expr.getArgs();

    if (expr.getReceiver() && !expr.getReceiverType().removePointer().isMutable() && isMutating) {
        return false;
    }

    if (args.size() < params.size()) {
        return false;
    }

    if (!isVariadic && args.size() > params.size()) {
        return false;
    }

    for (size_t i = 0; i < args.size(); ++i) {
        auto& arg = args[i];
        const ParamDecl* param = i < params.size() ? &params[i] : nullptr;

        if (!arg.getName().empty() && (!param || arg.getName() != param->getName())) {
            return false;
        }

        auto argType = arg.getValue()->getType();

        if (param) {
            if (!isImplicitlyConvertible(arg.getValue(), argType, param->getType(), nullptr)) {
                return false;
            }
        }
    }

    return true;
}

void Typechecker::validateArgs(CallExpr& expr, const Decl& calleeDecl, llvm::StringRef functionName,
                               SourceLocation location) const {
    switch (calleeDecl.getKind()) {
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl:
        case DeclKind::InitDecl:
        case DeclKind::DeinitDecl: {
            auto& functionDecl = llvm::cast<FunctionDecl>(calleeDecl);
            validateArgs(expr, functionDecl.isMutating(), functionDecl.getParams(), functionDecl.isVariadic(),
                         functionName, location);
            break;
        }
        case DeclKind::VarDecl: {
            auto* varDecl = llvm::cast<VarDecl>(&calleeDecl);
            if (auto* functionType = llvm::dyn_cast<FunctionType>(varDecl->getType().getBase())) {
                auto paramDecls = functionType->getParamDecls(varDecl->getLocation());
                validateArgs(expr, false, paramDecls, false, functionName, location);
            }
        }
        case DeclKind::ParamDecl: {
            auto* paramDecl = llvm::cast<ParamDecl>(&calleeDecl);
            if (auto* functionType = llvm::dyn_cast<FunctionType>(paramDecl->getType().getBase())) {
                auto paramDecls = functionType->getParamDecls(paramDecl->getLocation());
                validateArgs(expr, false, paramDecls, false, functionName, location);
            }
        }
        case DeclKind::FieldDecl: {
            auto* fieldDecl = llvm::cast<FieldDecl>(&calleeDecl);
            if (auto* functionType = llvm::dyn_cast<FunctionType>(fieldDecl->getType().getBase())) {
                auto paramDecls = functionType->getParamDecls(fieldDecl->getLocation());
                validateArgs(expr, false, paramDecls, false, functionName, location);
            }
        }
        default:
            llvm_unreachable("invalid callee decl");
    }
}

void Typechecker::validateArgs(CallExpr& expr, bool isMutating, llvm::ArrayRef<ParamDecl> params, bool isVariadic,
                               llvm::StringRef functionName, SourceLocation location) const {
    auto args = expr.getArgs();

    if (expr.getReceiver() && !expr.getReceiverType().removePointer().isMutable() && isMutating) {
        error(location, "cannot call mutating member function '", functionName, "' on immutable receiver of type '",
              expr.getReceiverType(), "'");
    }

    validateArgCount(params.size(), args.size(), isVariadic, functionName, location);

    for (size_t i = 0; i < args.size(); ++i) {
        auto& arg = args[i];
        const ParamDecl* param = i < params.size() ? &params[i] : nullptr;

        if (!arg.getName().empty() && (!param || arg.getName() != param->getName())) {
            error(arg.getLocation(), "invalid argument name '", arg.getName(), "' for parameter '", param->getName(),
                  "'");
        }

        auto argType = arg.getValue()->getType();

        if (param) {
            Type convertedType;

            if (isImplicitlyConvertible(arg.getValue(), argType, param->getType(), &convertedType)) {
                arg.getValue()->setType(convertedType ? convertedType : argType);
            } else {
                error(arg.getLocation(), "invalid argument #", i + 1, " type '", argType, "' to '", functionName,
                      "', expected '", param->getType(), "'");
            }
        }
    }
}

static bool isValidCast(Type sourceType, Type targetType) {
    switch (sourceType.getKind()) {
        case TypeKind::BasicType:
        case TypeKind::ArrayType:
        case TypeKind::TupleType:
        case TypeKind::FunctionType:
            return false;

        case TypeKind::PointerType: {
            Type sourcePointee = sourceType.getPointee();

            if (targetType.isPointerType()) {
                Type targetPointee = targetType.getPointee();

                if (sourcePointee.isVoid() && (!targetPointee.isMutable() || sourcePointee.isMutable())) {
                    return true; // (mutable) void* -> T* / mutable void* -> mutable T*
                }

                if (targetPointee.isVoid() && (!targetPointee.isMutable() || sourcePointee.isMutable())) {
                    return true; // (mutable) T* -> void* / mutable T* -> mutable void*
                }
            }

            return false;
        }
        case TypeKind::OptionalType: {
            Type sourceWrappedType = sourceType.getWrappedType();

            if (sourceWrappedType.isPointerType() && targetType.isOptionalType()) {
                Type targetWrappedType = targetType.getWrappedType();

                if (targetWrappedType.isPointerType() && isValidCast(sourceWrappedType, targetWrappedType)) {
                    return true;
                }
            }

            return false;
        }
    }

    llvm_unreachable("all cases handled");
}

Type Typechecker::typecheckBuiltinCast(CallExpr& expr) {
    validateGenericArgCount(1, expr.getGenericArgs(), expr.getFunctionName(), expr.getLocation());
    validateArgCount(1, expr.getArgs().size(), false, expr.getFunctionName(), expr.getLocation());

    Type sourceType = typecheckExpr(*expr.getArgs().front().getValue());
    Type targetType = expr.getGenericArgs().front();

    if (isValidCast(sourceType, targetType)) {
        return targetType;
    }

    error(expr.getCallee().getLocation(), "illegal cast from '", sourceType, "' to '", targetType, "'");
}

Type Typechecker::typecheckSizeofExpr(SizeofExpr&) {
    return Type::getUInt64();
}

Type Typechecker::typecheckAddressofExpr(AddressofExpr& expr) {
    if (!typecheckExpr(expr.getOperand()).removeOptional().isPointerType()) {
        error(expr.getLocation(), "operand to 'addressof' must have pointer type");
    }
    return Type::getUIntPtr();
}

Type Typechecker::typecheckMemberExpr(MemberExpr& expr) {
    if (auto* varExpr = llvm::dyn_cast<VarExpr>(expr.getBaseExpr())) {
        auto decls = getCurrentModule()->findDecls(varExpr->getIdentifier(), currentSourceFile, currentFunction);

        if (!decls.empty()) {
            if (auto* enumDecl = llvm::dyn_cast<EnumDecl>(decls.front())) {
                checkHasAccess(*enumDecl, varExpr->getLocation(), AccessLevel::None);
                if (!enumDecl->getCaseByName(expr.getMemberName())) {
                    error(expr.getLocation(), "enum '", enumDecl->getName(), "' has no case named '",
                          expr.getMemberName(), "'");
                }
                return enumDecl->getType();
            }
        }
    }

    Type baseType = typecheckExpr(*expr.getBaseExpr());

    if (baseType.isOptionalType()) {
        warning(expr.getBaseExpr()->getLocation(), "accessing member through value of optional type '", baseType,
                "' which may be null; unwrap the value with a postfix '!' to silence this warning");
        baseType = baseType.getWrappedType();
    }

    if (baseType.isPointerType()) {
        baseType = baseType.getPointee();
    }

    if (baseType.isArrayType()) {
        auto sizeSynonyms = { "count", "length", "size" };

        if (llvm::is_contained(sizeSynonyms, expr.getMemberName())) {
            error(expr.getLocation(), "use the '.size()' member function to get the number of elements in an array");
        }
    }

    if (baseType.isBasicType()) {
        Decl& typeDecl = getCurrentModule()->findDecl(baseType.getQualifiedTypeName(), expr.getBaseExpr()->getLocation(),
                                                      currentSourceFile, currentFieldDecls);

        for (auto& field : llvm::cast<TypeDecl>(typeDecl).getFields()) {
            if (field.getName() == expr.getMemberName()) {
                checkHasAccess(field, expr.getLocation(), AccessLevel::None);

                if (baseType.isMutable()) {
                    auto* varExpr = llvm::dyn_cast<VarExpr>(expr.getBaseExpr());

                    if (varExpr && varExpr->getIdentifier() == "this" &&
                        (currentFunction->isInitDecl() || currentFunction->isDeinitDecl())) {
                        return field.getType().asMutable(true);
                    } else {
                        return field.getType();
                    }
                } else {
                    return field.getType().asImmutable();
                }
            }
        }
    }

    if (baseType.isTupleType()) {
        for (auto& element : baseType.getTupleElements()) {
            if (element.name == expr.getMemberName()) {
                return element.type;
            }
        }
    }

    error(expr.getLocation(), "no member named '", expr.getMemberName(), "' in '", baseType, "'");
}

Type Typechecker::typecheckSubscriptExpr(SubscriptExpr& expr) {
    Type lhsType = typecheckExpr(*expr.getBaseExpr());
    Type arrayType;

    if (lhsType.isArrayType()) {
        arrayType = lhsType;
    } else if (lhsType.isPointerType() && lhsType.getPointee().isArrayType()) {
        arrayType = lhsType.getPointee();
    } else if (lhsType.removePointer().isBuiltinType()) {
        error(expr.getLocation(), "'", lhsType, "' doesn't provide a subscript operator");
    } else {
        return typecheckCallExpr(expr);
    }

    Type indexType = typecheckExpr(*expr.getIndexExpr());
    Type convertedType;

    if (isImplicitlyConvertible(expr.getIndexExpr(), indexType, ArrayType::getIndexType(), &convertedType)) {
        expr.getIndexExpr()->setType(convertedType ? convertedType : indexType);
    } else {
        error(expr.getIndexExpr()->getLocation(), "illegal subscript index type '", indexType, "', expected '",
              ArrayType::getIndexType(), "'");
    }

    if (arrayType.isArrayWithConstantSize()) {
        if (expr.getIndexExpr()->isConstant()) {
            auto index = expr.getIndexExpr()->getConstantIntegerValue();

            if (index < 0 || index >= arrayType.getArraySize()) {
                error(expr.getIndexExpr()->getLocation(), "accessing array out-of-bounds with index ", index,
                      ", array size is ", arrayType.getArraySize());
            }
        }
    }

    return arrayType.getElementType();
}

Type Typechecker::typecheckUnwrapExpr(UnwrapExpr& expr) {
    Type type = typecheckExpr(expr.getOperand());
    if (!type.isOptionalType()) {
        error(expr.getLocation(), "cannot unwrap non-optional type '", type, "'");
    }
    return type.getWrappedType();
}

Type Typechecker::typecheckLambdaExpr(LambdaExpr& expr) {
    getCurrentModule()->getSymbolTable().pushScope();

    typecheckParams(expr.getParams(), AccessLevel::None);
    auto returnType = typecheckExpr(*expr.getBody());

    getCurrentModule()->getSymbolTable().popScope();

    auto paramTypes = map(expr.getParams(), [](const ParamDecl& p) { return p.getType(); });
    return FunctionType::get(returnType, std::move(paramTypes));
}

Type Typechecker::typecheckIfExpr(IfExpr& expr) {
    auto conditionType = typecheckExpr(*expr.getCondition());

    if (!conditionType.isBool() && !conditionType.isOptionalType()) {
        error(expr.getCondition()->getLocation(), "if-expression condition must have type 'bool' or optional type");
    }

    auto thenType = typecheckExpr(*expr.getThenExpr());
    auto elseType = typecheckExpr(*expr.getElseExpr());
    Type convertedType;

    if (isImplicitlyConvertible(expr.getThenExpr(), thenType, elseType, &convertedType)) {
        if (convertedType) expr.getThenExpr()->setType(convertedType);
        return elseType;
    } else if (isImplicitlyConvertible(expr.getElseExpr(), elseType, thenType, &convertedType)) {
        if (convertedType) expr.getThenExpr()->setType(convertedType);
        return thenType;
    } else {
        error(expr.getLocation(), "incompatible operand types ('", thenType, "' and '", elseType, "')");
    }
}

Type Typechecker::typecheckExpr(Expr& expr, bool useIsWriteOnly) {
    llvm::Optional<Type> type;

    switch (expr.getKind()) {
        case ExprKind::VarExpr:
            type = typecheckVarExpr(llvm::cast<VarExpr>(expr), useIsWriteOnly);
            break;
        case ExprKind::StringLiteralExpr:
            type = typecheckStringLiteralExpr(llvm::cast<StringLiteralExpr>(expr));
            break;
        case ExprKind::CharacterLiteralExpr:
            type = typecheckCharacterLiteralExpr(llvm::cast<CharacterLiteralExpr>(expr));
            break;
        case ExprKind::IntLiteralExpr:
            type = typecheckIntLiteralExpr(llvm::cast<IntLiteralExpr>(expr));
            break;
        case ExprKind::FloatLiteralExpr:
            type = typecheckFloatLiteralExpr(llvm::cast<FloatLiteralExpr>(expr));
            break;
        case ExprKind::BoolLiteralExpr:
            type = typecheckBoolLiteralExpr(llvm::cast<BoolLiteralExpr>(expr));
            break;
        case ExprKind::NullLiteralExpr:
            type = typecheckNullLiteralExpr(llvm::cast<NullLiteralExpr>(expr));
            break;
        case ExprKind::UndefinedLiteralExpr:
            type = typecheckUndefinedLiteralExpr(llvm::cast<UndefinedLiteralExpr>(expr));
            break;
        case ExprKind::ArrayLiteralExpr:
            type = typecheckArrayLiteralExpr(llvm::cast<ArrayLiteralExpr>(expr));
            break;
        case ExprKind::TupleExpr:
            type = typecheckTupleExpr(llvm::cast<TupleExpr>(expr));
            break;
        case ExprKind::UnaryExpr:
            type = typecheckUnaryExpr(llvm::cast<UnaryExpr>(expr));
            break;
        case ExprKind::BinaryExpr:
            type = typecheckBinaryExpr(llvm::cast<BinaryExpr>(expr));
            break;
        case ExprKind::CallExpr:
            type = typecheckCallExpr(llvm::cast<CallExpr>(expr));
            break;
        case ExprKind::SizeofExpr:
            type = typecheckSizeofExpr(llvm::cast<SizeofExpr>(expr));
            break;
        case ExprKind::AddressofExpr:
            type = typecheckAddressofExpr(llvm::cast<AddressofExpr>(expr));
            break;
        case ExprKind::MemberExpr:
            type = typecheckMemberExpr(llvm::cast<MemberExpr>(expr));
            break;
        case ExprKind::SubscriptExpr:
            type = typecheckSubscriptExpr(llvm::cast<SubscriptExpr>(expr));
            break;
        case ExprKind::UnwrapExpr:
            type = typecheckUnwrapExpr(llvm::cast<UnwrapExpr>(expr));
            break;
        case ExprKind::LambdaExpr:
            type = typecheckLambdaExpr(llvm::cast<LambdaExpr>(expr));
            break;
        case ExprKind::IfExpr:
            type = typecheckIfExpr(llvm::cast<IfExpr>(expr));
            break;
    }

    if (type->isOptionalType() && isGuaranteedNonNull(expr)) {
        expr.setType(type->removeOptional());
    } else {
        expr.setType(*type);
    }
    expr.setAssignableType(*type);
    return expr.getType();
}
