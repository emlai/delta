#include "irgen.h"
#pragma warning(push, 0)
#include <llvm/Support/Path.h>
#pragma warning(pop)
#include "../ast/module.h"
#include "../support/utility.h"

using namespace delta;

Value* IRGenerator::emitVarExpr(const VarExpr& expr) {
    return getValue(expr.getDecl());
}

Value* IRGenerator::emitStringLiteralExpr(const StringLiteralExpr& expr) {
    auto* stringPtr = createGlobalStringPtr(expr.getValue());
    auto* size = createConstantInt(Type::getInt(), expr.getValue().size());
    auto* alloca = createEntryBlockAlloca(BasicType::get("string", {}), "__str");
    Function* stringConstructor = nullptr;

    for (auto* decl : Module::getStdlibModule()->getSymbolTable().find("string.init")) {
        auto params = llvm::cast<ConstructorDecl>(decl)->getParams();
        if (params.size() == 2 && params[0].getType().isPointerType() && params[1].getType().isInt()) {
            stringConstructor = getFunctionProto(*llvm::cast<ConstructorDecl>(decl));
            break;
        }
    }

    ASSERT(stringConstructor);
    createCall(stringConstructor, {alloca, stringPtr, size});
    return alloca;
}

Value* IRGenerator::emitCharacterLiteralExpr(const CharacterLiteralExpr& expr) {
    return createConstantInt(getILType(expr.getType()), expr.getValue());
}

Value* IRGenerator::emitIntLiteralExpr(const IntLiteralExpr& expr) {
    // Integer literals may be typed as floating-point when used in a context
    // that requires a floating-point value. It might make sense to combine
    // IntLiteralExpr and FloatLiteralExpr into a single class.
    if (expr.getType().isFloatingPoint()) {
        return createConstantFP(expr.getType(), expr.getValue().roundToDouble());
    }

    return createConstantInt(expr.getType(), expr.getValue().getExtValue());
}

Value* IRGenerator::emitFloatLiteralExpr(const FloatLiteralExpr& expr) {
    return createConstantFP(expr.getType(), expr.getValue());
}

Value* IRGenerator::emitBoolLiteralExpr(const BoolLiteralExpr& expr) {
    return createConstantBool(expr.getValue());
}

Value* IRGenerator::emitNullLiteralExpr(const NullLiteralExpr& expr) {
    if (expr.getType().isImplementedAsPointer()) {
        return createConstantNull(expr.getType());
    } else {
        return emitOptionalConstruction(expr.getType().getWrappedType(), nullptr);
    }
}

Value* IRGenerator::emitOptionalConstruction(Type wrappedType, Value* arg) {
    auto* decl = Module::getStdlibModule()->getSymbolTable().findOne("Optional");
    auto typeTemplate = llvm::cast<TypeTemplate>(decl);
    auto typeDecl = typeTemplate->instantiate(wrappedType);
    Function* optionalConstructor = nullptr;

    for (auto* constructor : typeDecl->getConstructors()) {
        if (constructor->getParams().size() == (arg ? 1 : 0)) {
            optionalConstructor = getFunctionProto(*constructor);
            break;
        }
    }

    ASSERT(optionalConstructor);
    auto* alloca = createEntryBlockAlloca(typeDecl->getType());
    llvm::SmallVector<Value*, 2> args;
    args.push_back(alloca);
    if (arg) args.push_back(arg);
    createCall(optionalConstructor, args);
    return alloca;
}

Value* IRGenerator::emitUndefinedLiteralExpr(const UndefinedLiteralExpr& expr) {
    return createUndefined(expr.getType());
}

Value* IRGenerator::emitArrayLiteralExpr(const ArrayLiteralExpr& expr) {
    Value* array = createUndefined(expr.getType());
    auto index = 0;

    for (auto& element : expr.getElements()) {
        auto* value = emitExpr(*element);
        array = createInsertValue(array, value, index++);
    }

    return array;
}

Value* IRGenerator::emitTupleExpr(const TupleExpr& expr) {
    Value* tuple = createUndefined(expr.getType());
    unsigned index = 0;
    for (auto& element : expr.getElements()) {
        tuple = createInsertValue(tuple, emitExpr(*element.getValue()), index++);
    }
    return tuple;
}

Value* IRGenerator::emitImplicitNullComparison(Value* operand) {
    return createBinaryOp(Token::NotEqual, operand, createConstantNull(operand->getType()));
}

Value* IRGenerator::emitNot(const UnaryExpr& expr) {
    auto* operand = emitExpr(expr.getOperand());
    if (operand->getType()->isPointerType()) {
        return createBinaryOp(Token::Equal, operand, createConstantNull(operand->getType())); //  TODO(ir): refactor to emitImplicitNullComparison?
    }
    return createNot(operand);
}

Value* IRGenerator::emitUnaryExpr(const UnaryExpr& expr) {
    switch (expr.getOperator()) {
        case Token::Plus:
            return emitExpr(expr.getOperand());
        case Token::Minus:
            return createNeg(emitExpr(expr.getOperand()));
        case Token::Star:
            return emitExpr(expr.getOperand());
        case Token::And:
            return emitExprAsPointer(expr.getOperand());
        case Token::Not:
            // FIXME: Temporary hack. Lower implicit null checks such as `if (ptr)` and `if (!ptr)` when expression lowering is implemented.
            if (expr.getOperand().getType().isOptionalType() && !expr.getOperand().getType().getWrappedType().isPointerType()) {
                auto operand = emitExpr(expr.getOperand());
                auto hasValue = createExtractValue(operand, optionalHasValueFieldIndex);
                return createNot(hasValue);
            }
            LLVM_FALLTHROUGH;
        case Token::Tilde:
            return emitNot(expr);
        case Token::Increment:
            return emitConstantIncrement(expr, 1);
        case Token::Decrement:
            return emitConstantIncrement(expr, -1);
        default:
            llvm_unreachable("invalid prefix operator");
    }
}

// TODO: Lower increment and decrement statements to compound assignments so this isn't needed.
Value* IRGenerator::emitConstantIncrement(const UnaryExpr& expr, int increment) {
    auto operandType = expr.getOperand().getType();
    auto* ptr = emitLvalueExpr(expr.getOperand());
    if (operandType.isPointerType() && llvm::isa<AllocaInst>(ptr)) {
        ptr = createLoad(ptr);
    }
    auto* value = createLoad(ptr);
    Value* result;

    if (value->getType()->isInteger()) {
        result = createBinaryOp(Token::Plus, value, createConstantInt(value->getType(), increment));
    } else if (value->getType()->isPointerType()) {
        result = createGEP(value, {createConstantInt(Type::getInt(), increment)});
    } else if (value->getType()->isFloatingPoint()) {
        result = createBinaryOp(Token::Plus, value, createConstantFP(value->getType(), increment));
    } else {
        llvm_unreachable("unknown increment/decrement operand type");
    }

    createStore(result, ptr);
    return nullptr;
}

Value* IRGenerator::emitLogicalAnd(const Expr& left, const Expr& right) {
    auto* rhsBlock = new BasicBlock("and.rhs", insertBlock->parent);
    auto* endBlock = new BasicBlock("and.end");

    Value* lhs = emitExpr(left);
    createCondBr(lhs, rhsBlock, endBlock);
    auto* lhsBlock = insertBlock;

    setInsertPoint(rhsBlock);
    Value* rhs = emitExpr(right);
    createBr(endBlock);
    rhsBlock = insertBlock;

    currentFunction->body.push_back(endBlock); // TODO(ir) cleanup
    endBlock->parent = currentFunction;
    setInsertPoint(endBlock);
    return createPhi({{lhs, lhsBlock}, {rhs, rhsBlock}}, "and");
}

Value* IRGenerator::emitLogicalOr(const Expr& left, const Expr& right) {
    auto* rhsBlock = new BasicBlock("or.rhs", insertBlock->parent);
    auto* endBlock = new BasicBlock("or.end");

    Value* lhs = emitExpr(left);
    createCondBr(lhs, endBlock, rhsBlock);
    auto* lhsBlock = insertBlock;

    setInsertPoint(rhsBlock);
    Value* rhs = emitExpr(right);
    createBr(endBlock);
    rhsBlock = insertBlock;

    currentFunction->body.push_back(endBlock); // TODO(ir) cleanup
    endBlock->parent = currentFunction;
    setInsertPoint(endBlock);
    return createPhi({{lhs, lhsBlock}, {rhs, rhsBlock}}, "or");
}

Value* IRGenerator::emitBinaryExpr(const BinaryExpr& expr) {
    if (expr.isAssignment()) {
        emitAssignment(expr);
        return nullptr;
    }

    if (expr.getCalleeDecl() != nullptr) {
        return emitCallExpr(expr);
    }

    switch (expr.getOperator()) {
        case Token::AndAnd:
            return emitLogicalAnd(expr.getLHS(), expr.getRHS());
        case Token::OrOr:
            return emitLogicalOr(expr.getLHS(), expr.getRHS());
        default:
            auto left = emitExprOrEnumTag(expr.getLHS(), nullptr);
            auto right = emitExprOrEnumTag(expr.getRHS(), nullptr);
            return createBinaryOp(expr.getOperator(), left, right);
    }
}

void IRGenerator::emitAssignment(const BinaryExpr& expr) {
    if (expr.getRHS().isUndefinedLiteralExpr()) return;

    auto lvalue = emitAssignmentLHS(expr.getLHS());
    auto rvalue = emitExprForPassing(expr.getRHS(), lvalue->getType()->getPointee());
    createStore(rvalue, lvalue);
}

static bool isBuiltinArrayToArrayRefConversion(Type sourceType, IRType* targetType) {
    return sourceType.removePointer().isArrayWithConstantSize() && targetType->isStruct() && targetType->getName().startswith("ArrayRef<"); // TODO(ir)
}

Value* IRGenerator::emitExprForPassing(const Expr& expr, IRType* targetType) {
    if (!targetType) {
        return emitExpr(expr);
    }

    // TODO: Handle implicit conversions in a separate function.

    if (isBuiltinArrayToArrayRefConversion(expr.getType(), targetType)) {
        ASSERT(expr.getType().removePointer().isArrayWithConstantSize());
        auto* value = emitExprAsPointer(expr);
        auto* elementPtr = createGEP(value, 0, 0);
        auto* arrayRef = createInsertValue(createUndefined(targetType), elementPtr, 0);
        auto size = createConstantInt(Type::getInt(), expr.getType().removePointer().getArraySize());
        return createInsertValue(arrayRef, size, 1);
    }

    // Handle implicit conversions to type 'T[*]'.
    if (expr.getType().removePointer().isArrayWithConstantSize() && targetType->isPointerType() && !targetType->getPointee()->isArrayType()) {
        return createCast(emitLvalueExpr(expr), targetType);
    }

    // Handle implicit conversions to void pointer, and to base type pointer.
    if (expr.getType().isImplementedAsPointer() && targetType->isPointerType()) {
        return createCast(emitExpr(expr), targetType);
    }

    // TODO: Refactor the following.
    auto* value = emitLvalueExpr(expr);

    if (targetType->isPointerType() && value->getType()->equals(targetType->getPointee())) {
        return createTempAlloca(value);
    } else if (value->getType()->isPointerType() && !targetType->equals(value->getType())) {
        value = createLoad(value);
        if (value->getType()->isPointerType() && !targetType->equals(value->getType())) {
            value = createLoad(value);
        }
        return value;
    } else {
        return value;
    }
}

void IRGenerator::emitAssert(Value* condition, SourceLocation location, llvm::StringRef message) {
    condition = createIsNull(condition, "assert.condition");
    auto* function = insertBlock->parent;
    auto* failBlock = new BasicBlock("assert.fail", function);
    auto* successBlock = new BasicBlock("assert.success", function);
    auto* assertFail = getFunctionProto(*llvm::cast<FunctionDecl>(Module::getStdlibModule()->getSymbolTable().findOne("assertFail")));
    createCondBr(condition, failBlock, successBlock);
    setInsertPoint(failBlock);
    auto messageAndLocation = llvm::join_items("", message, " at ", llvm::sys::path::filename(location.file), ":", std::to_string(location.line), ":",
                                               std::to_string(location.column), "\n");
    createCall(assertFail, createGlobalStringPtr(messageAndLocation));
    createUnreachable();
    setInsertPoint(successBlock);
}

Value* IRGenerator::emitEnumCase(const EnumCase& enumCase, llvm::ArrayRef<NamedValue> associatedValueElements) {
    auto enumDecl = enumCase.getEnumDecl();
    auto tag = emitExpr(*enumCase.getValue());
    if (!enumDecl->hasAssociatedValues()) return tag;

    // TODO: Could reuse variable alloca instead of always creating a new one here.
    auto* enumValue = createEntryBlockAlloca(enumDecl->getType(), "enum");
    createStore(tag, createGEP(enumValue, 0, 0, "tag"));

    if (!associatedValueElements.empty()) {
        // TODO: This is duplicated in emitTupleExpr.
        Value* associatedValue = createUndefined(enumCase.getAssociatedType());
        int index = 0;
        for (auto& element : associatedValueElements) {
            associatedValue = createInsertValue(associatedValue, emitExpr(*element.getValue()), index++);
        }
        auto* associatedValuePtr = createCast(createGEP(enumValue, 0, 1, "associatedValue"), associatedValue->getType()->getPointerTo());
        createStore(associatedValue, associatedValuePtr);
    }

    return enumValue;
}

Value* IRGenerator::emitCallExpr(const CallExpr& expr, AllocaInst* thisAllocaForInit) {
    if (expr.isBuiltinConversion()) {
        return createCast(emitExpr(*expr.getArgs().front().getValue()), expr.getType());
    }

    if (expr.isBuiltinCast()) {
        return emitBuiltinCast(expr);
    }

    if (expr.getFunctionName() == "assert") {
        emitAssert(emitExpr(*expr.getArgs().front().getValue()), expr.getCallee().getLocation());
        return nullptr;
    }

    if (auto* enumCase = llvm::dyn_cast_or_null<EnumCase>(expr.getCalleeDecl())) {
        return emitEnumCase(*enumCase, expr.getArgs());
    }

    if (expr.getReceiver() && expr.getReceiverType().removePointer().isArrayType()) {
        if (expr.getFunctionName() == "size") {
            return getArrayLength(*expr.getReceiver(), expr.getReceiverType().removePointer());
        }
        if (expr.getFunctionName() == "iterator") {
            return getArrayIterator(*expr.getReceiver(), expr.getReceiverType().removePointer());
        }
        llvm_unreachable("unknown array member function");
    }

    if (expr.isMoveInit()) {
        auto* receiverValue = emitExpr(*expr.getReceiver());
        auto* argumentValue = emitExpr(*expr.getArgs()[0].getValue());
        createStore(argumentValue, receiverValue);
        return nullptr;
    }

    Value* calleeValue = getFunctionForCall(expr);

    if (!calleeValue) {
        return nullptr;
    }

    std::vector<IRType*> params;

    if (auto* function = llvm::dyn_cast<Function>(calleeValue)) {
        params = map(function->params, [](const Parameter& p) { return p.type; });
    } else {
        if (!calleeValue->getType()->getPointee()->isFunctionType()) {
            calleeValue = createLoad(calleeValue);
        }
        params = calleeValue->getType()->getPointee()->getParamTypes();
    }

    auto param = params.begin();
    llvm::SmallVector<Value*, 16> args;
    auto* calleeDecl = expr.getCalleeDecl();

    if (calleeDecl->isMethodDecl()) {
        if (auto* constructorDecl = llvm::dyn_cast<ConstructorDecl>(calleeDecl)) {
            if (thisAllocaForInit) {
                args.emplace_back(thisAllocaForInit);
            } else if (currentDecl->isConstructorDecl() && expr.getFunctionName() == "init") {
                args.emplace_back(getThis(*param));
            } else {
                args.emplace_back(createEntryBlockAlloca(constructorDecl->getTypeDecl()->getType()));
            }
        } else if (expr.getReceiver()) {
            args.emplace_back(emitExprForPassing(*expr.getReceiver(), *param));
        } else {
            args.emplace_back(getThis());
        }
        ++param;
    }

    for (const auto& arg : expr.getArgs()) {
        auto paramType = param != params.end() ? *param++ : nullptr;
        auto* argValue = emitExprForPassing(*arg.getValue(), paramType);
        //        if (paramType) ASSERT_EQUAL(argValue->getType(), paramType); // TODO(ir)
        args.push_back(argValue);
    }

    if (calleeDecl->isConstructorDecl()) {
        createCall(calleeValue, args);
        return args[0];
    } else {
        return createCall(calleeValue, args);
    }
}

Value* IRGenerator::emitBuiltinCast(const CallExpr& expr) {
    auto* value = emitExpr(*expr.getArgs().front().getValue());
    auto type = expr.getGenericArgs().front();
    return createCast(value, type);
}

Value* IRGenerator::emitSizeofExpr(const SizeofExpr& expr) {
    return createSizeof(expr.getType());
}

Value* IRGenerator::emitAddressofExpr(const AddressofExpr& expr) {
    Value* value = emitExpr(expr.getOperand());
    return createCast(value, Type::getUIntPtr(), "address");
}

Value* IRGenerator::emitMemberAccess(Value* baseValue, const FieldDecl* field) {
    auto baseTypeDecl = field->getParentDecl();
    auto baseType = baseValue->getType();

    if (baseType->isPointerType()) {
        baseType = baseType->getPointee();

        if (baseType->isPointerType()) {
            baseValue = createLoad(baseValue);
        }

        if (baseTypeDecl->isUnion()) {
            return createCast(baseValue, field->getType().getPointerTo(), field->getName());
        } else {
            return createGEP(baseValue, 0, baseTypeDecl->getFieldIndex(field), field->getName());
        }
    } else {
        auto index = baseTypeDecl->isUnion() ? 0 : baseTypeDecl->getFieldIndex(field);
        return createExtractValue(baseValue, index, field->getName());
    }
}

Value* IRGenerator::getArrayLength(const Expr& object, Type objectType) {
    if (objectType.isArrayWithRuntimeSize()) {
        return createExtractValue(emitExpr(object), 1, "size");
    } else {
        return createConstantInt(Type::getInt(), objectType.getArraySize());
    }
}

Value* IRGenerator::getArrayIterator(const Expr& object, Type objectType) {
    auto type = BasicType::get("ArrayIterator", objectType.getElementType());
    auto* value = emitExprAsPointer(object);
    auto* elementPtr = createGEP(value, 0, 0);
    auto* size = getArrayLength(object, objectType);
    auto* end = createGEP(elementPtr, {size});
    auto* iterator = createInsertValue(createUndefined(type), elementPtr, 0);
    return createInsertValue(iterator, end, 1);
}

Value* IRGenerator::emitMemberExpr(const MemberExpr& expr) {
    if (auto* enumCase = llvm::dyn_cast_or_null<EnumCase>(expr.getDecl())) {
        return emitEnumCase(*enumCase, {});
    }

    if (expr.getBaseExpr()->getType().removePointer().isTupleType()) {
        return emitTupleElementAccess(expr);
    }

    return emitMemberAccess(emitLvalueExpr(*expr.getBaseExpr()), llvm::cast<FieldDecl>(expr.getDecl()));
}

Value* IRGenerator::emitTupleElementAccess(const MemberExpr& expr) {
    unsigned index = 0;
    for (auto& element : expr.getBaseExpr()->getType().removePointer().getTupleElements()) {
        if (element.name == expr.getMemberName()) break;
        ++index;
    }

    auto* baseValue = emitLvalueExpr(*expr.getBaseExpr());
    if (baseValue->getType()->isPointerType() && baseValue->getType()->getPointee()->isPointerType()) {
        baseValue = createLoad(baseValue);
    }

    if (baseValue->getType()->isPointerType()) {
        return createGEP(baseValue, 0, index, expr.getMemberName());
    } else {
        return createExtractValue(baseValue, index, expr.getMemberName());
    }
}

Value* IRGenerator::emitIndexExpr(const IndexExpr& expr) {
    if (!expr.getBase()->getType().removePointer().isArrayType()) {
        return emitCallExpr(expr);
    }

    auto* value = emitLvalueExpr(*expr.getBase());
    Type lhsType = expr.getBase()->getType();

    if (lhsType.isArrayWithRuntimeSize()) {
        if (value->getType()->isPointerType()) {
            value = createLoad(value);
        }
        auto* ptr = createExtractValue(value, 0);
        auto* index = emitExpr(*expr.getIndex());
        return createGEP(ptr, {index});
    }

    if (value->getType()->isPointerType() && value->getType()->getPointee()->isPointerType() && value->getType()->getPointee()->equals(getILType(lhsType))) {
        value = createLoad(value);
    }

    if (lhsType.isArrayWithUnknownSize()) {
        return createGEP(value, {emitExpr(*expr.getIndex())});
    }

    return createGEP(value, {createConstantInt(Type::getInt(), 0), emitExpr(*expr.getIndex())});
}

Value* IRGenerator::emitUnwrapExpr(const UnwrapExpr& expr) {
    auto* value = emitExpr(expr.getOperand());
    llvm::StringRef message = "Unwrap failed";

    if (expr.getOperand().getType().isImplementedAsPointer()) {
        emitAssert(value, expr.getLocation(), message);
        return value;
    } else {
        emitAssert(createExtractValue(value, optionalHasValueFieldIndex), expr.getLocation(), message);
        return createExtractValue(value, optionalValueFieldIndex);
    }
}

Value* IRGenerator::emitLambdaExpr(const LambdaExpr& expr) {
    auto functionDecl = expr.getFunctionDecl();

    auto insertBlockBackup = insertBlock;
    auto scopesBackup = std::move(scopes);

    emitFunctionDecl(*functionDecl);

    scopes = std::move(scopesBackup);
    if (insertBlockBackup) setInsertPoint(insertBlockBackup);

    VarExpr varExpr(functionDecl->getName(), functionDecl->getLocation());
    varExpr.setDecl(functionDecl);
    varExpr.setType(expr.getType());
    return emitVarExpr(varExpr);
}

Value* IRGenerator::emitIfExpr(const IfExpr& expr) {
    auto* condition = emitExpr(*expr.getCondition());
    if (condition->getType()->isPointerType()) {
        condition = emitImplicitNullComparison(condition);
    }
    auto* function = currentFunction;
    auto* thenBlock = new BasicBlock("if.then", function);
    auto* elseBlock = new BasicBlock("if.else");
    auto* endIfBlock = new BasicBlock("if.end");
    createCondBr(condition, thenBlock, elseBlock);

    setInsertPoint(thenBlock);
    auto* thenValue = emitExpr(*expr.getThenExpr());
    createBr(endIfBlock);
    thenBlock = insertBlock;

    function->body.push_back(elseBlock); // TODO(ir) remove these if not needed ( function added constructor calls)
    elseBlock->parent = function; // TODO(ir) move these to ctor?
    setInsertPoint(elseBlock);
    auto* elseValue = emitExpr(*expr.getElseExpr());
    createBr(endIfBlock);
    elseBlock = insertBlock;

    function->body.push_back(endIfBlock);
    endIfBlock->parent = function;
    setInsertPoint(endIfBlock);
    return createPhi({{thenValue, thenBlock}, {elseValue, elseBlock}}, "phi");
}

Value* IRGenerator::emitImplicitCastExpr(const ImplicitCastExpr& expr) {
    if (expr.getType().isOptionalType() && !expr.getType().getWrappedType().isImplementedAsPointer() &&
        expr.getOperand()->getType() == expr.getType().getWrappedType()) {
        return emitOptionalConstruction(expr.getOperand()->getType(), emitExprWithoutAutoCast(*expr.getOperand()));
    }

    if (expr.getOperand()->isStringLiteralExpr() && expr.getType().removeOptional().isPointerType() && expr.getType().removeOptional().getPointee().isChar()) {
        return createGlobalStringPtr(llvm::cast<StringLiteralExpr>(expr.getOperand())->getValue());
    }

    return emitExprWithoutAutoCast(*expr.getOperand());
}

Value* IRGenerator::emitExprWithoutAutoCast(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::VarExpr:
            return emitVarExpr(llvm::cast<VarExpr>(expr));
        case ExprKind::StringLiteralExpr:
            return emitStringLiteralExpr(llvm::cast<StringLiteralExpr>(expr));
        case ExprKind::CharacterLiteralExpr:
            return emitCharacterLiteralExpr(llvm::cast<CharacterLiteralExpr>(expr));
        case ExprKind::IntLiteralExpr:
            return emitIntLiteralExpr(llvm::cast<IntLiteralExpr>(expr));
        case ExprKind::FloatLiteralExpr:
            return emitFloatLiteralExpr(llvm::cast<FloatLiteralExpr>(expr));
        case ExprKind::BoolLiteralExpr:
            return emitBoolLiteralExpr(llvm::cast<BoolLiteralExpr>(expr));
        case ExprKind::NullLiteralExpr:
            return emitNullLiteralExpr(llvm::cast<NullLiteralExpr>(expr));
        case ExprKind::UndefinedLiteralExpr:
            return emitUndefinedLiteralExpr(llvm::cast<UndefinedLiteralExpr>(expr));
        case ExprKind::ArrayLiteralExpr:
            return emitArrayLiteralExpr(llvm::cast<ArrayLiteralExpr>(expr));
        case ExprKind::TupleExpr:
            return emitTupleExpr(llvm::cast<TupleExpr>(expr));
        case ExprKind::UnaryExpr:
            return emitUnaryExpr(llvm::cast<UnaryExpr>(expr));
        case ExprKind::BinaryExpr:
            return emitBinaryExpr(llvm::cast<BinaryExpr>(expr));
        case ExprKind::CallExpr:
            return emitCallExpr(llvm::cast<CallExpr>(expr));
        case ExprKind::SizeofExpr:
            return emitSizeofExpr(llvm::cast<SizeofExpr>(expr));
        case ExprKind::AddressofExpr:
            return emitAddressofExpr(llvm::cast<AddressofExpr>(expr));
        case ExprKind::MemberExpr:
            return emitMemberExpr(llvm::cast<MemberExpr>(expr));
        case ExprKind::IndexExpr:
            return emitIndexExpr(llvm::cast<IndexExpr>(expr));
        case ExprKind::UnwrapExpr:
            return emitUnwrapExpr(llvm::cast<UnwrapExpr>(expr));
        case ExprKind::LambdaExpr:
            return emitLambdaExpr(llvm::cast<LambdaExpr>(expr));
        case ExprKind::IfExpr:
            return emitIfExpr(llvm::cast<IfExpr>(expr));
        case ExprKind::ImplicitCastExpr:
            return emitImplicitCastExpr(llvm::cast<ImplicitCastExpr>(expr));
    }
    llvm_unreachable("all cases handled");
}

Value* IRGenerator::emitExpr(const Expr& expr) {
    auto* value = emitLvalueExpr(expr);

    if (value) {
        // FIXME: Temporary
        if (auto implicitCastExpr = llvm::dyn_cast<ImplicitCastExpr>(&expr)) {
            if (value->getType()->isPointerType() && value->getType()->getPointee()->equals(getILType(implicitCastExpr->getOperand()->getType()))) {
                return createLoad(value);
            }
        }

        // TODO(ir): are these needed?
        if (value->getType()->isPointerType() && value->getType()->getPointee()->equals(getILType(expr.getType()))) {
            return createLoad(value);
        }
    }

    return value;
}

Value* IRGenerator::emitLvalueExpr(const Expr& expr) {
    return emitAutoCast(emitExprWithoutAutoCast(expr), expr);
}

Value* IRGenerator::emitExprAsPointer(const Expr& expr) {
    auto* value = emitLvalueExpr(expr);
    if (!value->getType()->isPointerType()) {
        value = createTempAlloca(value);
    }
    return value;
}

Value* IRGenerator::emitExprOrEnumTag(const Expr& expr, Value** enumValue) {
    if (auto* memberExpr = llvm::dyn_cast<MemberExpr>(&expr)) {
        if (auto* enumCase = llvm::dyn_cast_or_null<EnumCase>(memberExpr->getDecl())) {
            return emitExpr(*enumCase->getValue());
        }
    }

    if (auto* enumDecl = llvm::dyn_cast_or_null<EnumDecl>(expr.getType().getDecl())) {
        if (enumDecl->hasAssociatedValues()) {
            auto* value = emitLvalueExpr(expr);
            if (enumValue) *enumValue = value;
            return createLoad(createGEP(value, 0, 0, value->getName() + ".tag"));
        }
    }

    return emitExpr(expr);
}

Value* IRGenerator::emitAutoCast(Value* value, const Expr& expr) {
    // Handle optionals that have been implicitly unwrapped due to data-flow analysis.
    if (expr.hasAssignableType() && expr.getAssignableType().isOptionalType() && !expr.getAssignableType().getWrappedType().isPointerType() &&
        expr.getType() == expr.getAssignableType().getWrappedType()) {
        return createGEP(value, 0, optionalValueFieldIndex);
    }

    if (value && expr.hasType()) {
        auto type = getILType(expr.getType());

        // TODO: Why only FP and integers are cast here?
        if (!type->equals(value->getType()) && (value->getType()->isFloatingPoint() || value->getType()->isInteger())) {
            return createCast(value, type);
        }
    }

    return value;
}

void IRGenerator::setInsertPoint(BasicBlock* block) {
    insertBlock = block;
}
