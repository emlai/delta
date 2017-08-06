#include <unordered_map>
#include <vector>
#include <memory>
#include <cassert>
#include <llvm/ADT/STLExtras.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/CFG.h>
#include "irgen.h"
#include "../ast/mangle.h"
#include "../ast/expr.h"
#include "../ast/decl.h"
#include "../ast/module.h"
#include "../ast/token.h"
#include "../sema/typecheck.h"
#include "../support/utility.h"

using namespace delta;

namespace {

llvm::LLVMContext ctx;

/// Helper for storing parameter name info in 'functionInstantiations' key strings.
template<typename T>
std::string mangleWithParams(const T& decl, llvm::ArrayRef<Type> typeGenericArgs = {},
                             llvm::ArrayRef<Type> functionGenericArgs = {}) {
    std::string result = mangle(decl, typeGenericArgs, functionGenericArgs);
    for (const ParamDecl& param : decl.getParams()) result.append("$").append(param.name);
    return result;
}

} // anonymous namespace

void Scope::onScopeEnd() {
    for (const Expr* expr : llvm::reverse(deferredExprs)) irGenerator.codegenExpr(*expr);
    for (auto& p : llvm::reverse(deinitsToCall)) irGenerator.createDeinitCall(p.first, p.second);
}

void Scope::clear() {
    deferredExprs.clear();
    deinitsToCall.clear();
}

IRGenerator::IRGenerator()
: builder(ctx), module("", ctx) {
    scopes.push_back(Scope(*this));
}

llvm::Function* IRGenerator::getDeinitializerFor(Type type) {
    std::string mangledName = mangleDeinitDecl(type.getName());
    auto it = functionInstantiations.find(mangledName);
    if (it == functionInstantiations.end()) {
        auto decls = currentTypeChecker->findDecls(mangledName, /*everywhere*/ true);
        if (!decls.empty()) {
            auto& deinitDecl = llvm::cast<DeinitDecl>(*decls[0]);
            auto* deinitializerProto = codegenDeinitializerProto(deinitDecl, type);
            codegenDeinitDecl(deinitDecl, type.getGenericArgs());
            return deinitializerProto;
        }
        return nullptr;
    }
    return it->second.function;
}

/// @param type The Delta type of the variable, or null if the variable is 'this'.
void IRGenerator::setLocalValue(Type type, std::string name, llvm::Value* value) {
    bool wasInserted = scopes.back().localValues.emplace(std::move(name), value).second;
    (void) wasInserted;
    assert(wasInserted);

    if (type && type.isBasicType()) {
        llvm::Function* deinit = getDeinitializerFor(type);
        if (deinit) deferDeinitCall(deinit, value);
    }
}

llvm::Value* IRGenerator::findValue(llvm::StringRef name, const Decl* decl) {
    llvm::Value* value = nullptr;

    for (const auto& scope : llvm::reverse(scopes)) {
        auto it = scope.localValues.find(name);
        if (it == scope.localValues.end()) continue;
        value = it->second;
        break;
    }

    if (!value) {
        assert(decl);
        if (auto fieldDecl = llvm::dyn_cast<FieldDecl>(decl)) {
            return codegenMemberAccess(findValue("this", nullptr), fieldDecl->type, fieldDecl->name);
        }
        codegenDecl(*decl);
        if (auto* varDecl = llvm::dyn_cast<VarDecl>(decl)) {
            name = varDecl->name;
        }
        return globalScope().localValues.find(name)->second;
    }
    return value;
}

static const std::unordered_map<std::string, llvm::Type*> builtinTypes = {
    { "void", llvm::Type::getVoidTy(ctx) },
    { "bool", llvm::Type::getInt1Ty(ctx) },
    { "char", llvm::Type::getInt8Ty(ctx) },
    { "int", llvm::Type::getInt32Ty(ctx) },
    { "int8", llvm::Type::getInt8Ty(ctx) },
    { "int16", llvm::Type::getInt16Ty(ctx) },
    { "int32", llvm::Type::getInt32Ty(ctx) },
    { "int64", llvm::Type::getInt64Ty(ctx) },
    { "uint", llvm::Type::getInt32Ty(ctx) },
    { "uint8", llvm::Type::getInt8Ty(ctx) },
    { "uint16", llvm::Type::getInt16Ty(ctx) },
    { "uint32", llvm::Type::getInt32Ty(ctx) },
    { "uint64", llvm::Type::getInt64Ty(ctx) },
    { "float", llvm::Type::getFloatTy(ctx) },
    { "float32", llvm::Type::getFloatTy(ctx) },
    { "float64", llvm::Type::getDoubleTy(ctx) },
    { "float80", llvm::Type::getX86_FP80Ty(ctx) },
    { "string", llvm::StructType::get(llvm::Type::getInt8PtrTy(ctx),
                                      llvm::Type::getInt32Ty(ctx), nullptr) },
};

llvm::Type* IRGenerator::toIR(Type type) {
    switch (type.getKind()) {
        case TypeKind::BasicType: {
            llvm::StringRef name = type.getName();

            auto builtinType = builtinTypes.find(name);
            if (builtinType != builtinTypes.end()) return builtinType->second;

            auto it = structs.find(name);
            if (it == structs.end()) {
                // Is it a generic parameter?
                auto genericArg = currentGenericArgs.find(name);
                if (genericArg != currentGenericArgs.end()) return genericArg->second;

                auto& decl = currentTypeChecker->findDecl(name, SourceLocation::invalid(),
                                                          /* everywhere */ true);

                // Is it a generic type?
                auto genericArgs = llvm::cast<BasicType>(*type).getGenericArgs();
                if (!genericArgs.empty()) {
                    return codegenGenericTypeInstantiation(llvm::cast<TypeDecl>(decl), genericArgs);
                }

                // Custom type that has not been defined yet.
                codegenTypeDecl(llvm::cast<TypeDecl>(decl));
                it = structs.find(name);
            }
            return it->second.first;
        }
        case TypeKind::ArrayType:
            assert(type.getArraySize() != ArrayType::unsized && "unimplemented");
            return llvm::ArrayType::get(toIR(type.getElementType()), type.getArraySize());
        case TypeKind::RangeType:
            llvm_unreachable("IRGen doesn't support range types yet");
        case TypeKind::TupleType:
            llvm_unreachable("IRGen doesn't support tuple types yet");
        case TypeKind::FunctionType:
            return llvm::IntegerType::getInt8Ty(ctx)->getPointerTo(); // FIXME: Temporary.
        case TypeKind::PointerType: {
            if (type.getPointee().isUnsizedArrayType())
                return llvm::StructType::get(toIR(type.getPointee().getElementType())->getPointerTo(),
                                             llvm::Type::getInt32Ty(ctx), NULL);
            auto* pointeeType = toIR(type.getPointee());
            if (!pointeeType->isVoidTy()) return llvm::PointerType::get(pointeeType, 0);
            else return llvm::PointerType::get(llvm::Type::getInt8Ty(ctx), 0);
        }
    }
    llvm_unreachable("all cases handled");
}

llvm::Value* IRGenerator::codegenVarExpr(const VarExpr& expr) {
    auto* value = findValue(expr.identifier, expr.getDecl());

    if (llvm::isa<llvm::AllocaInst>(value) || llvm::isa<llvm::GlobalValue>(value) ||
        llvm::isa<llvm::GetElementPtrInst>(value)) {
        return builder.CreateLoad(value, expr.identifier);
    } else {
        return value;
    }
}

llvm::Value* IRGenerator::codegenLvalueVarExpr(const VarExpr& expr) {
    return findValue(expr.identifier, expr.getDecl());
}

llvm::Value* IRGenerator::codegenStringLiteralExpr(const StringLiteralExpr& expr) {
    if (expr.getType().isString()) {
        assert(builder.GetInsertBlock() && "CreateGlobalStringPtr requires block to insert into");
        auto* stringPtr = builder.CreateGlobalStringPtr(expr.value);
        auto* string = builder.CreateInsertValue(llvm::UndefValue::get(toIR(Type::getString())),
                                                 stringPtr, 0);
        auto* size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), expr.value.size());
        return builder.CreateInsertValue(string, size, 1);
    } else {
        // Passing as C-string, i.e. char pointer.
        return builder.CreateGlobalStringPtr(expr.value);
    }
}

llvm::Value* IRGenerator::codegenIntLiteralExpr(const IntLiteralExpr& expr) {
    // Integer literals may be typed as floating-point when used in a context
    // that requires a floating-point value. It might make sense to combine
    // IntLiteralExpr and FloatLiteralExpr into a single class.
    if (expr.getType().isFloatingPoint())
        return llvm::ConstantFP::get(toIR(expr.getType()), expr.value);

    return llvm::ConstantInt::getSigned(toIR(expr.getType()), expr.value);
}

llvm::Value* IRGenerator::codegenFloatLiteralExpr(const FloatLiteralExpr& expr) {
    return llvm::ConstantFP::get(toIR(expr.getType()), expr.value);
}

llvm::Value* IRGenerator::codegenBoolLiteralExpr(const BoolLiteralExpr& expr) {
    return expr.value ? llvm::ConstantInt::getTrue(ctx) : llvm::ConstantInt::getFalse(ctx);
}

llvm::Value* IRGenerator::codegenNullLiteralExpr(const NullLiteralExpr& expr) {
    if (expr.getType().getPointee().isUnsizedArrayType()) {
        return llvm::ConstantStruct::getAnon({
            llvm::ConstantPointerNull::get(toIR(expr.getType().getPointee().getElementType())->getPointerTo()),
            llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(ctx), 0)
        });
    }
    return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(toIR(expr.getType())));
}

llvm::Value* IRGenerator::codegenArrayLiteralExpr(const ArrayLiteralExpr& expr) {
    auto* arrayType = llvm::ArrayType::get(toIR(expr.elements[0]->getType()), expr.elements.size());
    std::vector<llvm::Constant*> values;
    values.reserve(expr.elements.size());
    for (auto& e : expr.elements)
        values.emplace_back(llvm::cast<llvm::Constant>(codegenExpr(*e)));
    return llvm::ConstantArray::get(arrayType, values);
}

llvm::Value* IRGenerator::codegenNot(const PrefixExpr& expr) {
    return builder.CreateNot(codegenExpr(expr.getOperand()), "");
}

llvm::Value* IRGenerator::codegenPrefixExpr(const PrefixExpr& expr) {
    switch (expr.op) {
        case PLUS: return codegenExpr(expr.getOperand());
        case MINUS:
            if (expr.getOperand().getType().isFloatingPoint()) {
                return builder.CreateFNeg(codegenExpr(expr.getOperand()));
            } else {
                return builder.CreateNeg(codegenExpr(expr.getOperand()));
            }
        case STAR: return builder.CreateLoad(codegenExpr(expr.getOperand()));
        case AND: return codegenLvalueExpr(expr.getOperand());
        case NOT: return codegenNot(expr);
        case COMPL: return codegenNot(expr);
        default: llvm_unreachable("invalid prefix operator");
    }
}

llvm::Value* IRGenerator::codegenLvaluePrefixExpr(const PrefixExpr& expr) {
    switch (expr.op) {
        case STAR: return codegenExpr(expr.getOperand());
        default: llvm_unreachable("invalid lvalue prefix operator");
    }
}

llvm::Value* IRGenerator::codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs, BinaryCreate0 create) {
    return (builder.*create)(lhs, rhs, "");
}

llvm::Value* IRGenerator::codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs, BinaryCreate1 create) {
    return (builder.*create)(lhs, rhs, "", false);
}

llvm::Value* IRGenerator::codegenBinaryOp(llvm::Value* lhs, llvm::Value* rhs, BinaryCreate2 create) {
    return (builder.*create)(lhs, rhs, "", false, false);
}

llvm::Value* IRGenerator::codegenLogicalAnd(const Expr& left, const Expr& right) {
    auto* lhsBlock = builder.GetInsertBlock();
    auto* rhsBlock = llvm::BasicBlock::Create(ctx, "andRHS", builder.GetInsertBlock()->getParent());
    auto* endBlock = llvm::BasicBlock::Create(ctx, "andEnd", builder.GetInsertBlock()->getParent());

    llvm::Value* lhs = codegenExpr(left);
    builder.CreateCondBr(lhs, rhsBlock, endBlock);

    builder.SetInsertPoint(rhsBlock);
    llvm::Value* rhs = codegenExpr(right);
    builder.CreateBr(endBlock);

    builder.SetInsertPoint(endBlock);
    llvm::PHINode* phi = builder.CreatePHI(llvm::IntegerType::getInt1Ty(ctx), 2, "and");
    phi->addIncoming(lhs, lhsBlock);
    phi->addIncoming(rhs, rhsBlock);
    return phi;
}

llvm::Value* IRGenerator::codegenLogicalOr(const Expr& left, const Expr& right) {
    auto* lhsBlock = builder.GetInsertBlock();
    auto* rhsBlock = llvm::BasicBlock::Create(ctx, "orRHS", builder.GetInsertBlock()->getParent());
    auto* endBlock = llvm::BasicBlock::Create(ctx, "orEnd", builder.GetInsertBlock()->getParent());

    llvm::Value* lhs = codegenExpr(left);
    builder.CreateCondBr(lhs, endBlock, rhsBlock);

    builder.SetInsertPoint(rhsBlock);
    llvm::Value* rhs = codegenExpr(right);
    builder.CreateBr(endBlock);

    builder.SetInsertPoint(endBlock);
    llvm::PHINode* phi = builder.CreatePHI(llvm::IntegerType::getInt1Ty(ctx), 2, "or");
    phi->addIncoming(lhs, lhsBlock);
    phi->addIncoming(rhs, rhsBlock);
    return phi;
}

llvm::Value* IRGenerator::codegenBinaryOp(BinaryOperator op, llvm::Value* lhs, llvm::Value* rhs, const Expr& leftExpr) {
    if (lhs->getType()->isFloatingPointTy()) {
        switch (op) {
            case EQ:    return builder.CreateFCmpOEQ(lhs, rhs);
            case NE:    return builder.CreateFCmpONE(lhs, rhs);
            case LT:    return builder.CreateFCmpOLT(lhs, rhs);
            case LE:    return builder.CreateFCmpOLE(lhs, rhs);
            case GT:    return builder.CreateFCmpOGT(lhs, rhs);
            case GE:    return builder.CreateFCmpOGE(lhs, rhs);
            case PLUS:  return builder.CreateFAdd(lhs, rhs);
            case MINUS: return builder.CreateFSub(lhs, rhs);
            case STAR:  return builder.CreateFMul(lhs, rhs);
            case SLASH: return builder.CreateFDiv(lhs, rhs);
            case MOD:   return builder.CreateFRem(lhs, rhs);
            default:    llvm_unreachable("all cases handled");
        }
    }

    switch (op) {
        case EQ:    return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateICmpEQ);
        case NE:    return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateICmpNE);
        case LT:    return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateICmpSLT :
                                           &llvm::IRBuilder<>::CreateICmpULT);
        case LE:    return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateICmpSLE :
                                           &llvm::IRBuilder<>::CreateICmpULE);
        case GT:    return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateICmpSGT :
                                           &llvm::IRBuilder<>::CreateICmpUGT);
        case GE:    return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateICmpSGE :
                                           &llvm::IRBuilder<>::CreateICmpUGE);
        case PLUS:  return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateAdd);
        case MINUS: return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateSub);
        case STAR:  return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateMul);
        case SLASH: return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateSDiv :
                                           &llvm::IRBuilder<>::CreateUDiv);
        case MOD:   return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           &llvm::IRBuilder<>::CreateSRem :
                                           &llvm::IRBuilder<>::CreateURem);
        case AND:   return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateAnd);
        case OR:    return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateOr);
        case XOR:   return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateXor);
        case LSHIFT:return codegenBinaryOp(lhs, rhs, &llvm::IRBuilder<>::CreateShl);
        case RSHIFT:return codegenBinaryOp(lhs, rhs, leftExpr.getType().isSigned() ?
                                           (BinaryCreate1) &llvm::IRBuilder<>::CreateAShr :
                                           (BinaryCreate1) &llvm::IRBuilder<>::CreateLShr);
        default:    llvm_unreachable("all cases handled");
    }
}

llvm::Value* IRGenerator::codegenShortCircuitBinaryOp(BinaryOperator op, const Expr& lhs, const Expr& rhs) {
    switch (op) {
        case AND_AND: return codegenLogicalAnd(lhs, rhs);
        case OR_OR:   return codegenLogicalOr(lhs, rhs);
        default:      llvm_unreachable("invalid short-circuit binary operator");
    }
}

llvm::Value* IRGenerator::codegenBinaryExpr(const BinaryExpr& expr) {
    if (!expr.isBuiltinOp()) return codegenCallExpr((const CallExpr&) expr);

    assert(expr.getLHS().getType().isImplicitlyConvertibleTo(expr.getRHS().getType())
        || expr.getRHS().getType().isImplicitlyConvertibleTo(expr.getLHS().getType()));

    switch (expr.op) {
        case AND_AND: case OR_OR:
            return codegenShortCircuitBinaryOp(expr.op, expr.getLHS(), expr.getRHS());
        default:
            llvm::Value* lhs = codegenExpr(expr.getLHS());
            llvm::Value* rhs = codegenExpr(expr.getRHS());
            return codegenBinaryOp(expr.op, lhs, rhs, expr.getLHS());
    }
}

bool isSizedArrayToUnsizedArrayRefConversion(Type sourceType, llvm::Type* targetType) {
    return sourceType.isPointerType() && sourceType.getPointee().isSizedArrayType()
        && targetType->isStructTy() && targetType->getStructNumElements() == 2
        && targetType->getStructElementType(0)->isPointerTy()
        && targetType->getStructElementType(1)->isIntegerTy(32);
}

llvm::Value* IRGenerator::codegenExprForPassing(const Expr& expr, llvm::Type* targetType,
                                                bool forceByReference) {
    if (targetType && isSizedArrayToUnsizedArrayRefConversion(expr.getType(), targetType)) {
        assert(expr.getType().getPointee().getArraySize() != ArrayType::unsized);
        auto* elementPtr = builder.CreateConstGEP2_32(nullptr, codegenExpr(expr), 0, 0);
        auto* arrayRef = builder.CreateInsertValue(llvm::UndefValue::get(targetType),
                                                   elementPtr, 0);
        auto size = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx),
                                           expr.getType().getPointee().getArraySize());
        return builder.CreateInsertValue(arrayRef, size, 1);
    }

    Type exprType = expr.getType();
    if (exprType.isPointerType()) exprType = exprType.getPointee();

    if (expr.isRvalue() || !exprType.isBasicType())
        return codegenExpr(expr);

    auto it = structs.find(exprType.getName());
    if ((it == structs.end() || it->second.second->passByValue()) && !forceByReference) {
        if (expr.getType().isPointerType() && targetType && !targetType->isPointerTy()) {
            return builder.CreateLoad(codegenExpr(expr));
        }
    } else if (!expr.getType().isPointerType()) {
        return codegenLvalueExpr(expr);
    }
    return codegenExpr(expr);
}

llvm::Value* IRGenerator::codegenBuiltinConversion(const Expr& expr, Type type) {
    if (expr.getType().isUnsigned() && type.isInteger()) {
        return builder.CreateZExtOrTrunc(codegenExpr(expr), toIR(type));
    } else if (expr.getType().isSigned() && type.isInteger()) {
        return builder.CreateSExtOrTrunc(codegenExpr(expr), toIR(type));
    } else if (expr.getType().isFloatingPoint()) {
        if (type.isSigned()) return builder.CreateFPToSI(codegenExpr(expr), toIR(type));
        if (type.isUnsigned()) return builder.CreateFPToUI(codegenExpr(expr), toIR(type));
        if (type.isFloatingPoint()) return builder.CreateFPCast(codegenExpr(expr), toIR(type));
    } else if (type.isFloatingPoint()) {
        if (expr.getType().isSigned()) return builder.CreateSIToFP(codegenExpr(expr), toIR(type));
        if (expr.getType().isUnsigned()) return builder.CreateUIToFP(codegenExpr(expr), toIR(type));
    }
    error(expr.getLocation(), "conversion from '", expr.getType(), "' to '", type, "' not supported");
}

llvm::Value* IRGenerator::codegenCallExpr(const CallExpr& expr) {
    if (expr.isBuiltinConversion())
        return codegenBuiltinConversion(*expr.args.front().value, expr.getType());

    if (expr.getFunctionName() == "sizeOf") {
        return llvm::ConstantExpr::getSizeOf(toIR(expr.getGenericArgs().front()));
    } else if (expr.getFunctionName() == "offsetUnsafely") {
        return codegenOffsetUnsafely(expr);
    }

    llvm::Function* function = getFunctionForCall(expr);
    assert(function);
    auto param = function->arg_begin();
    llvm::SmallVector<llvm::Value*, 16> args;

    auto* calleeDecl = expr.getCalleeDecl();

    if ((calleeDecl && ((calleeDecl->isFunctionDecl() && llvm::cast<FunctionDecl>(calleeDecl)->isMethodDecl()) ||
                        calleeDecl->isDeinitDecl())) || (!calleeDecl && function->getName() == "offsetUnsafely")) {
        bool forceByReference = calleeDecl && calleeDecl->isFunctionDecl() && llvm::cast<FunctionDecl>(calleeDecl)->isMutating();

        if (expr.getReceiver()) {
            args.emplace_back(codegenExprForPassing(*expr.getReceiver(), param->getType(), forceByReference));
        } else {
            auto* thisValue = findValue("this", nullptr);
            if (thisValue->getType()->isPointerTy() && !param->getType()->isPointerTy()) {
                thisValue = builder.CreateLoad(thisValue, thisValue->getName());
            }
            args.emplace_back(thisValue);
        }
        ++param;
    }

    for (const auto& arg : expr.args) {
        auto* paramType = param != function->arg_end() ? param++->getType() : nullptr;
        args.emplace_back(codegenExprForPassing(*arg.value, paramType));
    }

    return builder.CreateCall(function, args);
}

llvm::Value* IRGenerator::codegenCastExpr(const CastExpr& expr) {
    auto* value = codegenExpr(*expr.expr);
    auto* type = toIR(expr.type);
    if (value->getType()->isIntegerTy() && type->isIntegerTy()) {
        return builder.CreateIntCast(value, type, expr.expr->getType().isSigned());
    }
    if (expr.expr->getType().isPointerType() && expr.expr->getType().getPointee().isUnsizedArrayType()) {
        value = builder.CreateExtractValue(value, 0);
    }
    return builder.CreateBitOrPointerCast(value, type);
}

llvm::Value* IRGenerator::codegenMemberAccess(llvm::Value* baseValue, Type memberType, llvm::StringRef memberName) {
    auto baseType = baseValue->getType();
    if (baseType->isPointerTy()) {
        baseType = baseType->getPointerElementType();
        if (baseType->isPointerTy()) {
            baseType = baseType->getPointerElementType();
            baseValue = builder.CreateLoad(baseValue);
        }
        auto& baseTypeDecl = *structs.find(baseType->getStructName())->second.second;
        auto index = baseTypeDecl.isUnion() ? 0 : baseTypeDecl.getFieldIndex(memberName);
        auto* gep = builder.CreateStructGEP(nullptr, baseValue, index);
        if (baseTypeDecl.isUnion()) {
            return builder.CreateBitCast(gep, toIR(memberType)->getPointerTo(), memberName);
        }
        return gep;
    } else {
        auto& baseTypeDecl = *structs.find(baseType->getStructName())->second.second;
        auto index = baseTypeDecl.isUnion() ? 0 : baseTypeDecl.getFieldIndex(memberName);
        return builder.CreateExtractValue(baseValue, index);
    }
}

llvm::Value* IRGenerator::getArrayOrStringDataPointer(const Expr& object, Type objectType) {
    if (objectType.isUnsizedArrayType() || objectType.isString()) {
        return builder.CreateExtractValue(codegenExpr(object), 0, "data");
    } else {
        llvm::Value* objectValue = codegenExpr(object);
        if (objectValue->getType()->isPointerTy()) {
            auto* zeroConstant = llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0);
            return builder.CreateGEP(objectValue, { zeroConstant, zeroConstant });
        } else {
            auto* alloca = createEntryBlockAlloca(objectType);
            builder.CreateStore(objectValue, alloca);
            return alloca;
        }
    }
}

llvm::Value* IRGenerator::getArrayOrStringLength(const Expr& object, Type objectType) {
    if (objectType.isUnsizedArrayType() || objectType.isString()) {
        return builder.CreateExtractValue(codegenExpr(object), 1, "count");
    } else {
        return llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), objectType.getArraySize());
    }
}

llvm::Value* IRGenerator::codegenOffsetUnsafely(const CallExpr& call) {
    auto* pointer = codegenExpr(*call.getReceiver());
    auto* offset = codegenExpr(*call.args.front().value);
    return builder.CreateGEP(pointer, offset);
}

llvm::Value* IRGenerator::codegenLvalueMemberExpr(const MemberExpr& expr) {
    return codegenMemberAccess(codegenLvalueExpr(*expr.base), expr.getType(), expr.member);
}

llvm::Value* IRGenerator::codegenMemberExpr(const MemberExpr& expr) {
    Type baseType = expr.base->getType();
    if (baseType.isReference()) baseType = baseType.getPointee();
    if (baseType.isArrayType() || baseType.isString()) {
        if (expr.member == "data") return getArrayOrStringDataPointer(*expr.base, baseType);
        if (expr.member == "count") return getArrayOrStringLength(*expr.base, baseType);
    }
    auto* value = codegenLvalueMemberExpr(expr);
    return value->getType()->isPointerTy() ? builder.CreateLoad(value) : value;
}

llvm::Value* IRGenerator::codegenLvalueSubscriptExpr(const SubscriptExpr& expr) {
    auto* value = codegenLvalueExpr(*expr.array);
    Type lhsType = expr.array->getType();

    if (lhsType.isPointerType() && lhsType.getPointee().isUnsizedArrayType()) {
        if (value->getType()->isPointerTy()) {
            value = builder.CreateLoad(value);
        }
        return builder.CreateGEP(builder.CreateExtractValue(value, 0), codegenExpr(*expr.index));
    }
    if (value->getType()->getPointerElementType()->isPointerTy()) value = builder.CreateLoad(value);
    return builder.CreateGEP(value,
                             {llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0), codegenExpr(*expr.index)});
}

llvm::Value* IRGenerator::codegenSubscriptExpr(const SubscriptExpr& expr) {
    return builder.CreateLoad(codegenLvalueSubscriptExpr(expr));
}

llvm::Value* IRGenerator::codegenUnwrapExpr(const UnwrapExpr& expr) {
    // TODO: Assert that the operand is non-null.
    return codegenExpr(*expr.operand);
}

llvm::Value* IRGenerator::codegenExpr(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::VarExpr: return codegenVarExpr(llvm::cast<VarExpr>(expr));
        case ExprKind::StringLiteralExpr: return codegenStringLiteralExpr(llvm::cast<StringLiteralExpr>(expr));
        case ExprKind::IntLiteralExpr: return codegenIntLiteralExpr(llvm::cast<IntLiteralExpr>(expr));
        case ExprKind::FloatLiteralExpr: return codegenFloatLiteralExpr(llvm::cast<FloatLiteralExpr>(expr));
        case ExprKind::BoolLiteralExpr: return codegenBoolLiteralExpr(llvm::cast<BoolLiteralExpr>(expr));
        case ExprKind::NullLiteralExpr: return codegenNullLiteralExpr(llvm::cast<NullLiteralExpr>(expr));
        case ExprKind::ArrayLiteralExpr: return codegenArrayLiteralExpr(llvm::cast<ArrayLiteralExpr>(expr));
        case ExprKind::PrefixExpr: return codegenPrefixExpr(llvm::cast<PrefixExpr>(expr));
        case ExprKind::BinaryExpr: return codegenBinaryExpr(llvm::cast<BinaryExpr>(expr));
        case ExprKind::CallExpr: return codegenCallExpr(llvm::cast<CallExpr>(expr));
        case ExprKind::CastExpr: return codegenCastExpr(llvm::cast<CastExpr>(expr));
        case ExprKind::MemberExpr: return codegenMemberExpr(llvm::cast<MemberExpr>(expr));
        case ExprKind::SubscriptExpr: return codegenSubscriptExpr(llvm::cast<SubscriptExpr>(expr));
        case ExprKind::UnwrapExpr: return codegenUnwrapExpr(llvm::cast<UnwrapExpr>(expr));
    }
    llvm_unreachable("all cases handled");
}

llvm::Value* IRGenerator::codegenLvalueExpr(const Expr& expr) {
    switch (expr.getKind()) {
        case ExprKind::VarExpr: return codegenLvalueVarExpr(llvm::cast<VarExpr>(expr));
        case ExprKind::StringLiteralExpr: llvm_unreachable("no lvalue string literals");
        case ExprKind::IntLiteralExpr: llvm_unreachable("no lvalue integer literals");
        case ExprKind::FloatLiteralExpr: llvm_unreachable("no lvalue float literals");
        case ExprKind::BoolLiteralExpr: llvm_unreachable("no lvalue boolean literals");
        case ExprKind::NullLiteralExpr: llvm_unreachable("no lvalue null literals");
        case ExprKind::ArrayLiteralExpr: llvm_unreachable("no lvalue array literals");
        case ExprKind::PrefixExpr: return codegenLvaluePrefixExpr(llvm::cast<PrefixExpr>(expr));
        case ExprKind::BinaryExpr: llvm_unreachable("no lvalue binary expressions");
        case ExprKind::CallExpr: llvm_unreachable("IRGen doesn't support lvalue call expressions yet");
        case ExprKind::CastExpr: llvm_unreachable("IRGen doesn't support lvalue cast expressions yet");
        case ExprKind::MemberExpr: return codegenLvalueMemberExpr(llvm::cast<MemberExpr>(expr));
        case ExprKind::SubscriptExpr: return codegenLvalueSubscriptExpr(llvm::cast<SubscriptExpr>(expr));
        case ExprKind::UnwrapExpr: return codegenUnwrapExpr(llvm::cast<UnwrapExpr>(expr));
    }
    llvm_unreachable("all cases handled");
}

void IRGenerator::beginScope() {
    scopes.push_back(Scope(*this));
}

void IRGenerator::endScope() {
    scopes.back().onScopeEnd();
    scopes.pop_back();
}

void IRGenerator::deferEvaluationOf(const Expr& expr) {
    scopes.back().deferredExprs.emplace_back(&expr);
}

void IRGenerator::deferDeinitCall(llvm::Function* deinit, llvm::Value* valueToDeinit) {
    scopes.back().deinitsToCall.emplace_back(deinit, valueToDeinit);
}

void IRGenerator::codegenDeferredExprsAndDeinitCallsForReturn() {
    for (auto& scope : llvm::reverse(scopes)) scope.onScopeEnd();
    scopes.back().clear();
}

void IRGenerator::codegenReturnStmt(const ReturnStmt& stmt) {
    assert(stmt.values.size() < 2 && "IRGen doesn't support multiple return values yet");

    codegenDeferredExprsAndDeinitCallsForReturn();

    if (stmt.values.empty()) {
        if (llvm::cast<FunctionDecl>(currentDecl)->getName() != "main") builder.CreateRetVoid();
        else builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0));
    } else {
        builder.CreateRet(codegenExpr(*stmt.values[0]));
    }
}

llvm::AllocaInst* IRGenerator::createEntryBlockAlloca(Type type, llvm::Value* arraySize,
                                                      const llvm::Twine& name) {
    static llvm::BasicBlock::iterator lastAlloca;
    auto* insertBlock = builder.GetInsertBlock();
    auto* entryBlock = &insertBlock->getParent()->getEntryBlock();

    if (lastAlloca == llvm::BasicBlock::iterator() || lastAlloca->getParent() != entryBlock) {
        if (entryBlock->empty()) {
            builder.SetInsertPoint(entryBlock);
        } else {
            builder.SetInsertPoint(&entryBlock->front());
        }
    } else {
        builder.SetInsertPoint(entryBlock, std::next(lastAlloca));
    }

    auto* alloca = builder.CreateAlloca(toIR(type), arraySize, name);
    lastAlloca = alloca->getIterator();
    setLocalValue(type, name.str(), alloca);
    builder.SetInsertPoint(insertBlock);
    return alloca;
}

void IRGenerator::codegenVarStmt(const VarStmt& stmt) {
    auto* alloca = createEntryBlockAlloca(stmt.decl->getType(), nullptr, stmt.decl->name);
    if (auto initializer = stmt.decl->initializer) {
        builder.CreateStore(codegenExprForPassing(*initializer, alloca->getAllocatedType()), alloca);
    }
}

void IRGenerator::codegenIncrementStmt(const IncrementStmt& stmt) {
    auto* alloca = codegenLvalueExpr(*stmt.operand);
    auto* value = builder.CreateLoad(alloca);
    auto* result = builder.CreateAdd(value, llvm::ConstantInt::get(value->getType(), 1));
    builder.CreateStore(result, alloca);
}

void IRGenerator::codegenDecrementStmt(const DecrementStmt& stmt) {
    auto* alloca = codegenLvalueExpr(*stmt.operand);
    auto* value = builder.CreateLoad(alloca);
    auto* result = builder.CreateSub(value, llvm::ConstantInt::get(value->getType(), 1));
    builder.CreateStore(result, alloca);
}

void IRGenerator::codegenBlock(llvm::ArrayRef<std::unique_ptr<Stmt>> stmts,
                               llvm::BasicBlock* destination, llvm::BasicBlock* continuation) {
    builder.SetInsertPoint(destination);

    beginScope();
    for (const auto& stmt : stmts) {
        codegenStmt(*stmt);
        if (stmt->isReturnStmt() || stmt->isBreakStmt()) break;
    }
    endScope();

    llvm::BasicBlock* insertBlock = builder.GetInsertBlock();
    if (insertBlock->empty() || (!llvm::isa<llvm::ReturnInst>(insertBlock->back())
                              && !llvm::isa<llvm::BranchInst>(insertBlock->back())))
        builder.CreateBr(continuation);
}

void IRGenerator::codegenIfStmt(const IfStmt& ifStmt) {
    auto* condition = codegenExpr(*ifStmt.condition);
    auto* function = builder.GetInsertBlock()->getParent();
    auto* thenBlock = llvm::BasicBlock::Create(ctx, "then", function);
    auto* elseBlock = llvm::BasicBlock::Create(ctx, "else", function);
    auto* endIfBlock = llvm::BasicBlock::Create(ctx, "endif", function);
    builder.CreateCondBr(condition, thenBlock, elseBlock);
    codegenBlock(ifStmt.thenBody, thenBlock, endIfBlock);
    codegenBlock(ifStmt.elseBody, elseBlock, endIfBlock);
    builder.SetInsertPoint(endIfBlock);
}

void IRGenerator::codegenSwitchStmt(const SwitchStmt& switchStmt) {
    auto* condition = codegenExpr(*switchStmt.condition);
    auto* function = builder.GetInsertBlock()->getParent();
    auto* insertBlockBackup = builder.GetInsertBlock();

    std::vector<std::pair<llvm::ConstantInt*, llvm::BasicBlock*>> cases;
    for (const SwitchCase& switchCase : switchStmt.cases) {
        auto* value = llvm::cast<llvm::ConstantInt>(codegenExpr(*switchCase.value));
        auto* block = llvm::BasicBlock::Create(ctx, "", function);
        cases.emplace_back(value, block);
    }

    builder.SetInsertPoint(insertBlockBackup);
    auto* defaultBlock = llvm::BasicBlock::Create(ctx, "default", function);
    auto* end = llvm::BasicBlock::Create(ctx, "endswitch", function);
    breakTargets.push_back(end);
    auto* switchInst = builder.CreateSwitch(condition, defaultBlock);

    auto casesIterator = cases.begin();
    for (const SwitchCase& switchCase : switchStmt.cases) {
        auto* value = casesIterator->first;
        auto* block = casesIterator->second;
        codegenBlock(switchCase.stmts, block, end);
        switchInst->addCase(value, block);
        ++casesIterator;
    }

    codegenBlock(switchStmt.defaultStmts, defaultBlock, end);
    breakTargets.pop_back();
    builder.SetInsertPoint(end);
}

void IRGenerator::codegenWhileStmt(const WhileStmt& whileStmt) {
    auto* function = builder.GetInsertBlock()->getParent();
    auto* condition = llvm::BasicBlock::Create(ctx, "while", function);
    auto* body = llvm::BasicBlock::Create(ctx, "body", function);
    auto* end = llvm::BasicBlock::Create(ctx, "endwhile", function);
    breakTargets.push_back(end);
    builder.CreateBr(condition);

    builder.SetInsertPoint(condition);
    builder.CreateCondBr(codegenExpr(*whileStmt.condition), body, end);
    codegenBlock(whileStmt.body, body, condition);

    breakTargets.pop_back();
    builder.SetInsertPoint(end);
}

// This transforms 'for (id in x...y) { ... }' (where 'x' and 'y' are integers) into:
//
//  var counter = x;
//  while (counter <= y) {
//      const id = counter;
//      ...
//      counter++;
//  }
void IRGenerator::codegenForStmt(const ForStmt& forStmt) {
    if (!forStmt.range->getType().isRangeType()) {
        error(forStmt.range->getLocation(),
              "IRGen doesn't support 'for'-loops over non-range iterables yet");
    }

    if (!forStmt.range->getType().getIterableElementType().isInteger()) {
        error(forStmt.range->getLocation(),
              "IRGen doesn't support 'for'-loops over non-integer ranges yet");
    }

    beginScope();
    auto& range = llvm::cast<BinaryExpr>(*forStmt.range);
    auto* counterAlloca = createEntryBlockAlloca(forStmt.range->getType().getIterableElementType(),
                                                 nullptr, forStmt.id);
    builder.CreateStore(codegenExpr(range.getLHS()), counterAlloca);
    auto* lastValue = codegenExpr(range.getRHS());

    auto* function = builder.GetInsertBlock()->getParent();
    auto* condition = llvm::BasicBlock::Create(ctx, "for", function);
    auto* body = llvm::BasicBlock::Create(ctx, "body", function);
    auto* end = llvm::BasicBlock::Create(ctx, "endfor", function);
    breakTargets.push_back(end);
    builder.CreateBr(condition);

    builder.SetInsertPoint(condition);
    auto* counter = builder.CreateLoad(counterAlloca, forStmt.id);

    llvm::Value* cmp;
    if (llvm::cast<RangeType>(*forStmt.range->getType()).isExclusive()) {
        if (range.getLHS().getType().isSigned())
            cmp = builder.CreateICmpSLT(counter, lastValue);
        else
            cmp = builder.CreateICmpULT(counter, lastValue);
    } else {
        if (range.getLHS().getType().isSigned())
            cmp = builder.CreateICmpSLE(counter, lastValue);
        else
            cmp = builder.CreateICmpULE(counter, lastValue);
    }
    builder.CreateCondBr(cmp, body, end);

    codegenBlock(forStmt.body, body, condition);

    builder.SetInsertPoint(&builder.GetInsertBlock()->back());
    auto* newCounter = builder.CreateAdd(counter, llvm::ConstantInt::get(counter->getType(), 1));
    builder.CreateStore(newCounter, counterAlloca);

    breakTargets.pop_back();
    builder.SetInsertPoint(end);
    endScope();
}

void IRGenerator::codegenBreakStmt(const BreakStmt&) {
    assert(!breakTargets.empty());
    builder.CreateBr(breakTargets.back());
}

void IRGenerator::codegenAssignStmt(const AssignStmt& stmt) {
    auto* lhsLvalue = codegenLvalueExpr(*stmt.lhs);

    if (stmt.isCompoundAssignment()) {
        auto& binaryExpr = llvm::cast<BinaryExpr>(*stmt.rhs);

        switch (binaryExpr.op) {
            case AND_AND: fatalError("'&&=' not implemented yet");
            case OR_OR: fatalError("'||=' not implemented yet");
            default: break;
        }

        auto* lhsValue = builder.CreateLoad(lhsLvalue);
        auto* rhsValue = codegenExpr(binaryExpr.getRHS());
        builder.CreateStore(codegenBinaryOp(binaryExpr.op, lhsValue, rhsValue, *stmt.lhs), lhsLvalue);
    } else {
        builder.CreateStore(codegenExpr(*stmt.rhs), lhsLvalue);
    }
}

void IRGenerator::codegenStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt: codegenReturnStmt(llvm::cast<ReturnStmt>(stmt)); break;
        case StmtKind::VarStmt: codegenVarStmt(llvm::cast<VarStmt>(stmt)); break;
        case StmtKind::IncrementStmt: codegenIncrementStmt(llvm::cast<IncrementStmt>(stmt)); break;
        case StmtKind::DecrementStmt: codegenDecrementStmt(llvm::cast<DecrementStmt>(stmt)); break;
        case StmtKind::ExprStmt: codegenExpr(*llvm::cast<ExprStmt>(stmt).expr); break;
        case StmtKind::DeferStmt: deferEvaluationOf(*llvm::cast<DeferStmt>(stmt).expr); break;
        case StmtKind::IfStmt: codegenIfStmt(llvm::cast<IfStmt>(stmt)); break;
        case StmtKind::SwitchStmt: codegenSwitchStmt(llvm::cast<SwitchStmt>(stmt)); break;
        case StmtKind::WhileStmt: codegenWhileStmt(llvm::cast<WhileStmt>(stmt)); break;
        case StmtKind::ForStmt: codegenForStmt(llvm::cast<ForStmt>(stmt)); break;
        case StmtKind::BreakStmt: codegenBreakStmt(llvm::cast<BreakStmt>(stmt)); break;
        case StmtKind::AssignStmt: codegenAssignStmt(llvm::cast<AssignStmt>(stmt)); break;
    }
}

void IRGenerator::createDeinitCall(llvm::Function* deinit, llvm::Value* valueToDeinit) {
    // Prevent recursively destroying the argument in struct deinitializers.
    if (llvm::isa<llvm::Argument>(valueToDeinit)
        && builder.GetInsertBlock()->getParent()->getName().endswith(".deinit")) return;

    if (valueToDeinit->getType()->isPointerTy() && !deinit->arg_begin()->getType()->isPointerTy()) {
        builder.CreateCall(deinit, builder.CreateLoad(valueToDeinit));
    } else if (!valueToDeinit->getType()->isPointerTy() && deinit->arg_begin()->getType()->isPointerTy()) {
        llvm::errs() << "deinitialization of by-value class parameters not implemented yet\n";
        abort();
    } else {
        builder.CreateCall(deinit, valueToDeinit);
    }
}

llvm::Type* IRGenerator::getLLVMTypeForPassing(llvm::StringRef typeName, bool isMutating) const {
    assert(structs.count(typeName));
    auto structTypeAndDecl = structs.find(typeName)->second;

    if (!isMutating && structTypeAndDecl.second->passByValue()) {
        return structTypeAndDecl.first;
    } else {
        return llvm::PointerType::get(structTypeAndDecl.first, 0);
    }
}

void IRGenerator::setCurrentGenericArgs(llvm::ArrayRef<GenericParamDecl> genericParams,
                                        llvm::ArrayRef<Type> genericArgs) {
    assert(genericParams.size() == genericArgs.size());
    for (auto tuple : llvm::zip_first(genericParams, genericArgs)) {
        currentGenericArgs.emplace(std::get<0>(tuple).name, toIR(std::get<1>(tuple)));
    }
}

llvm::Function* IRGenerator::getFunctionProto(const FunctionLikeDecl& decl,
                                              llvm::ArrayRef<Type> functionGenericArgs,
                                              Type receiverType, std::string&& mangledName) {
    llvm::ArrayRef<Type> receiverTypeGenericArgs;
    if (receiverType) {
        receiverTypeGenericArgs = llvm::cast<BasicType>(*receiverType.removePointer()).getGenericArgs();
    }

    auto it = functionInstantiations.find(mangleWithParams(decl, receiverTypeGenericArgs, functionGenericArgs));
    if (it != functionInstantiations.end()) return it->second.function;

    auto previousGenericArgs = std::move(currentGenericArgs);
    setCurrentGenericArgs(decl.getGenericParams(), functionGenericArgs);

    auto* functionType = decl.getFunctionType();
    llvm::SmallVector<llvm::Type*, 16> paramTypes;

    if (decl.isMethodDecl() || decl.isDeinitDecl()) {
        auto* receiverTypeDecl = decl.getTypeDecl();
        std::string receiverTypeName;

        if (receiverTypeDecl->isGeneric()) {
            receiverTypeName = mangle(*receiverTypeDecl, receiverTypeGenericArgs);
            if (structs.count(receiverTypeName) == 0) {
                codegenGenericTypeInstantiation(*receiverTypeDecl, receiverTypeGenericArgs);
            }
            mangledName = mangle(decl, receiverTypeGenericArgs, functionGenericArgs);
            setCurrentGenericArgs(receiverTypeDecl->genericParams, receiverTypeGenericArgs);
        } else {
            receiverTypeName = receiverTypeDecl->name;
        }
        paramTypes.emplace_back(getLLVMTypeForPassing(receiverTypeName, decl.isMutating()));
    }

    for (const auto& t : functionType->paramTypes) paramTypes.emplace_back(toIR(t));

    assert(!functionType->returnType.isTupleType() && "IRGen doesn't support tuple return values yet");
    auto* returnType = toIR(decl.isInitDecl() ? receiverType : functionType->returnType);
    if (decl.getName() == "main" && returnType->isVoidTy()) returnType = llvm::Type::getInt32Ty(ctx);

    auto* llvmFunctionType = llvm::FunctionType::get(returnType, paramTypes, decl.isVariadic());
    if (mangledName.empty()) mangledName = mangle(decl, receiverTypeGenericArgs, functionGenericArgs);
    auto* function = llvm::Function::Create(llvmFunctionType, llvm::Function::ExternalLinkage,
                                            mangledName, &module);

    auto arg = function->arg_begin(), argsEnd = function->arg_end();
    if (decl.isMethodDecl() || decl.isDeinitDecl()) arg++->setName("this");
    for (auto param = decl.getParams().begin(); arg != argsEnd; ++param, ++arg) arg->setName(param->name);

    currentGenericArgs = std::move(previousGenericArgs);

    auto mangled = mangleWithParams(decl, receiverTypeGenericArgs, functionGenericArgs);
    FunctionInstantiation functionInstantiation{decl, receiverTypeGenericArgs, functionGenericArgs, function};
    return functionInstantiations.emplace(std::move(mangled),
                                          std::move(functionInstantiation)).first->second.function;
}

llvm::Function* IRGenerator::getInitProto(const InitDecl& decl, llvm::ArrayRef<Type> typeGenericArgs,
                                          llvm::ArrayRef<Type> functionGenericArgs) {
    return getFunctionProto(decl, functionGenericArgs, decl.getTypeDecl()->getType(typeGenericArgs));
}

llvm::Function* IRGenerator::codegenDeinitializerProto(const DeinitDecl& decl, Type receiverType) {
    return getFunctionProto(decl, {}, receiverType);
}

llvm::Function* IRGenerator::getFunctionForCall(const CallExpr& call) {
    if (!call.callsNamedFunction()) fatalError("anonymous function calls not implemented yet");

    const Decl* decl = call.getCalleeDecl();

    if (auto* functionDecl = llvm::dyn_cast<FunctionDecl>(decl)) {
        llvm::Function* function = getFunctionProto(*functionDecl, call.genericArgs, call.getReceiverType());
//        if (function->empty() && !call.getGenericArgs().empty()) {
//            auto backup = builder.GetInsertBlock();
//            codegenInitDecl(*initDecl, call.genericArgs);
//            builder.SetInsertPoint(backup);
//        }
        return function;
    } else if (auto* initDecl = llvm::dyn_cast<InitDecl>(decl)) {
        llvm::Function* function = getInitProto(*initDecl, call.genericArgs);
        if (function->empty() && !call.getGenericArgs().empty()) {
            auto backup = builder.GetInsertBlock();
            codegenInitDecl(*initDecl, call.genericArgs);
            builder.SetInsertPoint(backup);
        }
        return function;
    } else {
        llvm_unreachable("invalid callee decl");
    }
}

void IRGenerator::codegenFunctionBody(const FunctionLikeDecl& decl, llvm::Function& function) {
    builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "", &function));
    beginScope();
    auto arg = function.arg_begin();
    if (decl.getTypeDecl() != nullptr) setLocalValue(nullptr, "this", &*arg++);
    for (const auto& param : decl.getParams()) setLocalValue(param.type, param.name, &*arg++);
    for (const auto& stmt : *decl.body) codegenStmt(*stmt);
    endScope();

    auto* insertBlock = builder.GetInsertBlock();
    if (insertBlock != &function.getEntryBlock() && llvm::pred_empty(insertBlock)) {
        insertBlock->eraseFromParent();
        return;
    }

    if (insertBlock->empty() || !llvm::isa<llvm::ReturnInst>(insertBlock->back())) {
        if (function.getName() != "main") {
            builder.CreateRetVoid();
        } else {
            builder.CreateRet(llvm::ConstantInt::get(llvm::Type::getInt32Ty(ctx), 0));
        }
    }
}

void IRGenerator::codegenFunctionDecl(const FunctionDecl& decl) {
    if (decl.isGeneric()) return;
    if (decl.getTypeDecl() && decl.getTypeDecl()->isGeneric()) return;

    llvm::Function* function = getFunctionProto(decl);
    if (!decl.isExtern()) codegenFunctionBody(decl, *function);
    assert(!llvm::verifyFunction(*function, &llvm::errs()));
}

void IRGenerator::codegenInitDecl(const InitDecl& decl, llvm::ArrayRef<Type> typeGenericArgs) {
    if (decl.getTypeDecl()->isGeneric() && typeGenericArgs.empty()) return;

    llvm::Function* function = getInitProto(decl, typeGenericArgs);

    builder.SetInsertPoint(llvm::BasicBlock::Create(ctx, "", function));

    auto* type = llvm::cast<llvm::StructType>(toIR(decl.getTypeDecl()->getType(typeGenericArgs)));
    auto* alloca = builder.CreateAlloca(type);

    beginScope();
    setLocalValue(nullptr, "this", alloca);
    auto param = decl.getParams().begin();
    for (auto& arg : function->args()) setLocalValue(param++->type, arg.getName(), &arg);
    for (const auto& stmt : *decl.body) codegenStmt(*stmt);
    builder.CreateRet(builder.CreateLoad(alloca));
    endScope();

    assert(!llvm::verifyFunction(*function, &llvm::errs()));
}

void IRGenerator::codegenDeinitDecl(const DeinitDecl& decl, llvm::ArrayRef<Type> typeGenericArgs) {
    if (decl.getTypeDecl()->isGeneric() && typeGenericArgs.empty()) return;

    llvm::Function* function = getFunctionProto(decl, {}, decl.getTypeDecl()->getType(typeGenericArgs));
    codegenFunctionBody(decl, *function);
    assert(!llvm::verifyFunction(*function, &llvm::errs()));
}

std::vector<llvm::Type*> IRGenerator::getFieldTypes(const TypeDecl& decl) {
    std::vector<llvm::Type*> fieldTypes;
    fieldTypes.reserve(decl.fields.size());
    for (const auto& field : decl.fields) {
        fieldTypes.emplace_back(toIR(field.type));
    }
    return fieldTypes;
}

void IRGenerator::codegenTypeDecl(const TypeDecl& decl) {
    if (decl.isGeneric()) return;
    if (structs.count(decl.name)) return;

    if (decl.fields.empty()) {
        structs.emplace(decl.name, std::make_pair(llvm::StructType::get(ctx), &decl));
    } else {
        structs.emplace(decl.name, std::make_pair(llvm::StructType::create(getFieldTypes(decl),
                                                                           decl.name), &decl));
    }

    auto insertBlockBackup = builder.GetInsertBlock();
    auto insertPointBackup = builder.GetInsertPoint();

    for (auto& memberDecl : decl.getMemberDecls()) {
        codegenDecl(*memberDecl);
    }

    if (insertBlockBackup) builder.SetInsertPoint(insertBlockBackup, insertPointBackup);
}

llvm::Type* IRGenerator::codegenGenericTypeInstantiation(const TypeDecl& decl, llvm::ArrayRef<Type> genericArgs) {
    auto name = mangle(decl, genericArgs);

    if (decl.fields.empty()) {
        auto value = std::make_pair(llvm::StructType::get(ctx), &decl);
        return structs.emplace(name, std::move(value)).first->second.first;
    }

    auto previousGenericArgs = std::move(currentGenericArgs);
    setCurrentGenericArgs(decl.genericParams, genericArgs);
    auto elements = getFieldTypes(decl);
    currentGenericArgs = std::move(previousGenericArgs);

    auto value = std::make_pair(llvm::StructType::create(elements, name), &decl);
    return structs.emplace(name, std::move(value)).first->second.first;
}

void IRGenerator::codegenVarDecl(const VarDecl& decl) {
    if (globalScope().localValues.find(decl.name) != globalScope().localValues.end()) return;

    llvm::Value* value = decl.initializer ? codegenExpr(*decl.initializer) : nullptr;

    if (!value || decl.getType().isMutable() /* || decl.isPublic() */) {
        auto linkage = value ? llvm::GlobalValue::PrivateLinkage : llvm::GlobalValue::ExternalLinkage;
        auto initializer = value ? llvm::cast<llvm::Constant>(value) : nullptr;
        value = new llvm::GlobalVariable(module, toIR(decl.getType()), !decl.getType().isMutable(),
                                         linkage, initializer, decl.name);
    }

    globalScope().localValues.emplace(decl.name, value);
}

void IRGenerator::codegenDecl(const Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl: llvm_unreachable("handled via FunctionDecl");
        case DeclKind::FunctionDecl:
        case DeclKind::MethodDecl: codegenFunctionDecl(llvm::cast<FunctionDecl>(decl)); break;
        case DeclKind::GenericParamDecl: llvm_unreachable("cannot codegen generic parameter declaration");
        case DeclKind::InitDecl: codegenInitDecl(llvm::cast<InitDecl>(decl)); break;
        case DeclKind::DeinitDecl: codegenDeinitDecl(llvm::cast<DeinitDecl>(decl)); break;
        case DeclKind::TypeDecl: codegenTypeDecl(llvm::cast<TypeDecl>(decl)); break;
        case DeclKind::VarDecl: codegenVarDecl(llvm::cast<VarDecl>(decl)); break;
        case DeclKind::FieldDecl: llvm_unreachable("handled via TypeDecl");
        case DeclKind::ImportDecl: break;
    }
}

llvm::Module& IRGenerator::compile(const Module& sourceModule) {
    for (const auto& sourceFile : sourceModule.getSourceFiles()) {
        setTypeChecker(TypeChecker(const_cast<Module*>(&sourceModule),
                                   const_cast<SourceFile*>(&sourceFile)));

        for (const auto& decl : sourceFile.getTopLevelDecls()) {
            setCurrentDecl(decl.get());
            codegenDecl(*decl);
        }

        setCurrentDecl(nullptr);
    }

    while (true) {
        auto currentFunctionInstantiations = functionInstantiations;

        for (auto& p : currentFunctionInstantiations) {
            if (p.second.decl.isExtern() || !p.second.function->empty()) continue;

            setTypeChecker(TypeChecker(const_cast<Module*>(&sourceModule), nullptr));
            auto previousGenericArgs = std::move(currentGenericArgs);
            setCurrentGenericArgs(p.second.decl.getGenericParams(), p.second.genericArgs);
            if (p.second.decl.getTypeDecl() != nullptr) {
                auto& receiverTypeDecl = *p.second.decl.getTypeDecl();
                setCurrentGenericArgs(receiverTypeDecl.genericParams,
                                      p.second.receiverTypeGenericArgs);
            }
            codegenFunctionBody(p.second.decl, *p.second.function);
            currentGenericArgs = std::move(previousGenericArgs);
            assert(!llvm::verifyFunction(*p.second.function, &llvm::errs()));
        }

        if (functionInstantiations.size() == currentFunctionInstantiations.size()) break;
    }

    assert(!llvm::verifyModule(module, &llvm::errs()));
    return module;
}

llvm::LLVMContext& irgen::getContext() {
    return ctx;
}
