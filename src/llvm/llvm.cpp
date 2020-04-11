#include "llvm.h"
#pragma warning(push, 0)
#include <llvm/ADT/StringSwitch.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#pragma warning(pop)
#include "../ast/decl.h"
#include "../ir/ir.h"

using namespace delta;

// TODO(ir) rename llvm directory to "codegen" or "backend"? so all backends can be put there?
//  OR put llvm.cpp/h under ir/ and rename ir/ to "codegen" or "backend"?

LLVMGenerator::LLVMGenerator() : builder(ctx) {
    scopes.push_back(LLVMGenScope(*this));
}

void LLVMGenerator::setLocalValue(llvm::Value* value, const IRValue* decl) {
    auto it = scopes.back().valuesByDecl.try_emplace(decl, value);
    ASSERT(it.second);
}

llvm::Value* LLVMGenerator::getValueOrNull(const IRValue* decl) {
    for (auto& scope : llvm::reverse(scopes)) {
        if (auto* value = scope.valuesByDecl.lookup(decl)) {
            return value;
        }
    }

    return nullptr;
}

llvm::Type* LLVMGenerator::getBuiltinType(llvm::StringRef name) {
    return llvm::StringSwitch<llvm::Type*>(name)
        .Case("void", llvm::Type::getVoidTy(ctx))
        .Case("bool", llvm::Type::getInt1Ty(ctx))
        .Case("char", llvm::Type::getInt8Ty(ctx))
        .Case("int", llvm::Type::getInt32Ty(ctx))
        .Case("int8", llvm::Type::getInt8Ty(ctx))
        .Case("int16", llvm::Type::getInt16Ty(ctx))
        .Case("int32", llvm::Type::getInt32Ty(ctx))
        .Case("int64", llvm::Type::getInt64Ty(ctx))
        .Case("uint", llvm::Type::getInt32Ty(ctx))
        .Case("uint8", llvm::Type::getInt8Ty(ctx))
        .Case("uint16", llvm::Type::getInt16Ty(ctx))
        .Case("uint32", llvm::Type::getInt32Ty(ctx))
        .Case("uint64", llvm::Type::getInt64Ty(ctx))
        .Case("float", llvm::Type::getFloatTy(ctx))
        .Case("float32", llvm::Type::getFloatTy(ctx))
        .Case("float64", llvm::Type::getDoubleTy(ctx))
        .Case("float80", llvm::Type::getX86_FP80Ty(ctx))
        .Default(nullptr);
}

llvm::Type* LLVMGenerator::getStructType(IRStructType* type) {
    return codegenTypeDecl(type);
}

llvm::Type* LLVMGenerator::getLLVMType(IRType* type, SourceLocation location) {
    switch (type->kind) {
        case IRTypeKind::IRBasicType:
            if (auto* builtinType = getBuiltinType(type->getName())) {
                return builtinType;
            } else {
                llvm::errs() << type->getName();
                ASSERT(false); // TODO(ir) ?
            }
        case IRTypeKind::IRArrayType: {
            auto arrayType = llvm::cast<IRArrayType>(type);
            return llvm::ArrayType::get(getLLVMType(arrayType->elementType, location), arrayType->size);
        }
        case IRTypeKind::IRFunctionType: {
            auto functionType = llvm::cast<IRFunctionType>(type);
            auto returnType = getLLVMType(functionType->returnType);
            auto paramTypes = map(functionType->paramTypes, [&](IRType* type) { return getLLVMType(type); });
            return llvm::FunctionType::get(returnType, paramTypes, false);
        }
        case IRTypeKind::IRPointerType: {
            auto pointerType = llvm::cast<IRPointerType>(type);
            auto* pointeeType = getLLVMType(pointerType->pointee, location);
            return llvm::PointerType::get(pointeeType->isVoidTy() ? llvm::Type::getInt8Ty(ctx) : pointeeType, 0);
        }
        case IRTypeKind::IRStructType: {
            // TODO(ir) DOING THIS IF CHECK BEFORE GETSTRUCTTYPE FIXES ISSIZED STACK OVERFLOW???
            if (llvm::cast<IRStructType>(type)->name.empty()) {
                auto elementTypes = map(llvm::cast<IRStructType>(type)->elementTypes, [&](IRType* type) { return getLLVMType(type); });
                return llvm::StructType::get(ctx, elementTypes);
            } else {
                return getStructType(llvm::cast<IRStructType>(type));
            }
        }
        case IRTypeKind::IRUnionType: {
            auto unionType = llvm::cast<IRUnionType>(type);

            auto it = structs.find(unionType);
            if (it != structs.end()) return it->second;

            auto structType = unionType->name.empty() ? llvm::StructType::get(ctx) : llvm::StructType::create(ctx, unionType->name);
            structs.try_emplace(unionType, structType);

            unsigned maxSize = 0;
            for (auto* field : unionType->getFields()) {
                if (!field) continue;
                auto size = module->getDataLayout().getTypeAllocSize(getLLVMType(field));
                if (size > maxSize) maxSize = size;
            }

            structType->setBody(llvm::ArrayType::get(llvm::Type::getInt8Ty(ctx), maxSize));
            return structType;
        }
    }

    llvm_unreachable("all cases handled");
}

// TODO(ir): rename all "IRxxx decl"
llvm::Function* LLVMGenerator::getFunctionProto(const IRFunction& decl) {
    if (auto* function = module->getFunction(decl.mangledName)) return function;

    llvm::SmallVector<llvm::Type*, 16> paramTypes;
    for (auto& param : decl.params) {
        paramTypes.emplace_back(getLLVMType(param.type));
    }

    auto* returnType = getLLVMType(decl.returnType);
    auto* functionType = llvm::FunctionType::get(returnType, paramTypes, decl.isVariadic);
    auto* function = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, decl.mangledName, &*module);

    auto arg = function->arg_begin(), argsEnd = function->arg_end();
    ASSERT(decl.params.size() == size_t(std::distance(arg, argsEnd)));
    for (auto param = decl.params.begin(); arg != argsEnd; ++param, ++arg) {
        arg->setName(param->name);
    }

    for (auto& instantiation : functionInstantiations) {
        if (instantiation.function->getName() == decl.mangledName) {
            return function;
        }
    }

    functionInstantiations.push_back({&decl, function});
    return function;
}

// TODO(ir): rename all irfunction decls to "function" or "irfunction" or "intermediatefunction"
// TODO(ir): change params to be pointers, rename decl
void LLVMGenerator::codegenFunctionBody(const IRFunction& decl, llvm::Function& function) {
    llvm::IRBuilder<>::InsertPointGuard insertPointGuard(builder);

    auto arg = function.arg_begin();
    for (auto& param : decl.params) {
        setLocalValue(&*arg++, &param);
    }

    for (auto* block : decl.body) {
        auto basicBlock = getBasicBlock(block);
        basicBlock->insertInto(&function);
        builder.SetInsertPoint(basicBlock);

        for (auto* inst : block->insts) {
            codegenExpr(inst);
        }
    }

    auto insertBlock = builder.GetInsertBlock();
    if (insertBlock && insertBlock != &function.getEntryBlock() && llvm::pred_empty(insertBlock)) {
        insertBlock->eraseFromParent();
    }
}

void LLVMGenerator::codegenFunction(const IRFunction& decl) {
    llvm::Function* function = getFunctionProto(decl);

    if (!decl.isExtern && function->empty()) {
        codegenFunctionBody(decl, *function);
    }

#ifndef NDEBUG
    if (llvm::verifyFunction(*function, &llvm::errs())) {
        llvm::errs() << '\n';
        function->getParent()->print(llvm::errs(), nullptr, false, true);
        llvm::errs() << '\n';
        ASSERT(false && "llvm::verifyFunction failed");
    }
#endif
}

llvm::StructType* LLVMGenerator::codegenTypeDecl(IRStructType* type) {
    ASSERT(type);
    llvm::StructType* structType = nullptr;

    auto it = structs.find(type);
    if (it != structs.end()) {
        structType = it->second;
        ASSERT(structType);
        return structType;
    }

    if (type->elementTypes.empty() || type->name.empty()) {
        structType = llvm::StructType::get(ctx);
    } else {
        structType = llvm::StructType::create(ctx, type->getName());
    }

    structs.try_emplace(type, structType);
    auto fieldTypes = map(type->elementTypes, [&](IRType* t) { return getLLVMType(t); });
    structType->setBody(std::move(fieldTypes));
    return structType;
}

// TODO(ir): Rename "expr" to "value" or "instruction" in llvm/. Rename "codegen" to "build"?
// TODO(ir): add ir tests?

llvm::BasicBlock* LLVMGenerator::getBasicBlock(const IRBasicBlock* block) {
    auto it = generatedBlocks.find(block);
    if (it != generatedBlocks.end()) return it->second;

    auto basicBlock = llvm::BasicBlock::Create(ctx, block->name);
    generatedBlocks.emplace(block, basicBlock);
    return basicBlock;
}

// TODO(ir) rename instruction param
llvm::Value* LLVMGenerator::codegenExpr(const IRValue* instruction) {
    auto it = generatedValues.find(instruction);
    if (it != generatedValues.end()) return it->second;
    auto value = codegenExprUncached(instruction);
    generatedValues.emplace(instruction, value);
    return value;
}

// TODO(ir): find better name
// TODO(ir): refactor cases into functions
llvm::Value* LLVMGenerator::codegenExprUncached(const IRValue* instruction) {
    switch (instruction->kind) {
        // TODO(ir)
        case ValueKind::IRInstruction: {
            auto inst = llvm::cast<IRInstruction>(instruction);
            llvm_unreachable("unhandled IRInstruction");
            // TODO(ir): IRInstruction shouldn't be an IRValue/instruction because these are always unhandled?
            return nullptr;
        }
        case ValueKind::IRAllocaInst: {
            auto inst = llvm::cast<IRAllocaInst>(instruction);
            return builder.CreateAlloca(getLLVMType(inst->allocatedType), nullptr, inst->name);
        }
        case ValueKind::IRReturnInst: {
            auto inst = llvm::cast<IRReturnInst>(instruction);
            if (inst->value) {
                builder.CreateRet(codegenExpr(inst->value));
            } else {
                builder.CreateRetVoid();
            }
            return nullptr;
        }
        case ValueKind::IRBranchInst: {
            auto inst = llvm::cast<IRBranchInst>(instruction);
            builder.CreateBr(getBasicBlock(inst->destination));
            return nullptr;
        }
        case ValueKind::IRConditionalBranchInst: {
            auto inst = llvm::cast<IRConditionalBranchInst>(instruction);
            auto condition = codegenExpr(inst->condition);
            auto trueBlock = getBasicBlock(inst->trueBlock);
            auto falseBlock = getBasicBlock(inst->falseBlock);
            builder.CreateCondBr(condition, trueBlock, falseBlock);
            return nullptr;
        }
        case ValueKind::IRPhiInst: {
            auto inst = llvm::cast<IRPhiInst>(instruction);
            auto type = getLLVMType(inst->valuesAndPredecessors[0].first->getType());
            auto phi = builder.CreatePHI(type, (unsigned) inst->valuesAndPredecessors.size(), inst->name);
            for (auto& p : inst->valuesAndPredecessors) {
                auto value = codegenExpr(p.first);
                auto block = getBasicBlock(p.second);
                phi->addIncoming(value, block);
            }
            return phi;
        }
        case ValueKind::IRSwitchInst: {
            auto inst = llvm::cast<IRSwitchInst>(instruction);
            auto condition = codegenExpr(inst->condition);
            auto cases = map(inst->cases, [&](auto& p) {
                auto value = llvm::cast<llvm::ConstantInt>(codegenExpr(p.first));
                auto block = getBasicBlock(p.second);
                return std::make_pair(value, block);
            });
            auto defaultBlock = getBasicBlock(inst->defaultBlock);
            auto switchInst = builder.CreateSwitch(condition, defaultBlock);
            for (auto& [value, block] : cases) {
                switchInst->addCase(value, block);
            }
            return nullptr;
        }
        case ValueKind::IRLoadInst: {
            auto inst = llvm::cast<IRLoadInst>(instruction);
            return builder.CreateLoad(codegenExpr(inst->value), inst->name);
        }
        case ValueKind::IRStoreInst: {
            auto inst = llvm::cast<IRStoreInst>(instruction);
            auto value = codegenExpr(inst->value);
            auto pointer = codegenExpr(inst->pointer);
            builder.CreateStore(value, pointer);
            return nullptr;
        }
        case ValueKind::IRInsertValueInst: {
            auto inst = llvm::cast<IRInsertValueInst>(instruction);
            auto aggregate = codegenExpr(inst->aggregate);
            auto value = codegenExpr(inst->value);
            return builder.CreateInsertValue(aggregate, value, inst->index);
        }
        case ValueKind::IRExtractValueInst: {
            auto inst = llvm::cast<IRExtractValueInst>(instruction);
            auto aggregate = codegenExpr(inst->aggregate);
            return builder.CreateExtractValue(aggregate, inst->index, inst->name);
        }
        case ValueKind::IRCallInst: {
            auto inst = llvm::cast<IRCallInst>(instruction);
            auto function = codegenExpr(inst->function);
            auto args = map(inst->args, [&](auto* arg) { return codegenExpr(arg); });
            ASSERT(function->getType()->isFunctionTy() || (function->getType()->isPointerTy() && function->getType()->getPointerElementType()->isFunctionTy()));
            return builder.CreateCall(function, args);
        }
        case ValueKind::IRBinaryOp: {
            auto inst = llvm::cast<IRBinaryOp>(instruction);
            auto left = codegenExpr(inst->left);
            auto right = codegenExpr(inst->right);
            auto leftType = inst->left->getType();

            if (leftType->isFloatingPoint()) {
                switch (inst->op) {
                    case Token::Equal:
                        return builder.CreateFCmpOEQ(left, right);
                    case Token::NotEqual:
                        return builder.CreateFCmpONE(left, right);
                    case Token::Less:
                        return builder.CreateFCmpOLT(left, right);
                    case Token::LessOrEqual:
                        return builder.CreateFCmpOLE(left, right);
                    case Token::Greater:
                        return builder.CreateFCmpOGT(left, right);
                    case Token::GreaterOrEqual:
                        return builder.CreateFCmpOGE(left, right);
                    case Token::Plus:
                        return builder.CreateFAdd(left, right);
                    case Token::Minus:
                        return builder.CreateFSub(left, right);
                    case Token::Star:
                        return builder.CreateFMul(left, right);
                    case Token::Slash:
                        return builder.CreateFDiv(left, right);
                    case Token::Modulo:
                        return builder.CreateFRem(left, right);
                    default:
                        llvm_unreachable("invalid binary operation");
                }
            }

            auto isSigned = leftType->isSignedInteger();

            switch (inst->op) {
                case Token::Plus:
                    return builder.CreateAdd(left, right);
                case Token::Minus:
                    return builder.CreateSub(left, right);
                case Token::Star:
                    return builder.CreateMul(left, right);
                case Token::Equal:
                case Token::PointerEqual:
                    return builder.CreateICmpEQ(left, right, inst->name);
                case Token::NotEqual:
                case Token::PointerNotEqual:
                    return builder.CreateICmpNE(left, right);
                case Token::And:
                    return builder.CreateAnd(left, right);
                case Token::Or:
                    return builder.CreateOr(left, right);
                case Token::Xor:
                    return builder.CreateXor(left, right);
                case Token::LeftShift:
                    return builder.CreateShl(left, right);
                case Token::Slash:
                    return isSigned ? builder.CreateSDiv(left, right) : builder.CreateUDiv(left, right);
                case Token::Less:
                    return isSigned ? builder.CreateICmpSLT(left, right) : builder.CreateICmpULT(left, right);
                case Token::LessOrEqual:
                    return isSigned ? builder.CreateICmpSLE(left, right) : builder.CreateICmpULE(left, right);
                case Token::Greater:
                    return isSigned ? builder.CreateICmpSGT(left, right) : builder.CreateICmpUGT(left, right);
                case Token::GreaterOrEqual:
                    return isSigned ? builder.CreateICmpSGE(left, right) : builder.CreateICmpUGE(left, right);
                case Token::Modulo:
                    return isSigned ? builder.CreateSRem(left, right) : builder.CreateURem(left, right);
                case Token::RightShift:
                    return isSigned ? builder.CreateAShr(left, right) : builder.CreateLShr(left, right);
                default:
                    llvm_unreachable("invalid binary operation");
            }
        }
        case ValueKind::IRUnaryOp: {
            auto inst = llvm::cast<IRUnaryOp>(instruction);
            auto operand = codegenExpr(inst->operand);
            switch (inst->op) {
                case Token::Minus:
                    return operand->getType()->isFloatingPointTy() ? builder.CreateFNeg(operand) : builder.CreateNeg(operand);
                case Token::Not:
                    return builder.CreateNot(operand);
                default:
                    llvm_unreachable("invalid unary operation");
            }
        }
        case ValueKind::IRGetElementPtr: {
            auto inst = llvm::cast<IRGetElementPtr>(instruction);
            auto pointer = codegenExpr(inst->pointer);
            auto indexes = map(inst->indexes, [&](auto* index) { return codegenExpr(index); });
            return builder.CreateInBoundsGEP(pointer, indexes, inst->name);
        }
        case ValueKind::IRConstGEP: {
            auto inst = llvm::cast<IRConstGEP>(instruction);
            auto pointer = codegenExpr(inst->pointer);
            return builder.CreateConstInBoundsGEP2_32(nullptr, pointer, inst->index0, inst->index1, inst->name);
        }
        case ValueKind::IRCastInst: {
            auto inst = llvm::cast<IRCastInst>(instruction);
            auto value = codegenExpr(inst->value);
            auto sourceType = inst->value->getType();
            auto type = inst->type;

            // TODO(ir): cleanup
            if (sourceType->isUnsignedInteger() && type->isInteger()) {
                return builder.CreateZExtOrTrunc(value, getLLVMType(type));
            }
            if (sourceType->isSignedInteger() && type->isInteger()) {
                return builder.CreateSExtOrTrunc(value, getLLVMType(type));
            }
            if ((sourceType->isInteger() || sourceType->isChar() || sourceType->isBool()) && (type->isInteger() || type->isChar())) {
                return builder.CreateIntCast(value, getLLVMType(type), sourceType->isSignedInteger());
            }
            if ((sourceType->isInteger() || sourceType->isChar() || sourceType->isBool()) && type->isBool()) {
                return builder.CreateIsNotNull(value);
            }
            if (sourceType->isFloatingPoint()) {
                if (type->isSignedInteger()) return builder.CreateFPToSI(value, getLLVMType(type));
                if (type->isUnsignedInteger()) return builder.CreateFPToUI(value, getLLVMType(type));
                if (type->isFloatingPoint()) return builder.CreateFPCast(value, getLLVMType(type));
            }
            if (type->isFloatingPoint()) {
                if (sourceType->isSignedInteger()) return builder.CreateSIToFP(value, getLLVMType(type));
                if (sourceType->isUnsignedInteger()) return builder.CreateUIToFP(value, getLLVMType(type));
            }

            return builder.CreateBitOrPointerCast(value, getLLVMType(type), inst->name);
        }
        case ValueKind::IRUnreachable: {
            return builder.CreateUnreachable();
        }
        case ValueKind::IRSizeof: {
            auto inst = llvm::cast<IRSizeof>(instruction);
            return llvm::ConstantExpr::getSizeOf(getLLVMType(inst->type));
        }
        case ValueKind::IRBasicBlock: {
            auto inst = llvm::cast<IRBasicBlock>(instruction);
            llvm_unreachable("unhandled IRBasicBlock");
            // TODO(ir): basicblock shouldn't be an IRValue/instruction because these are always unhandled?
            return nullptr;
        }
        case ValueKind::IRFunction: {
            auto inst = llvm::cast<IRFunction>(instruction);
            return getFunctionProto(*inst);
        }
        case ValueKind::IRParam: {
            auto inst = llvm::cast<IRParam>(instruction);
            return getValueOrNull(inst); // TODO(ir) params are only thing that uses getValueOrNull. Find a way to get rid of it?
        }
        case ValueKind::IRGlobalVariable: {
            auto inst = llvm::cast<IRGlobalVariable>(instruction);
            // TODO(ir): Match previous global constant inlining behavior.
            auto linkage = inst->value ? llvm::GlobalValue::PrivateLinkage : llvm::GlobalValue::ExternalLinkage;
            auto initializer = inst->value ? llvm::cast<llvm::Constant>(codegenExpr(inst->value)) : nullptr;
            return new llvm::GlobalVariable(*module, getLLVMType(inst->value->getType()), false, linkage, initializer, inst->name);
        }
        case ValueKind::IRConstantString: {
            auto inst = llvm::cast<IRConstantString>(instruction);
            return builder.CreateGlobalStringPtr(inst->value);
        }
        case ValueKind::IRConstantInt: {
            auto inst = llvm::cast<IRConstantInt>(instruction);
            auto type = getLLVMType(inst->type);
            return llvm::ConstantInt::get(type, inst->value.extOrTrunc(type->getIntegerBitWidth()));
        }
        case ValueKind::IRConstantFP: {
            auto inst = llvm::cast<IRConstantFP>(instruction);
            llvm::SmallString<128> buffer;
            inst->value.toString(buffer);
            return llvm::ConstantFP::get(getLLVMType(inst->type), buffer);
        }
        case ValueKind::IRConstantBool: {
            auto inst = llvm::cast<IRConstantBool>(instruction);
            return inst->value ? llvm::ConstantInt::getTrue(ctx) : llvm::ConstantInt::getFalse(ctx);
        }
        case ValueKind::IRConstantNull: {
            auto inst = llvm::cast<IRConstantNull>(instruction);
            return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(getLLVMType(inst->type)));
        }
        case ValueKind::IRUndefined: {
            auto inst = llvm::cast<IRUndefined>(instruction);
            return llvm::UndefValue::get(getLLVMType(inst->type));
        }
        case ValueKind::IRModule: {
            auto inst = llvm::cast<IRModule>(instruction);
            llvm_unreachable("unhandled IRModule");
            // TODO(ir): IRModule shouldn't be an IRValue/instruction because these are always unhandled?
            return nullptr;
        }
    }
    llvm_unreachable("all cases handled");
}

llvm::Module& LLVMGenerator::codegenModule(const IRModule& sourceModule) {
    ASSERT(!module);
    module = new llvm::Module(sourceModule.name, ctx);

    for (auto* globalVariable : sourceModule.globalVariables) {
        codegenExpr(globalVariable);
    }

    for (auto* function : sourceModule.functions) {
        codegenFunction(*function);
    }

    for (size_t i = 0; i < functionInstantiations.size(); ++i) {
        auto& instantiation = functionInstantiations[i];

        if (!instantiation.decl->isExtern && instantiation.function->empty()) {
            codegenFunctionBody(*instantiation.decl, *instantiation.function);
            ASSERT(!llvm::verifyFunction(*instantiation.function, &llvm::errs()));
            // TODO(ir) this is duplicated in codegenFunction?
        }
    }

    ASSERT(!llvm::verifyModule(*module, &llvm::errs()));
    generatedModules.push_back(module);
    module = nullptr;
    return *generatedModules.back();
}
