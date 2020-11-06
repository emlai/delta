#include "llvm.h"
#pragma warning(push, 0)
#include <llvm/ADT/StringSwitch.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/Verifier.h>
#pragma warning(pop)
#include "../intermediate/ir.h"

using namespace delta;

// TODO(ir) rename llvm directory to "codegen" or "backend"? so all backends can be put there?
//  OR put llvm.cpp/h under ir/ and rename ir/ to "codegen" or "backend"?

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

llvm::Type* LLVMGenerator::getLLVMType(IRType* type) {
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
            return llvm::ArrayType::get(getLLVMType(arrayType->elementType), arrayType->size);
        }
        case IRTypeKind::IRFunctionType: {
            auto functionType = llvm::cast<IRFunctionType>(type);
            auto returnType = getLLVMType(functionType->returnType);
            auto paramTypes = map(functionType->paramTypes, [&](IRType* type) { return getLLVMType(type); });
            return llvm::FunctionType::get(returnType, paramTypes, false);
        }
        case IRTypeKind::IRPointerType: {
            auto pointerType = llvm::cast<IRPointerType>(type);
            auto* pointeeType = getLLVMType(pointerType->pointee);
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
llvm::Function* LLVMGenerator::getFunctionProto(const Function& function) {
    if (auto* llvmFunction = module->getFunction(function.mangledName)) return llvmFunction;

    llvm::SmallVector<llvm::Type*, 16> paramTypes;
    for (auto& param : function.params) {
        paramTypes.emplace_back(getLLVMType(param.type));
    }

    auto* returnType = getLLVMType(function.returnType);
    auto* functionType = llvm::FunctionType::get(returnType, paramTypes, function.isVariadic);
    auto* llvmFunction = llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, function.mangledName, &*module);

    auto arg = llvmFunction->arg_begin(), argsEnd = llvmFunction->arg_end();
    ASSERT(function.params.size() == size_t(std::distance(arg, argsEnd)));
    for (auto param = function.params.begin(); arg != argsEnd; ++param, ++arg) {
        arg->setName(param->name);
    }

    for (auto& instantiation : functionInstantiations) {
        if (instantiation.second->getName() == function.mangledName) {
            return llvmFunction;
        }
    }

    functionInstantiations.push_back({&function, llvmFunction});
    return llvmFunction;
}

// TODO(ir): rename all irfunction decls to "function" or "irfunction" or "intermediatefunction"
// TODO(ir): change params to be pointers, rename decl
void LLVMGenerator::codegenFunctionBody(const Function& function, llvm::Function& llvmFunction) {
    llvm::IRBuilder<>::InsertPointGuard insertPointGuard(builder);

    auto arg = llvmFunction.arg_begin();
    for (auto& param : function.params) {
        generatedValues.emplace(&param, &*arg++);
    }

    for (auto* block : function.body) {
        auto llvmBlock = getBasicBlock(block);
        llvmBlock->insertInto(&llvmFunction);
        builder.SetInsertPoint(llvmBlock);

        for (auto* inst : block->insts) {
            codegenExpr(inst);
        }
    }

    auto insertBlock = builder.GetInsertBlock();
    if (insertBlock && insertBlock != &llvmFunction.getEntryBlock() && llvm::pred_empty(insertBlock)) {
        insertBlock->eraseFromParent();
    }
}

void LLVMGenerator::codegenFunction(const Function& function) {
    auto llvmFunction = getFunctionProto(function);

    if (!function.isExtern && llvmFunction->empty()) {
        codegenFunctionBody(function, *llvmFunction);
    }

#ifndef NDEBUG
    if (llvm::verifyFunction(*llvmFunction, &llvm::errs())) {
        llvm::errs() << '\n';
        llvmFunction->print(llvm::errs(), nullptr, false, true);
        llvm::errs() << '\n';
        ASSERT(false && "llvm::verifyFunction failed");
    }
#endif
}

// TODO(ir): Rename "expr" to "value" or "instruction" in llvm/. Rename "codegen" to "build"?
// TODO(ir): add ir tests?

llvm::BasicBlock* LLVMGenerator::getBasicBlock(const BasicBlock* block) {
    return llvm::cast<llvm::BasicBlock>(codegenExpr(block));
}

// TODO(ir) rename instruction param
llvm::Value* LLVMGenerator::codegenExpr(const Value* instruction) {
    auto it = generatedValues.find(instruction);
    if (it != generatedValues.end()) return it->second;
    auto value = codegenExprUncached(instruction);
    generatedValues.emplace(instruction, value);
    return value;
}

// TODO(ir): find better name
// TODO(ir): refactor cases into functions
llvm::Value* LLVMGenerator::codegenExprUncached(const Value* instruction) {
    switch (instruction->kind) {
        case ValueKind::AllocaInst: {
            auto inst = llvm::cast<AllocaInst>(instruction);
            return builder.CreateAlloca(getLLVMType(inst->allocatedType), nullptr, inst->name);
        }
        case ValueKind::ReturnInst: {
            auto inst = llvm::cast<ReturnInst>(instruction);
            if (inst->value) {
                return builder.CreateRet(codegenExpr(inst->value));
            } else {
                return builder.CreateRetVoid();
            }
        }
        case ValueKind::BranchInst: {
            auto inst = llvm::cast<BranchInst>(instruction);
            return builder.CreateBr(getBasicBlock(inst->destination));
        }
        case ValueKind::CondBranchInst: {
            auto inst = llvm::cast<CondBranchInst>(instruction);
            auto condition = codegenExpr(inst->condition);
            auto trueBlock = getBasicBlock(inst->trueBlock);
            auto falseBlock = getBasicBlock(inst->falseBlock);
            return builder.CreateCondBr(condition, trueBlock, falseBlock);
        }
        case ValueKind::PhiInst: {
            auto inst = llvm::cast<PhiInst>(instruction);
            auto type = getLLVMType(inst->valuesAndPredecessors[0].first->getType());
            auto phi = builder.CreatePHI(type, (unsigned) inst->valuesAndPredecessors.size(), inst->name);
            for (auto& p : inst->valuesAndPredecessors) {
                auto value = codegenExpr(p.first);
                auto block = getBasicBlock(p.second);
                phi->addIncoming(value, block);
            }
            return phi;
        }
        case ValueKind::SwitchInst: {
            auto inst = llvm::cast<SwitchInst>(instruction);
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
            return switchInst;
        }
        case ValueKind::LoadInst: {
            auto inst = llvm::cast<LoadInst>(instruction);
            return builder.CreateLoad(codegenExpr(inst->value), inst->name);
        }
        case ValueKind::StoreInst: {
            auto inst = llvm::cast<StoreInst>(instruction);
            auto value = codegenExpr(inst->value);
            auto pointer = codegenExpr(inst->pointer);
            return builder.CreateStore(value, pointer);
        }
        case ValueKind::InsertInst: {
            auto inst = llvm::cast<InsertInst>(instruction);
            auto aggregate = codegenExpr(inst->aggregate);
            auto value = codegenExpr(inst->value);
            return builder.CreateInsertValue(aggregate, value, inst->index);
        }
        case ValueKind::ExtractInst: {
            auto inst = llvm::cast<ExtractInst>(instruction);
            auto aggregate = codegenExpr(inst->aggregate);
            return builder.CreateExtractValue(aggregate, inst->index, inst->name);
        }
        case ValueKind::CallInst: {
            auto inst = llvm::cast<CallInst>(instruction);
            auto function = codegenExpr(inst->function);
            auto args = map(inst->args, [&](auto* arg) { return codegenExpr(arg); });
            ASSERT(function->getType()->isFunctionTy() || (function->getType()->isPointerTy() && function->getType()->getPointerElementType()->isFunctionTy()));
            return builder.CreateCall(function, args);
        }
        case ValueKind::BinaryInst: {
            auto inst = llvm::cast<BinaryInst>(instruction);
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
        case ValueKind::UnaryInst: {
            auto inst = llvm::cast<UnaryInst>(instruction);
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
        case ValueKind::GEPInst: {
            auto inst = llvm::cast<GEPInst>(instruction);
            auto pointer = codegenExpr(inst->pointer);
            auto indexes = map(inst->indexes, [&](auto* index) { return codegenExpr(index); });
            return builder.CreateInBoundsGEP(pointer, indexes, inst->name);
        }
        case ValueKind::ConstGEPInst: {
            auto inst = llvm::cast<ConstGEPInst>(instruction);
            auto pointer = codegenExpr(inst->pointer);
            return builder.CreateConstInBoundsGEP2_32(nullptr, pointer, inst->index0, inst->index1, inst->name);
        }
        case ValueKind::CastInst: {
            auto inst = llvm::cast<CastInst>(instruction);
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
        case ValueKind::UnreachableInst: {
            return builder.CreateUnreachable();
        }
        case ValueKind::SizeofInst: {
            auto inst = llvm::cast<SizeofInst>(instruction);
            return llvm::ConstantExpr::getSizeOf(getLLVMType(inst->type));
        }
        case ValueKind::BasicBlock: {
            auto block = llvm::cast<BasicBlock>(instruction);
            return llvm::BasicBlock::Create(ctx, block->name);
        }
        case ValueKind::Function: {
            auto inst = llvm::cast<Function>(instruction);
            return getFunctionProto(*inst);
        }
        case ValueKind::Parameter: {
            return generatedValues.at(instruction);
        }
        case ValueKind::GlobalVariable: {
            auto inst = llvm::cast<GlobalVariable>(instruction);
            // TODO(ir): Match previous global constant inlining behavior.
            auto linkage = inst->value ? llvm::GlobalValue::PrivateLinkage : llvm::GlobalValue::ExternalLinkage;
            auto initializer = inst->value ? llvm::cast<llvm::Constant>(codegenExpr(inst->value)) : nullptr;
            return new llvm::GlobalVariable(*module, getLLVMType(inst->value->getType()), false, linkage, initializer, inst->name);
        }
        case ValueKind::ConstantString: {
            auto inst = llvm::cast<ConstantString>(instruction);
            return builder.CreateGlobalStringPtr(inst->value);
        }
        case ValueKind::ConstantInt: {
            auto inst = llvm::cast<ConstantInt>(instruction);
            auto type = getLLVMType(inst->type);
            return llvm::ConstantInt::get(type, inst->value.extOrTrunc(type->getIntegerBitWidth()));
        }
        case ValueKind::ConstantFP: {
            auto inst = llvm::cast<ConstantFP>(instruction);
            llvm::SmallString<128> buffer;
            inst->value.toString(buffer);
            return llvm::ConstantFP::get(getLLVMType(inst->type), buffer);
        }
        case ValueKind::ConstantBool: {
            auto inst = llvm::cast<ConstantBool>(instruction);
            return inst->value ? llvm::ConstantInt::getTrue(ctx) : llvm::ConstantInt::getFalse(ctx);
        }
        case ValueKind::ConstantNull: {
            auto inst = llvm::cast<ConstantNull>(instruction);
            return llvm::ConstantPointerNull::get(llvm::cast<llvm::PointerType>(getLLVMType(inst->type)));
        }
        case ValueKind::Undefined: {
            auto inst = llvm::cast<Undefined>(instruction);
            return llvm::UndefValue::get(getLLVMType(inst->type));
        }
        case ValueKind::IRModule: {
            llvm_unreachable("unhandled IRModule"); // TODO(ir): IRModule shouldn't be an Value/instruction because these are always unhandled?
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

        if (!instantiation.first->isExtern && instantiation.second->empty()) {
            codegenFunctionBody(*instantiation.first, *instantiation.second);
            ASSERT(!llvm::verifyFunction(*instantiation.second, &llvm::errs()));
            // TODO(ir) this is duplicated in codegenFunction?
        }
    }

    ASSERT(!llvm::verifyModule(*module, &llvm::errs()));
    generatedModules.push_back(module);
    module = nullptr;
    return *generatedModules.back();
}
