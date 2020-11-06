#pragma once

#include <unordered_map>
#include <vector>
#pragma warning(push, 0)
#include <llvm/IR/IRBuilder.h>
#pragma warning(pop)

namespace delta {

struct IRModule;
struct IRType;
struct IRStructType;
struct Value;
struct Function;
struct BasicBlock;

class LLVMGenerator {
public:
    LLVMGenerator() : builder(ctx) {}
    llvm::Module& codegenModule(const IRModule& sourceModule);
    llvm::LLVMContext& getLLVMContext() { return ctx; }
    std::vector<llvm::Module*> getGeneratedModules() { return std::move(generatedModules); }

private:
    // TODO(ir): Rename to codegenInst.
    llvm::Value* codegenExpr(const Value* instruction);
    llvm::Value* codegenExprUncached(const Value* instruction);
    llvm::BasicBlock* getBasicBlock(const BasicBlock* block);
    llvm::Function* getFunctionProto(const Function& function);
    void codegenFunction(const Function& function);

private:
    void codegenFunctionBody(const Function& function, llvm::Function& llvmFunction);
    llvm::Type* getLLVMType(IRType* type);
    llvm::Type* getBuiltinType(llvm::StringRef name);
    llvm::Type* getStructType(IRStructType* type);

private:
    llvm::LLVMContext ctx;
    llvm::IRBuilder<> builder;
    llvm::Module* module = nullptr;
    std::vector<llvm::Module*> generatedModules;
    std::vector<std::pair<const Function*, llvm::Function*>> functionInstantiations; // TODO(ir) make this a map also?
    std::unordered_map<const BasicBlock*, llvm::BasicBlock*> generatedBlocks;
    std::unordered_map<const Value*, llvm::Value*> generatedValues;
    std::unordered_map<IRType*, llvm::StructType*> structs;
};

} // namespace delta
