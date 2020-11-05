#pragma once

#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#pragma warning(pop)
#include "../ir/ir.h"

// TODO(ir): Rename "decl" to something else

namespace delta {

struct IRModule;
struct Type;
class Typechecker;
class LLVMGenerator;
class Decl;
class EnumDecl;
class FunctionDecl;

class LLVMGenerator {
public:
    LLVMGenerator() : builder(ctx) {}
    llvm::Module& codegenModule(const IRModule& sourceModule);
    llvm::LLVMContext& getLLVMContext() { return ctx; }
    std::vector<llvm::Module*> getGeneratedModules() { return std::move(generatedModules); }

private:
    // TODO(ir): Rename to codegenInst.
    llvm::Value* codegenExpr(const IRValue* instruction);
    llvm::Value* codegenExprUncached(const IRValue* instruction);
    llvm::BasicBlock* getBasicBlock(const IRBasicBlock* block);
    llvm::Function* getFunctionProto(const IRFunction& function);
    void codegenFunction(const IRFunction& function);

private:
    friend struct LLVMGenScope;

    void codegenFunctionBody(const IRFunction& decl, llvm::Function& function);
    llvm::Type* getLLVMType(IRType* type, SourceLocation location = SourceLocation());
    llvm::Type* getBuiltinType(llvm::StringRef name);
    llvm::Type* getStructType(IRStructType* type);
    llvm::StructType* codegenTypeDecl(IRStructType* type);

private:
    struct FunctionInstantiation {
        const IRFunction* decl;
        llvm::Function* function;
    };

    llvm::LLVMContext ctx;
    llvm::IRBuilder<> builder;
    llvm::Module* module = nullptr;
    std::vector<llvm::Module*> generatedModules;
    std::vector<FunctionInstantiation> functionInstantiations;
    std::unordered_map<const IRBasicBlock*, llvm::BasicBlock*> generatedBlocks;
    std::unordered_map<const IRValue*, llvm::Value*> generatedValues;
    std::unordered_map<IRType*, llvm::StructType*> structs;
};

} // namespace delta
