#pragma once

#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Module.h>
#pragma warning(pop)
#include "../intermediate/ir.h"

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
    llvm::Value* codegenExpr(const Value* instruction);
    llvm::Value* codegenExprUncached(const Value* instruction);
    llvm::BasicBlock* getBasicBlock(const Block* block);
    llvm::Function* getFunctionProto(const Function& function);
    void codegenFunction(const Function& function);

private:
    friend struct LLVMGenScope;

    void codegenFunctionBody(const Function& decl, llvm::Function& function);
    llvm::Type* getLLVMType(IRType* type, SourceLocation location = SourceLocation());
    llvm::Type* getBuiltinType(llvm::StringRef name);
    llvm::Type* getStructType(IRStructType* type);
    llvm::StructType* codegenTypeDecl(IRStructType* type);

private:
    struct FunctionInstantiation {
        const Function* decl;
        llvm::Function* function;
    };

    llvm::LLVMContext ctx;
    llvm::IRBuilder<> builder;
    llvm::Module* module = nullptr;
    std::vector<llvm::Module*> generatedModules;
    std::vector<FunctionInstantiation> functionInstantiations;
    std::unordered_map<const Block*, llvm::BasicBlock*> generatedBlocks;
    std::unordered_map<const Value*, llvm::Value*> generatedValues;
    std::unordered_map<IRType*, llvm::StructType*> structs;
};

} // namespace delta
