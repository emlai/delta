#pragma once

#include <llvm/IR/IRBuilder.h>

namespace llvm {
    class Module;
    class LLVMContext;
    class StringRef;
    class Function;
}

namespace delta {
    class FuncDecl;
    class Expr;

    namespace irgen {
        void initDebugInfo(llvm::LLVMContext& ctx, llvm::StringRef fileName,
                           llvm::StringRef directory, llvm::Module& module,
                           bool isOptimized, llvm::StringRef flags);
        void finalizeDebugInfo();
        void emitDebugInfo(const FuncDecl& decl, llvm::Function* func);
        void clearDebugLoc(llvm::IRBuilder<>& irBuilder);
        void setDebugLoc(llvm::IRBuilder<>& irBuilder, const Expr& expr);
    }
}
