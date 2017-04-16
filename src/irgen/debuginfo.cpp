#include "debuginfo.h"
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/Triple.h>
#include <llvm/IR/DIBuilder.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/IRBuilder.h>
#include "../ast/decl.h"

using namespace delta;

namespace {

std::unique_ptr<llvm::DIBuilder> builder;
llvm::DIFile* file;
llvm::DICompileUnit* compileUnit;

llvm::DISubroutineType* createFunctionType() {
    return builder->createSubroutineType({}); // TODO: Set parameter types.
}

llvm::DebugLoc getDebugLoc(const Expr& expr, const llvm::MDNode* scope) {
    return llvm::DebugLoc::get(expr.getSrcLoc().line, expr.getSrcLoc().column, scope);
}

}

void irgen::initDebugInfo(llvm::LLVMContext& ctx, llvm::StringRef fileName,
                          llvm::StringRef directory, llvm::Module& module,
                          bool isOptimized, llvm::StringRef flags) {
    if (!builder) {
        // Add the current debug info version into the module.
        module.addModuleFlag(llvm::Module::Warning, "Debug Info Version",
                             llvm::DEBUG_METADATA_VERSION);

        // Darwin only supports dwarf2.
        if (llvm::Triple(llvm::sys::getProcessTriple()).isOSDarwin())
            module.addModuleFlag(llvm::Module::Warning, "Dwarf Version", 2);

        builder = llvm::make_unique<llvm::DIBuilder>(module);
    }
    file = llvm::DIFile::get(ctx, fileName, directory);
    compileUnit = builder->createCompileUnit(llvm::dwarf::DW_LANG_C, file,
                                             "Delta Compiler", isOptimized, flags, 0);
}

void irgen::finalizeDebugInfo() {
    builder->finalize();
}

void irgen::emitDebugInfo(const FuncDecl& decl, llvm::Function* func) {
    auto line = decl.getSrcLoc().line;
    unsigned scopeLine = 0;
    llvm::DISubprogram* sp = builder->createFunction(file, decl.name, func->getName(),
                                                     file, line, createFunctionType(),
                                                     false /* isLocalToUnit */,
                                                     true /* isDefinition */, scopeLine,
                                                     llvm::DINode::FlagPrototyped,
                                                     false /* isOptimized */);
    func->setSubprogram(sp);
}

void irgen::clearDebugLoc(llvm::IRBuilder<>& irBuilder) {
    return irBuilder.SetCurrentDebugLocation(llvm::DebugLoc());
}

void irgen::setDebugLoc(llvm::IRBuilder<>& irBuilder, const Expr& expr) {
    llvm::DIScope* scope = compileUnit;
    irBuilder.SetCurrentDebugLocation(getDebugLoc(expr, scope));
}
