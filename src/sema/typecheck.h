#pragma once

#include <functional>
#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/SmallPtrSet.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/Support/ErrorOr.h>
#pragma warning(pop)
#include "../ast/decl.h"
#include "../ast/expr.h"
#include "../ast/stmt.h"

namespace llvm {
class StringRef;
template<typename T>
class ArrayRef;
template<typename T, unsigned N>
class SmallVector;
} // namespace llvm

namespace delta {

struct Module;
struct PackageManifest;
struct SourceFile;
struct SourceLocation;
struct Type;
struct CompileOptions;

struct ArgumentValidation {
    enum Error { None, TooFew, TooMany, InvalidName, InvalidType };

    Error error;
    int index;

    ArgumentValidation(Error error, int index = -1) : error(error), index(index) {}
};

struct Typechecker {
    Typechecker(const CompileOptions& options)
    : currentModule(nullptr), currentSourceFile(nullptr), currentFunction(nullptr), currentStmt(nullptr), currentInitializedFields(nullptr),
      isPostProcessing(false), options(options) {}
    void typecheckModule(Module& module, const PackageManifest* manifest);

private:
    Module* getCurrentModule() const { return NOTNULL(currentModule); }
    Type typecheckExpr(Expr& expr, bool useIsWriteOnly = false, Type expectedType = Type());
    void typecheckVarDecl(VarDecl& decl);
    void typecheckFieldDecl(FieldDecl& decl);
    void typecheckTopLevelDecl(Decl& decl, const PackageManifest* manifest);
    void typecheckParams(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel);
    void typecheckFunctionDecl(FunctionDecl& decl);
    void typecheckFunctionTemplate(FunctionTemplate& decl);
    void typecheckMethodDecl(Decl& decl);

    bool typecheckStmt(Stmt*& stmt);
    void typecheckCompoundStmt(CompoundStmt& stmt);
    void typecheckReturnStmt(ReturnStmt& stmt);
    void typecheckVarStmt(VarStmt& stmt);
    void typecheckIfStmt(IfStmt& ifStmt);
    void typecheckSwitchStmt(SwitchStmt& stmt);
    void typecheckForStmt(ForStmt& forStmt);
    void typecheckBreakStmt(BreakStmt& breakStmt);
    void typecheckContinueStmt(ContinueStmt& continueStmt);
    void typecheckType(Type type, AccessLevel userAccessLevel);
    void typecheckParamDecl(ParamDecl& decl, AccessLevel userAccessLevel);
    void typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel);
    void typecheckTypeDecl(TypeDecl& decl);
    void typecheckTypeTemplate(TypeTemplate& decl);
    void typecheckEnumDecl(EnumDecl& decl);
    void typecheckImportDecl(ImportDecl& decl, const PackageManifest* manifest);

    Type typecheckVarExpr(VarExpr& expr, bool useIsWriteOnly);
    Type typecheckNullLiteralExpr(NullLiteralExpr& expr, Type expectedType);
    Type typecheckArrayLiteralExpr(ArrayLiteralExpr& expr, Type expectedType = Type());
    Type typecheckTupleExpr(TupleExpr& expr);
    Type typecheckUnaryExpr(UnaryExpr& expr);
    Type typecheckBinaryExpr(BinaryExpr& expr);
    void typecheckAssignment(BinaryExpr& expr, SourceLocation location);
    Type typecheckCallExpr(CallExpr& expr, Type expectedType = Type());
    Type typecheckBuiltinConversion(CallExpr& expr);
    Type typecheckBuiltinCast(CallExpr& expr);
    Type typecheckSizeofExpr(SizeofExpr& expr);
    Type typecheckAddressofExpr(AddressofExpr& expr);
    Type typecheckMemberExpr(MemberExpr& expr);
    Type typecheckIndexExpr(IndexExpr& expr);
    Type typecheckIndexAssignmentExpr(IndexAssignmentExpr& expr);
    Type typecheckUnwrapExpr(UnwrapExpr& expr);
    Type typecheckLambdaExpr(LambdaExpr& expr, Type expectedType);
    Type typecheckIfExpr(IfExpr& expr);

    bool hasMethod(TypeDecl& typeDecl, FunctionDecl& functionDecl) const;
    bool providesInterfaceRequirements(TypeDecl& typeDecl, TypeDecl& interface, std::string* errorReason) const;
    /// Returns the converted expression if the conversion succeeds, or null otherwise.
    Expr* convert(Expr* expr, Type type, bool allowPointerToTemporary = false) const;
    /// Returns the converted type when the implicit conversion succeeds, or the null type when it doesn't.
    Type isImplicitlyConvertible(const Expr* expr, Type source, Type target, bool allowPointerToTemporary = false, bool* setType = nullptr) const;
    llvm::StringMap<Type> getGenericArgsForCall(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call, FunctionDecl* decl, bool returnOnError,
                                                Type expectedType);
    Decl* findDecl(llvm::StringRef name, SourceLocation location) const;
    std::vector<Decl*> findDecls(llvm::StringRef name, TypeDecl* receiverTypeDecl = nullptr, bool inAllImportedModules = false) const;
    std::vector<Decl*> findCalleeCandidates(const CallExpr& expr, llvm::StringRef callee);
    Decl* resolveOverload(llvm::ArrayRef<Decl*> decls, CallExpr& expr, llvm::StringRef callee, Type expectedType);
    std::vector<Type> inferGenericArgsFromCallArgs(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call, llvm::ArrayRef<ParamDecl> params,
                                                   bool returnOnError);
    ArgumentValidation getArgumentValidationResult(CallExpr& expr, llvm::ArrayRef<ParamDecl> params, bool isVariadic);
    bool argumentsMatch(CallExpr& expr, const FunctionDecl* functionDecl, llvm::ArrayRef<ParamDecl> params = {});
    void validateArgs(CallExpr& expr, const Decl& calleeDecl, llvm::StringRef functionName = "", SourceLocation location = SourceLocation());
    void validateArgs(CallExpr& expr, llvm::ArrayRef<ParamDecl> params, bool isVariadic, llvm::StringRef functionName = "",
                      SourceLocation location = SourceLocation());
    TypeDecl* getTypeDecl(const BasicType& type);
    EnumCase* getEnumCase(const Expr& expr);
    void checkReturnPointerToLocal(const Expr* returnValue) const;
    static void checkHasAccess(const Decl& decl, SourceLocation location, AccessLevel userAccessLevel);
    void checkLambdaCapture(const VariableDecl& variableDecl, const VarExpr& varExpr) const;
    llvm::ErrorOr<const Module&> importModule(SourceFile* importer, const PackageManifest* manifest, llvm::StringRef moduleName);
    void postProcess();

    void setMoved(Expr* expr, bool isMoved);
    void checkNotMoved(const Decl& decl, const VarExpr& expr);

    bool isWarningEnabled(llvm::StringRef warning) const;

private:
    Module* currentModule;
    SourceFile* currentSourceFile;
    FunctionDecl* currentFunction;
    Stmt** currentStmt; // Double-pointer so it refers to the correct statement after lowering.
    std::vector<Stmt*> currentControlStmts;
    llvm::SmallPtrSet<FieldDecl*, 32>* currentInitializedFields;
    llvm::SmallPtrSet<Decl*, 32> movedDecls;
    bool isPostProcessing;
    std::vector<Decl*> declsToTypecheck;
    const CompileOptions& options;
};

void validateGenericArgCount(size_t genericParamCount, llvm::ArrayRef<Type> genericArgs, llvm::StringRef name, SourceLocation location);

} // namespace delta
