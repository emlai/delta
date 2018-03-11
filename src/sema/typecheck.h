#pragma once

#include <memory>
#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/StringMap.h>
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

class Module;
class PackageManifest;
class SourceFile;
struct SourceLocation;
struct Type;

class Typechecker {
public:
    Typechecker()
    : currentModule(nullptr), currentSourceFile(nullptr), currentFunction(nullptr), isPostProcessing(false) {}

    Module* getCurrentModule() const { return ASSERT(currentModule), currentModule; }
    void setCurrentModule(Module* module) { currentModule = module; }
    const SourceFile* getCurrentSourceFile() const { return currentSourceFile; }

    void typecheckModule(Module& module, const PackageManifest* manifest, llvm::ArrayRef<std::string> importSearchPaths,
                         llvm::ArrayRef<std::string> frameworkSearchPaths);
    Type typecheckExpr(Expr& expr, bool useIsWriteOnly = false);
    void typecheckVarDecl(VarDecl& decl, bool isGlobal);
    void typecheckFieldDecl(FieldDecl& decl);
    void typecheckTopLevelDecl(Decl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importSearchPaths,
                               llvm::ArrayRef<std::string> frameworkSearchPaths);
    void postProcess();

private:
    void typecheckParams(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel);
    void typecheckFunctionDecl(FunctionDecl& decl);
    void typecheckFunctionTemplate(FunctionTemplate& decl);
    void typecheckMemberDecl(Decl& decl);

    void typecheckStmt(std::unique_ptr<Stmt>& stmt);
    void typecheckCompoundStmt(CompoundStmt& stmt);
    void typecheckReturnStmt(ReturnStmt& stmt);
    void typecheckVarStmt(VarStmt& stmt);
    void typecheckIfStmt(IfStmt& ifStmt);
    void typecheckSwitchStmt(SwitchStmt& stmt);
    void typecheckWhileStmt(WhileStmt& whileStmt);
    void typecheckBreakStmt(BreakStmt& breakStmt);
    void typecheckContinueStmt(ContinueStmt& continueStmt);
    void typecheckType(Type type, AccessLevel userAccessLevel);
    void typecheckParamDecl(ParamDecl& decl, AccessLevel userAccessLevel);
    void typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel);
    void typecheckTypeDecl(TypeDecl& decl);
    void typecheckTypeTemplate(TypeTemplate& decl);
    void typecheckEnumDecl(EnumDecl& decl);
    void typecheckImportDecl(ImportDecl& decl, const PackageManifest* manifest, llvm::ArrayRef<std::string> importSearchPaths,
                             llvm::ArrayRef<std::string> frameworkSearchPaths);

    Type typecheckVarExpr(VarExpr& expr, bool useIsWriteOnly);
    Type typecheckArrayLiteralExpr(ArrayLiteralExpr& expr);
    Type typecheckTupleExpr(TupleExpr& expr);
    Type typecheckUnaryExpr(UnaryExpr& expr);
    Type typecheckBinaryExpr(BinaryExpr& expr);
    Type typecheckBinaryExpr(BinaryExpr& expr, Token::Kind op);
    void typecheckAssignment(Expr& lhs, Expr* rhs, Type rightType, SourceLocation location);
    Type typecheckCallExpr(CallExpr& expr);
    Type typecheckBuiltinConversion(CallExpr& expr);
    Type typecheckBuiltinCast(CallExpr& expr);
    Type typecheckSizeofExpr(SizeofExpr& expr);
    Type typecheckAddressofExpr(AddressofExpr& expr);
    Type typecheckMemberExpr(MemberExpr& expr);
    Type typecheckSubscriptExpr(SubscriptExpr& expr);
    Type typecheckUnwrapExpr(UnwrapExpr& expr);
    Type typecheckLambdaExpr(LambdaExpr& expr);
    Type typecheckIfExpr(IfExpr& expr);

    bool isInterface(Type type);
    bool hasMethod(TypeDecl& type, FunctionDecl& functionDecl) const;
    bool providesInterfaceRequirements(TypeDecl& type, TypeDecl& interface, std::string* errorReason) const;
    bool isImplicitlyConvertible(const Expr* expr, Type source, Type target, Type* convertedType) const;
    llvm::StringMap<Type> getGenericArgsForCall(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call,
                                                llvm::ArrayRef<ParamDecl> params, bool returnOnError);
    std::vector<Decl*> findCalleeCandidates(const CallExpr& expr, llvm::StringRef callee);
    Decl* resolveOverload(llvm::ArrayRef<Decl*> decls, CallExpr& expr, llvm::StringRef callee, bool returnNullOnError = false);
    std::vector<Type> inferGenericArgs(llvm::ArrayRef<GenericParamDecl> genericParams, CallExpr& call,
                                       llvm::ArrayRef<ParamDecl> params, bool returnOnError);
    bool argumentsMatch(const CallExpr& expr, const FunctionDecl* functionDecl, llvm::ArrayRef<ParamDecl> params = {}) const;
    void validateArgs(CallExpr& expr, const Decl& calleeDecl, llvm::StringRef functionName = "",
                      SourceLocation location = SourceLocation()) const;
    void validateArgs(CallExpr& expr, bool isMutating, llvm::ArrayRef<ParamDecl> params, bool isVariadic,
                      llvm::StringRef functionName = "", SourceLocation location = SourceLocation()) const;
    TypeDecl* getTypeDecl(const BasicType& type);
    void markFieldAsInitialized(Expr& expr);
    void checkCanMove(Expr& expr);
    void checkReturnPointerToLocal(const ReturnStmt& stmt) const;
    static void checkHasAccess(const Decl& decl, SourceLocation location, AccessLevel userAccessLevel);

    llvm::ErrorOr<const Module&> importDeltaModule(SourceFile* importer, const PackageManifest* manifest,
                                                   llvm::ArrayRef<std::string> importSearchPaths,
                                                   llvm::ArrayRef<std::string> frameworkSearchPaths,
                                                   llvm::StringRef moduleExternalName,
                                                   llvm::StringRef moduleInternalName = "");

    /// Returns true if the given expression (of optional type) is guaranteed to be non-null, e.g.
    /// if it was previously checked against null, and the type-checker can prove that it wasn't set
    /// back to null after that check.
    bool isGuaranteedNonNull(const Expr& expr) const;
    bool isGuaranteedNonNull(const Expr& expr, const Stmt& currentControlStmt) const;

    /// Returns true if executing the given statement/expression/block might result in 'null' being
    /// assigned to the given variable before evaluating it. Returns false if the variable is
    /// guaranteed to be null when evaluating it. Returns an empty optional if the expression didn't
    /// contain the given variable, but also can't result in 'null' being assigned to it.
    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, const Stmt& stmt) const;
    llvm::Optional<bool> subExprMaySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const;
    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const;
    llvm::Optional<bool> maySetToNullBeforeEvaluating(const Expr& var, llvm::ArrayRef<std::unique_ptr<Stmt>> block) const;

private:
    Module* currentModule;
    SourceFile* currentSourceFile;
    std::vector<std::pair<FieldDecl*, bool>> currentFieldDecls;
    FunctionDecl* currentFunction;
    std::vector<Stmt*> currentControlStmts;
    Type functionReturnType;
    bool isPostProcessing;
    std::vector<Decl*> declsToTypecheck;
};

} // namespace delta
