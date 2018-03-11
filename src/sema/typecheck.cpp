#include "typecheck.h"
#include <algorithm>
#include <cstdlib>
#include <iterator>
#include <memory>
#include <string>
#include <system_error>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/StringMap.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/Support/ErrorHandling.h>
#include <llvm/Support/ErrorOr.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/Path.h>
#pragma warning(pop)
#include "c-import.h"
#include "../ast/decl.h"
#include "../ast/expr.h"
#include "../ast/mangle.h"
#include "../ast/module.h"
#include "../ast/type.h"
#include "../package-manager/manifest.h"
#include "../parser/parse.h"
#include "../support/utility.h"

using namespace delta;

void validateGenericArgCount(size_t genericParamCount, llvm::ArrayRef<Type> genericArgs, llvm::StringRef name,
                             SourceLocation location);

void Typechecker::checkReturnPointerToLocal(const ReturnStmt& stmt) const {
    auto* returnValue = stmt.getReturnValue();

    if (auto* unaryExpr = llvm::dyn_cast<UnaryExpr>(returnValue)) {
        if (unaryExpr->getOperator() == Token::And) {
            returnValue = &unaryExpr->getOperand();
        }
    }

    Type localVariableType;

    if (auto* varExpr = llvm::dyn_cast<VarExpr>(returnValue)) {
        switch (varExpr->getDecl()->getKind()) {
            case DeclKind::VarDecl: {
                auto* varDecl = llvm::cast<VarDecl>(varExpr->getDecl());
                if (varDecl->getParent() && varDecl->getParent()->isFunctionDecl()) {
                    localVariableType = varDecl->getType();
                }
                break;
            }
            case DeclKind::ParamDecl:
                localVariableType = llvm::cast<ParamDecl>(varExpr->getDecl())->getType();
                break;

            default:
                break;
        }
    }

    if (localVariableType && functionReturnType.removeOptional().isPointerType() &&
        functionReturnType.removeOptional().getPointee() == localVariableType) {
        warning(returnValue->getLocation(), "returning pointer to local variable ",
                "(local variables will not exist after the function returns)");
    }
}

void Typechecker::typecheckReturnStmt(ReturnStmt& stmt) {
    if (!stmt.getReturnValue()) {
        if (!functionReturnType.isVoid()) {
            error(stmt.getLocation(), "expected return statement to return a value of type '", functionReturnType, "'");
        }
        return;
    }

    Type returnValueType = typecheckExpr(*stmt.getReturnValue());
    Type convertedType;

    if (!isImplicitlyConvertible(stmt.getReturnValue(), returnValueType, functionReturnType, &convertedType)) {
        error(stmt.getLocation(), "mismatching return type '", returnValueType, "', expected '", functionReturnType,
              "'");
    }

    stmt.getReturnValue()->setType(convertedType ? convertedType : returnValueType);
    checkReturnPointerToLocal(stmt);

    if (!functionReturnType.isImplicitlyCopyable()) {
        checkCanMove(*stmt.getReturnValue());
        stmt.getReturnValue()->setMoved(true);
    }
}

void Typechecker::typecheckVarStmt(VarStmt& stmt) {
    typecheckVarDecl(stmt.getDecl(), false);
}

void Typechecker::typecheckIfStmt(IfStmt& ifStmt) {
    Type conditionType = typecheckExpr(ifStmt.getCondition());
    if (!conditionType.isBool() && !conditionType.isOptionalType()) {
        error(ifStmt.getCondition().getLocation(), "'if' condition must have type 'bool' or optional type");
    }

    currentControlStmts.push_back(&ifStmt);

    for (auto& stmt : ifStmt.getThenBody()) {
        typecheckStmt(stmt);
    }
    for (auto& stmt : ifStmt.getElseBody()) {
        typecheckStmt(stmt);
    }

    currentControlStmts.pop_back();
}

void Typechecker::typecheckSwitchStmt(SwitchStmt& stmt) {
    Type conditionType = typecheckExpr(stmt.getCondition());

    currentControlStmts.push_back(&stmt);

    for (auto& switchCase : stmt.getCases()) {
        Type caseType = typecheckExpr(*switchCase.getValue());

        if (!isImplicitlyConvertible(switchCase.getValue(), caseType, conditionType, nullptr)) {
            error(switchCase.getValue()->getLocation(), "case value type '", caseType,
                  "' doesn't match switch condition type '", conditionType, "'");
        }
        for (auto& caseStmt : switchCase.getStmts()) {
            typecheckStmt(caseStmt);
        }
    }
    for (auto& defaultStmt : stmt.getDefaultStmts()) {
        typecheckStmt(defaultStmt);
    }

    currentControlStmts.pop_back();
}

void Typechecker::typecheckWhileStmt(WhileStmt& whileStmt) {
    Type conditionType = typecheckExpr(whileStmt.getCondition());
    if (!conditionType.isBool() && !conditionType.isOptionalType()) {
        error(whileStmt.getCondition().getLocation(), "'while' condition must have type 'bool' or optional type");
    }

    currentControlStmts.push_back(&whileStmt);

    for (auto& stmt : whileStmt.getBody()) {
        typecheckStmt(stmt);
    }

    currentControlStmts.pop_back();
}

void Typechecker::typecheckBreakStmt(BreakStmt& breakStmt) {
    if (llvm::none_of(currentControlStmts, [](const Stmt* stmt) { return stmt->isBreakable(); })) {
        error(breakStmt.getLocation(), "'break' is only allowed inside 'while', 'for', and 'switch' statements");
    }
}

void Typechecker::typecheckContinueStmt(ContinueStmt& continueStmt) {
    if (llvm::none_of(currentControlStmts, [](const Stmt* stmt) { return stmt->isContinuable(); })) {
        error(continueStmt.getLocation(), "'continue' is only allowed inside 'while' and 'for' statements");
    }
}

void Typechecker::typecheckCompoundStmt(CompoundStmt& stmt) {
    getCurrentModule()->getSymbolTable().pushScope();

    for (auto& substmt : stmt.getBody()) {
        typecheckStmt(substmt);
    }

    getCurrentModule()->getSymbolTable().popScope();
}

void Typechecker::markFieldAsInitialized(Expr& expr) {
    if (currentFunction->isInitDecl()) {
        switch (expr.getKind()) {
            case ExprKind::VarExpr: {
                auto* varExpr = llvm::cast<VarExpr>(&expr);

                if (auto* fieldDecl = llvm::dyn_cast<FieldDecl>(varExpr->getDecl())) {
                    auto it = llvm::find_if(currentFieldDecls,
                                            [&](std::pair<FieldDecl*, bool>& p) { return p.first == fieldDecl; });

                    if (it != currentFieldDecls.end()) {
                        it->second = true; // Mark member variable as initialized.
                    }
                }

                break;
            }
            case ExprKind::MemberExpr: {
                auto* memberExpr = llvm::cast<MemberExpr>(&expr);

                if (auto* varExpr = llvm::dyn_cast<VarExpr>(memberExpr->getBaseExpr())) {
                    if (varExpr->getIdentifier() == "this") {
                        auto it = llvm::find_if(currentFieldDecls, [&](std::pair<FieldDecl*, bool>& p) {
                            return p.first->getName() == memberExpr->getMemberName();
                        });

                        if (it != currentFieldDecls.end()) {
                            it->second = true; // Mark member variable as initialized.
                        }
                    }
                }

                break;
            }
            default:
                break;
        }
    }
}

void Typechecker::checkCanMove(Expr& expr) {
    if (!expr.canDestructivelyMove()) {
        if (auto* varExpr = llvm::dyn_cast<VarExpr>(&expr)) {
            error(expr.getLocation(), "cannot destructively move from non-local variable '", varExpr->getIdentifier(),
                  "'");
        } else {
            error(expr.getLocation(), "cannot destructively move from non-local non-temporary value");
        }
    }
}

static const Expr& getIfOrWhileCondition(const Stmt& ifOrWhileStmt) {
    switch (ifOrWhileStmt.getKind()) {
        case StmtKind::IfStmt:
            return llvm::cast<IfStmt>(ifOrWhileStmt).getCondition();
        case StmtKind::WhileStmt:
            return llvm::cast<WhileStmt>(ifOrWhileStmt).getCondition();
        default:
            llvm_unreachable("should be IfStmt or WhileStmt");
    }
}

static llvm::ArrayRef<std::unique_ptr<Stmt>> getIfOrWhileThenBody(const Stmt& ifOrWhileStmt) {
    switch (ifOrWhileStmt.getKind()) {
        case StmtKind::IfStmt:
            return llvm::cast<IfStmt>(ifOrWhileStmt).getThenBody();
        case StmtKind::WhileStmt:
            return llvm::cast<WhileStmt>(ifOrWhileStmt).getBody();
        default:
            llvm_unreachable("should be IfStmt or WhileStmt");
    }
}

static llvm::Optional<bool> memberExprChainsMatch(const MemberExpr& a, const MemberExpr& b) {
    if (a.getMemberName() != b.getMemberName()) return false;

    switch (a.getBaseExpr()->getKind()) {
        case ExprKind::VarExpr: {
            auto* bBaseVarExpr = llvm::dyn_cast<VarExpr>(b.getBaseExpr());
            ASSERT(llvm::cast<VarExpr>(a.getBaseExpr())->getDecl() && bBaseVarExpr->getDecl());
            return bBaseVarExpr && llvm::cast<VarExpr>(a.getBaseExpr())->getDecl() == bBaseVarExpr->getDecl();
        }
        case ExprKind::MemberExpr: {
            auto* bBaseMemberExpr = llvm::dyn_cast<MemberExpr>(b.getBaseExpr());
            return bBaseMemberExpr && memberExprChainsMatch(*llvm::cast<MemberExpr>(a.getBaseExpr()), *bBaseMemberExpr);
        }
        default:
            return llvm::None;
    }
}

bool Typechecker::isGuaranteedNonNull(const Expr& expr) const {
    for (auto* currentControlStmt : llvm::reverse(currentControlStmts)) {
        if (isGuaranteedNonNull(expr, *currentControlStmt)) return true;
    }
    return false;
}

namespace {
struct NullCheckInfo {
    const Expr* nullableValue = nullptr;
    Token::Kind op = Token::None;
    bool isNullCheck() const { return nullableValue != nullptr; }
};
} // namespace

static NullCheckInfo analyzeNullCheck(const Expr& condition) {
    NullCheckInfo nullCheckInfo;

    if (condition.getType().isOptionalType()) {
        nullCheckInfo.nullableValue = &condition;
        nullCheckInfo.op = Token::NotEqual;
    } else if (auto* binaryExpr = llvm::dyn_cast<BinaryExpr>(&condition)) {
        if (binaryExpr->getRHS().isNullLiteralExpr()) {
            nullCheckInfo.nullableValue = &binaryExpr->getLHS();
            nullCheckInfo.op = binaryExpr->getOperator();
        }
    } else if (auto* unaryExpr = llvm::dyn_cast<UnaryExpr>(&condition)) {
        if (unaryExpr->getOperator() == Token::Not) {
            nullCheckInfo = analyzeNullCheck(unaryExpr->getOperand());
            nullCheckInfo.op = nullCheckInfo.op == Token::Equal ? Token::NotEqual : Token::Equal;
        }
    }

    if (nullCheckInfo.isNullCheck()) {
        ASSERT(nullCheckInfo.op == Token::NotEqual || nullCheckInfo.op == Token::Equal);
    }

    return nullCheckInfo;
}

bool Typechecker::isGuaranteedNonNull(const Expr& expr, const Stmt& currentControlStmt) const {
    if (!currentControlStmt.isIfStmt() && !currentControlStmt.isWhileStmt()) return false;
    auto& condition = getIfOrWhileCondition(currentControlStmt);
    NullCheckInfo nullCheckInfo = analyzeNullCheck(condition);
    if (!nullCheckInfo.isNullCheck()) return false;

    switch (expr.getKind()) {
        case ExprKind::VarExpr: {
            auto* decl = llvm::cast<VarExpr>(expr).getDecl();
            if (!decl) return false;
            auto* lhs = llvm::dyn_cast<VarExpr>(nullCheckInfo.nullableValue);
            if (!lhs || lhs->getDecl() != decl) return false;
            break;
        }
        case ExprKind::MemberExpr: {
            auto* lhs = llvm::dyn_cast<MemberExpr>(nullCheckInfo.nullableValue);
            if (!lhs) return false;
            if (memberExprChainsMatch(llvm::cast<MemberExpr>(expr), *lhs) != true) return false;
            break;
        }
        default:
            return false;
    }

    llvm::ArrayRef<std::unique_ptr<Stmt>> stmts;
    switch (nullCheckInfo.op) {
        case Token::NotEqual:
            stmts = getIfOrWhileThenBody(currentControlStmt);
            break;
        case Token::Equal:
            if (auto* ifStmt = llvm::dyn_cast<IfStmt>(&currentControlStmt)) {
                stmts = ifStmt->getElseBody();
            } else {
                return false;
            }
            break;
        default:
            return false;
    }

    for (auto& stmt : stmts) {
        if (auto result = maySetToNullBeforeEvaluating(expr, *stmt)) return !*result;
    }

    return false;
}

llvm::Optional<bool> Typechecker::maySetToNullBeforeEvaluating(const Expr& var, const Stmt& stmt) const {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt:
            if (auto* returnValue = llvm::cast<ReturnStmt>(stmt).getReturnValue()) {
                return maySetToNullBeforeEvaluating(var, *returnValue);
            }
            return llvm::None;

        case StmtKind::VarStmt:
            return maySetToNullBeforeEvaluating(var, *llvm::cast<VarStmt>(stmt).getDecl().getInitializer());

        case StmtKind::ExprStmt:
            return maySetToNullBeforeEvaluating(var, llvm::cast<ExprStmt>(stmt).getExpr());

        case StmtKind::DeferStmt:
            return true;

        case StmtKind::IfStmt: {
            auto& ifStmt = llvm::cast<IfStmt>(stmt);
            if (auto result = maySetToNullBeforeEvaluating(var, ifStmt.getCondition())) return *result;
            if (auto result = maySetToNullBeforeEvaluating(var, ifStmt.getThenBody())) return *result;
            if (auto result = maySetToNullBeforeEvaluating(var, ifStmt.getElseBody())) return *result;
            return llvm::None;
        }
        case StmtKind::SwitchStmt:
            return true;

        case StmtKind::WhileStmt: {
            auto& whileStmt = llvm::cast<WhileStmt>(stmt);
            if (auto result = maySetToNullBeforeEvaluating(var, whileStmt.getCondition())) return *result;
            if (auto result = maySetToNullBeforeEvaluating(var, whileStmt.getBody())) return *result;
            return llvm::None;
        }
        case StmtKind::ForStmt:
            llvm_unreachable("ForStmt should be lowered into a WhileStmt");

        case StmtKind::BreakStmt:
        case StmtKind::ContinueStmt:
            return false;

        case StmtKind::CompoundStmt:
            return maySetToNullBeforeEvaluating(var, llvm::cast<CompoundStmt>(stmt).getBody());
    }

    llvm_unreachable("all cases handled");
}

llvm::Optional<bool> Typechecker::subExprMaySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const {
    for (auto* subExpr : expr.getSubExprs()) {
        if (auto result = maySetToNullBeforeEvaluating(var, *subExpr)) return *result;
    }
    return llvm::None;
}

llvm::Optional<bool> Typechecker::maySetToNullBeforeEvaluating(const Expr& var, const Expr& expr) const {
    if (&expr == &var) return false;
    if (auto result = subExprMaySetToNullBeforeEvaluating(var, expr)) return *result;
    if (expr.isCallExpr()) return true;
    if (expr.isAssignment()) {
        auto* lhs = &llvm::cast<BinaryExpr>(expr).getLHS();
        auto* rhs = &llvm::cast<BinaryExpr>(expr).getRHS();

        if (auto result = maySetToNullBeforeEvaluating(var, *lhs)) return *result;
        if (auto result = maySetToNullBeforeEvaluating(var, *rhs)) return *result;

        switch (var.getKind()) {
            case ExprKind::VarExpr:
                if (auto* lhsVarExpr = llvm::dyn_cast<VarExpr>(lhs)) {
                    return lhsVarExpr->getDecl() == llvm::cast<VarExpr>(var).getDecl() && rhs->getType().isOptionalType();
                }
                return llvm::None;

            case ExprKind::MemberExpr:
                if (auto* lhsMemberExpr = llvm::dyn_cast<MemberExpr>(lhs)) {
                    if (auto result = memberExprChainsMatch(*lhsMemberExpr, llvm::cast<MemberExpr>(var))) {
                        return *result && rhs->getType().isOptionalType();
                    }
                }
                return llvm::None;

            default:
                llvm_unreachable("unsupported variable expression kind");
        }
    }
    return llvm::None;
}

llvm::Optional<bool> Typechecker::maySetToNullBeforeEvaluating(const Expr& var,
                                                               llvm::ArrayRef<std::unique_ptr<Stmt>> block) const {
    for (auto& stmt : block) {
        if (auto result = maySetToNullBeforeEvaluating(var, *stmt)) return *result;
    }
    return llvm::None;
}

void Typechecker::typecheckStmt(std::unique_ptr<Stmt>& stmt) {
    switch (stmt->getKind()) {
        case StmtKind::ReturnStmt:
            typecheckReturnStmt(llvm::cast<ReturnStmt>(*stmt));
            break;
        case StmtKind::VarStmt:
            typecheckVarStmt(llvm::cast<VarStmt>(*stmt));
            break;
        case StmtKind::ExprStmt:
            typecheckExpr(llvm::cast<ExprStmt>(*stmt).getExpr());
            break;
        case StmtKind::DeferStmt:
            typecheckExpr(llvm::cast<DeferStmt>(*stmt).getExpr());
            break;
        case StmtKind::IfStmt:
            typecheckIfStmt(llvm::cast<IfStmt>(*stmt));
            break;
        case StmtKind::SwitchStmt:
            typecheckSwitchStmt(llvm::cast<SwitchStmt>(*stmt));
            break;
        case StmtKind::WhileStmt:
            typecheckWhileStmt(llvm::cast<WhileStmt>(*stmt));
            break;
        case StmtKind::ForStmt:
            typecheckExpr(llvm::cast<ForStmt>(*stmt).getRangeExpr());
            stmt = llvm::cast<ForStmt>(*stmt).lower();
            typecheckStmt(stmt);
            break;
        case StmtKind::BreakStmt:
            typecheckBreakStmt(llvm::cast<BreakStmt>(*stmt));
            break;
        case StmtKind::ContinueStmt:
            typecheckContinueStmt(llvm::cast<ContinueStmt>(*stmt));
            break;
        case StmtKind::CompoundStmt:
            typecheckCompoundStmt(llvm::cast<CompoundStmt>(*stmt));
            break;
    }
}

void Typechecker::typecheckType(Type type, AccessLevel userAccessLevel) {
    switch (type.getKind()) {
        case TypeKind::BasicType: {
            if (type.isBuiltinType()) {
                validateGenericArgCount(0, type.getGenericArgs(), type.getName(), type.getLocation());
                break;
            }

            auto* basicType = llvm::cast<BasicType>(type.getBase());

            for (auto genericArg : basicType->getGenericArgs()) {
                typecheckType(genericArg, userAccessLevel);
            }

            auto decls = getCurrentModule()->findDecls(basicType->getQualifiedName(), currentSourceFile, currentFunction);
            Decl* decl;

            if (decls.empty()) {
                auto decls = getCurrentModule()->findDecls(basicType->getName(), currentSourceFile, currentFunction);

                if (decls.empty()) {
                    error(type.getLocation(), "unknown type '", type, "'");
                }

                decl = decls[0];
                auto typeTemplate = llvm::cast<TypeTemplate>(decl)->instantiate(basicType->getGenericArgs());
                getCurrentModule()->addToSymbolTable(*typeTemplate);
                typecheckTypeDecl(*typeTemplate);
            } else {
                decl = decls[0];

                switch (decl->getKind()) {
                    case DeclKind::TypeDecl:
                        if (auto* deinitDecl = llvm::cast<TypeDecl>(decl)->getDeinitializer()) {
                            typecheckFunctionDecl(*deinitDecl);
                        }
                        break;
                    case DeclKind::TypeTemplate:
                        validateGenericArgCount(llvm::cast<TypeTemplate>(decl)->getGenericParams().size(),
                                                basicType->getGenericArgs(), basicType->getName(), type.getLocation());
                        break;
                    default:
                        break;
                }
            }

            checkHasAccess(*decl, type.getLocation(), userAccessLevel);
            break;
        }
        case TypeKind::ArrayType:
            typecheckType(type.getElementType(), userAccessLevel);
            break;
        case TypeKind::TupleType:
            for (auto& element : type.getTupleElements()) {
                typecheckType(element.type, userAccessLevel);
            }
            break;
        case TypeKind::FunctionType:
            for (auto paramType : type.getParamTypes()) {
                typecheckType(paramType, userAccessLevel);
            }
            typecheckType(type.getReturnType(), userAccessLevel);
            break;
        case TypeKind::PointerType: {
            if (type.getPointee().isArrayWithRuntimeSize()) {
                auto qualifiedTypeName = getQualifiedTypeName("ArrayRef", type.getPointee().getElementType());
                if (getCurrentModule()->findDecls(qualifiedTypeName, currentSourceFile, currentFunction).empty()) {
                    auto& arrayRefDecl = getCurrentModule()->findDecl("ArrayRef", SourceLocation(), currentSourceFile,
                                                                      currentFieldDecls);
                    auto& arrayRef = llvm::cast<TypeTemplate>(arrayRefDecl);
                    auto* instantiation = arrayRef.instantiate({ type.getPointee().getElementType() });
                    getCurrentModule()->addToSymbolTable(*instantiation);
                    declsToTypecheck.push_back(instantiation);
                }
            } else {
                typecheckType(type.getPointee(), userAccessLevel);
            }
            break;
        }
        case TypeKind::OptionalType:
            typecheckType(type.getWrappedType(), userAccessLevel);
            break;
    }
}

void Typechecker::typecheckParamDecl(ParamDecl& decl, AccessLevel userAccessLevel) {
    if (getCurrentModule()->getSymbolTable().containsInCurrentScope(decl.getName())) {
        error(decl.getLocation(), "redefinition of '", decl.getName(), "'");
    }

    if (decl.getType().isMutable()) {
        error(decl.getLocation(), "parameter types cannot be 'mutable'");
    }

    typecheckType(decl.getType(), userAccessLevel);
    getCurrentModule()->getSymbolTable().add(decl.getName(), &decl);
}

bool allPathsReturn(llvm::ArrayRef<std::unique_ptr<Stmt>> block) {
    if (block.empty()) return false;

    switch (block.back()->getKind()) {
        case StmtKind::ReturnStmt:
            return true;
        case StmtKind::IfStmt: {
            auto& ifStmt = llvm::cast<IfStmt>(*block.back());
            return allPathsReturn(ifStmt.getThenBody()) && allPathsReturn(ifStmt.getElseBody());
        }
        case StmtKind::SwitchStmt: {
            auto& switchStmt = llvm::cast<SwitchStmt>(*block.back());
            return llvm::all_of(switchStmt.getCases(), [](auto& c) { return allPathsReturn(c.getStmts()); }) &&
                   allPathsReturn(switchStmt.getDefaultStmts());
        }
        default:
            return false;
    }
}

void Typechecker::typecheckGenericParamDecls(llvm::ArrayRef<GenericParamDecl> genericParams, AccessLevel userAccessLevel) {
    for (auto& genericParam : genericParams) {
        if (getCurrentModule()->getSymbolTable().contains(genericParam.getName())) {
            error(genericParam.getLocation(), "redefinition of '", genericParam.getName(), "'");
        }

        for (Type constraint : genericParam.getConstraints()) {
            typecheckType(constraint, userAccessLevel);
        }
    }
}

void Typechecker::typecheckParams(llvm::MutableArrayRef<ParamDecl> params, AccessLevel userAccessLevel) {
    for (auto& param : params) {
        typecheckParamDecl(param, userAccessLevel);
    }
}

void Typechecker::typecheckFunctionDecl(FunctionDecl& decl) {
    if (decl.isTypechecked()) return;
    if (decl.isExtern()) return; // TODO: Typecheck parameters and return type of extern functions.

    TypeDecl* receiverTypeDecl = decl.getTypeDecl();

    getCurrentModule()->getSymbolTable().pushScope();
    SAVE_STATE(currentFunction);
    currentFunction = &decl;

    typecheckParams(decl.getParams(), decl.getAccessLevel());

    if (!decl.isInitDecl() && !decl.isDeinitDecl()) {
        typecheckType(decl.getReturnType(), decl.getAccessLevel());
        if (decl.getReturnType().isMutable()) error(decl.getLocation(), "return types cannot be 'mutable'");
    }

    if (!decl.isExtern()) {
        SAVE_STATE(functionReturnType);
        functionReturnType = decl.getReturnType();
        SAVE_STATE(currentFieldDecls);
        if (receiverTypeDecl) {
            for (auto& field : receiverTypeDecl->getFields()) {
                currentFieldDecls.emplace_back(&field, false);
            }

            Type thisType = receiverTypeDecl->getTypeForPassing(decl.isMutating());
            getCurrentModule()->addToSymbolTable(
                VarDecl(thisType, "this", nullptr, &decl, AccessLevel::None, *getCurrentModule(), decl.getLocation()));
        }

        bool delegatedInit = false;

        if (decl.hasBody()) {
            for (auto& stmt : decl.getBody()) {
                typecheckStmt(stmt);

                if (!decl.isInitDecl()) continue;

                if (auto* exprStmt = llvm::dyn_cast<ExprStmt>(stmt.get())) {
                    if (auto* callExpr = llvm::dyn_cast<CallExpr>(&exprStmt->getExpr())) {
                        if (auto* initDecl = llvm::dyn_cast_or_null<InitDecl>(callExpr->getCalleeDecl())) {
                            if (initDecl->getTypeDecl() == receiverTypeDecl) {
                                delegatedInit = true;
                            }
                        }
                    }
                }
            }
        }

        if (decl.isInitDecl() && !delegatedInit) {
            for (auto& fieldAndInitialized : currentFieldDecls) {
                if (!fieldAndInitialized.second) {
                    warning(decl.getLocation(), "initializer doesn't initialize member variable '",
                            fieldAndInitialized.first->getName(), "'");
                }
            }
        }
    }

    getCurrentModule()->getSymbolTable().popScope();

    if ((!receiverTypeDecl || !receiverTypeDecl->isInterface()) && !decl.getReturnType().isVoid() &&
        !allPathsReturn(decl.getBody())) {
        error(decl.getLocation(), "'", decl.getName(), "' is missing a return statement");
    }

    decl.setTypechecked(true);
}

void Typechecker::typecheckFunctionTemplate(FunctionTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.getAccessLevel());
}

void Typechecker::typecheckTypeDecl(TypeDecl& decl) {
    for (Type interface : decl.getInterfaces()) {
        if (interface.isBasicType()) {
            if (auto* interfaceDecl = getTypeDecl(llvm::cast<BasicType>(*interface))) {
                if (interfaceDecl->isInterface()) {
                    std::string errorReason;
                    if (!providesInterfaceRequirements(decl, *interfaceDecl, &errorReason)) {
                        error(decl.getLocation(), "'", decl.getName(), "' ", errorReason, " required by interface '",
                              interfaceDecl->getName(), "'");
                    }

                    for (auto& method : interfaceDecl->getMethods()) {
                        auto& methodDecl = llvm::cast<MethodDecl>(*method);

                        if (methodDecl.hasBody()) {
                            auto copy = methodDecl.instantiate({ { "This", decl.getType() } }, decl);
                            getCurrentModule()->addToSymbolTable(*copy);
                            decl.addMethod(std::move(copy));
                        }
                    }

                    continue;
                }
            }
        }
        error(decl.getLocation(), "'", interface, "' is not an interface");
    }

    TypeDecl* realDecl;
    std::unique_ptr<TypeDecl> thisTypeResolved;

    if (decl.isInterface()) {
        thisTypeResolved = llvm::cast<TypeDecl>(decl.instantiate({ { "This", decl.getType() } }, {}));
        realDecl = thisTypeResolved.get();
    } else {
        realDecl = &decl;
    }

    for (auto& fieldDecl : realDecl->getFields()) {
        typecheckFieldDecl(fieldDecl);
    }

    for (auto& memberDecl : realDecl->getMemberDecls()) {
        typecheckMemberDecl(*memberDecl);
    }
}

void Typechecker::typecheckTypeTemplate(TypeTemplate& decl) {
    typecheckGenericParamDecls(decl.getGenericParams(), decl.getAccessLevel());
}

void Typechecker::typecheckEnumDecl(EnumDecl& decl) {
    std::vector<const EnumCase*> cases = map(decl.getCases(), [](const EnumCase& c) { return &c; });

    std::sort(cases.begin(), cases.end(),
              [](const EnumCase* a, const EnumCase* b) { return a->getName() < b->getName(); });

    auto it = std::adjacent_find(cases.begin(), cases.end(),
                                 [](const EnumCase* a, const EnumCase* b) { return a->getName() == b->getName(); });

    if (it != cases.end()) {
        error((*it)->getLocation(), "duplicate enum case '", (*it)->getName(), "'");
    }

    for (auto& enumCase : decl.getCases()) {
        typecheckExpr(*enumCase.getValue());
    }
}

TypeDecl* Typechecker::getTypeDecl(const BasicType& type) {
    if (auto* typeDecl = type.getDecl()) {
        return typeDecl;
    }

    auto decls = getCurrentModule()->findDecls(type.getQualifiedName(), currentSourceFile, currentFunction);

    if (!decls.empty()) {
        ASSERT(decls.size() == 1);
        return llvm::dyn_cast_or_null<TypeDecl>(decls[0]);
    }

    decls = getCurrentModule()->findDecls(type.getName(), currentSourceFile, currentFunction);
    if (decls.empty()) return nullptr;
    ASSERT(decls.size() == 1);
    auto instantiation = llvm::cast<TypeTemplate>(decls[0])->instantiate(type.getGenericArgs());
    getCurrentModule()->addToSymbolTable(*instantiation);
    declsToTypecheck.push_back(instantiation);
    return instantiation;
}

void Typechecker::typecheckVarDecl(VarDecl& decl, bool isGlobal) {
    if (!isGlobal && getCurrentModule()->getSymbolTable().contains(decl.getName())) {
        error(decl.getLocation(), "redefinition of '", decl.getName(), "'");
    }
    if (isGlobal && decl.getInitializer()->isUndefinedLiteralExpr()) {
        error(decl.getLocation(), "global variables cannot be uninitialized");
    }

    Type declaredType = decl.getType();
    Type initType = typecheckExpr(*decl.getInitializer());

    if (declaredType) {
        bool isLocalVariable = decl.getParent() && decl.getParent()->isFunctionDecl();
        typecheckType(declaredType, isLocalVariable ? AccessLevel::None : decl.getAccessLevel());

        if (initType) {
            Type convertedType;

            if (isImplicitlyConvertible(decl.getInitializer(), initType, declaredType, &convertedType)) {
                decl.getInitializer()->setType(convertedType ? convertedType : initType);
            } else {
                const char* hint;

                if (initType.isNull()) {
                    ASSERT(!declaredType.isOptionalType());
                    hint = " (add '?' to the type to make it nullable)";
                } else {
                    hint = "";
                }

                error(decl.getInitializer()->getLocation(), "cannot initialize variable of type '", declaredType,
                      "' with '", initType, "'", hint);
            }
        }
    } else {
        if (initType.isNull()) {
            error(decl.getLocation(), "couldn't infer type of '", decl.getName(), "', add a type annotation");
        }

        initType.setMutable(decl.getType().isMutable());
        decl.setType(initType);
    }

    if (!isGlobal) {
        getCurrentModule()->addToSymbolTable(decl, false);
    }

    if (!decl.getType().isImplicitlyCopyable()) {
        checkCanMove(*decl.getInitializer());
        decl.getInitializer()->setMoved(true);
    }
}

void Typechecker::typecheckFieldDecl(FieldDecl& decl) {
    typecheckType(decl.getType(), std::min(decl.getAccessLevel(), decl.getParent()->getAccessLevel()));
}

static std::error_code parseSourcesInDirectoryRecursively(llvm::StringRef directoryPath, Module& module,
                                                          llvm::ArrayRef<std::string> importSearchPaths,
                                                          llvm::ArrayRef<std::string> frameworkSearchPaths) {
    std::error_code error;
    llvm::sys::fs::recursive_directory_iterator it(directoryPath, error), end;

    for (; it != end; it.increment(error)) {
        if (error) break;
        if (llvm::sys::path::extension(it->path()) == ".delta") {
            Parser parser(it->path(), module, importSearchPaths, frameworkSearchPaths);
            parser.parse();
        }
    }

    return error;
}

llvm::ErrorOr<const Module&> Typechecker::importDeltaModule(SourceFile* importer, const PackageManifest* manifest,
                                                            llvm::ArrayRef<std::string> importSearchPaths,
                                                            llvm::ArrayRef<std::string> frameworkSearchPaths,
                                                            llvm::StringRef moduleExternalName,
                                                            llvm::StringRef moduleInternalName) {
    if (moduleInternalName.empty()) moduleInternalName = moduleExternalName;

    auto it = Module::getAllImportedModulesMap().find(moduleInternalName);
    if (it != Module::getAllImportedModulesMap().end()) {
        if (importer) importer->addImportedModule(it->second);
        return *it->second;
    }

    auto module = std::make_shared<Module>(moduleInternalName);
    std::error_code error;

    if (manifest) {
        for (auto& dependency : manifest->getDeclaredDependencies()) {
            if (dependency.getPackageIdentifier() == moduleInternalName) {
                error = parseSourcesInDirectoryRecursively(dependency.getFileSystemPath(), *module, importSearchPaths,
                                                           frameworkSearchPaths);
                goto done;
            }
        }
    }

    for (llvm::StringRef importPath : importSearchPaths) {
        llvm::sys::fs::directory_iterator it(importPath, error), end;
        for (; it != end; it.increment(error)) {
            if (error) goto done;
            if (!llvm::sys::fs::is_directory(it->path())) continue;
            if (llvm::sys::path::filename(it->path()) != moduleExternalName) continue;

            error = parseSourcesInDirectoryRecursively(it->path(), *module, importSearchPaths, frameworkSearchPaths);
            goto done;
        }
    }

done:
    if (error || module->getSourceFiles().empty()) {
        return error;
    }

    if (importer) importer->addImportedModule(module);
    Module::getAllImportedModulesMap()[module->getName()] = module;
    typecheckModule(*module, /* TODO: Pass the package manifest of `module` here. */ nullptr, importSearchPaths,
                    frameworkSearchPaths);
    return *module;
}

void Typechecker::typecheckImportDecl(ImportDecl& decl, const PackageManifest* manifest,
                                      llvm::ArrayRef<std::string> importSearchPaths,
                                      llvm::ArrayRef<std::string> frameworkSearchPaths) {
    if (importDeltaModule(currentSourceFile, manifest, importSearchPaths, frameworkSearchPaths, decl.getTarget())) {
        return;
    }

    if (!importCHeader(*currentSourceFile, decl.getTarget(), importSearchPaths, frameworkSearchPaths)) {
        error(decl.getLocation(), "couldn't find module or C header '", decl.getTarget(), "'");
    }
}

void Typechecker::typecheckTopLevelDecl(Decl& decl, const PackageManifest* manifest,
                                        llvm::ArrayRef<std::string> importSearchPaths,
                                        llvm::ArrayRef<std::string> frameworkSearchPaths) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::FunctionDecl:
            typecheckFunctionDecl(llvm::cast<FunctionDecl>(decl));
            break;
        case DeclKind::MethodDecl:
            llvm_unreachable("no top-level method declarations");
        case DeclKind::GenericParamDecl:
            llvm_unreachable("no top-level parameter declarations");
        case DeclKind::InitDecl:
            llvm_unreachable("no top-level initializer declarations");
        case DeclKind::DeinitDecl:
            llvm_unreachable("no top-level deinitializer declarations");
        case DeclKind::FunctionTemplate:
            typecheckFunctionTemplate(llvm::cast<FunctionTemplate>(decl));
            break;
        case DeclKind::TypeDecl:
            typecheckTypeDecl(llvm::cast<TypeDecl>(decl));
            break;
        case DeclKind::TypeTemplate:
            typecheckTypeTemplate(llvm::cast<TypeTemplate>(decl));
            break;
        case DeclKind::EnumDecl:
            typecheckEnumDecl(llvm::cast<EnumDecl>(decl));
            break;
        case DeclKind::VarDecl:
            typecheckVarDecl(llvm::cast<VarDecl>(decl), true);
            break;
        case DeclKind::FieldDecl:
            llvm_unreachable("no top-level field declarations");
        case DeclKind::ImportDecl:
            typecheckImportDecl(llvm::cast<ImportDecl>(decl), manifest, importSearchPaths, frameworkSearchPaths);
            break;
    }
}

void Typechecker::typecheckMemberDecl(Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::MethodDecl:
        case DeclKind::InitDecl:
        case DeclKind::DeinitDecl:
            typecheckFunctionDecl(llvm::cast<MethodDecl>(decl));
            break;
        case DeclKind::FieldDecl:
            typecheckFieldDecl(llvm::cast<FieldDecl>(decl));
            break;
        case DeclKind::FunctionTemplate:
            typecheckFunctionTemplate(llvm::cast<FunctionTemplate>(decl));
            break;
        default:
            llvm_unreachable("invalid member declaration kind");
    }
}

void Typechecker::postProcess() {
    SAVE_STATE(isPostProcessing);
    isPostProcessing = true;

    while (!declsToTypecheck.empty()) {
        auto currentDeclsToTypecheck = std::move(declsToTypecheck);

        for (auto* decl : currentDeclsToTypecheck) {
            switch (decl->getKind()) {
                case DeclKind::FunctionDecl:
                case DeclKind::MethodDecl:
                case DeclKind::InitDecl:
                case DeclKind::DeinitDecl:
                    typecheckFunctionDecl(*llvm::cast<FunctionDecl>(decl));
                    break;
                case DeclKind::FunctionTemplate:
                    typecheckFunctionTemplate(*llvm::cast<FunctionTemplate>(decl));
                    break;
                case DeclKind::TypeDecl:
                    typecheckTypeDecl(*llvm::cast<TypeDecl>(decl));
                    break;
                default:
                    llvm_unreachable("invalid deferred decl");
            }
        }
    }
}

static void checkUnusedDecls(const Module& module) {
    for (auto& sourceFile : module.getSourceFiles()) {
        for (auto& decl : sourceFile.getTopLevelDecls()) {
            if (decl->isReferenced()) continue;

            if (decl->isFunctionDecl() || decl->isFunctionTemplate()) {
                if (decl->isMain()) continue;
                warning(decl->getLocation(), "unused declaration '", decl->getName(), "'");
            }
        }
    }
}

void Typechecker::typecheckModule(Module& module, const PackageManifest* manifest,
                                  llvm::ArrayRef<std::string> importSearchPaths,
                                  llvm::ArrayRef<std::string> frameworkSearchPaths) {
    auto stdlibModule = importDeltaModule(nullptr, nullptr, importSearchPaths, frameworkSearchPaths, "stdlib", "std");
    if (!stdlibModule) {
        printErrorAndExit("couldn't import the standard library: ", stdlibModule.getError().message());
    }

    // Infer the types of global variables for use before their declaration.
    for (auto& sourceFile : module.getSourceFiles()) {
        currentModule = &module;
        currentSourceFile = &sourceFile;

        for (auto& decl : sourceFile.getTopLevelDecls()) {
            if (auto* varDecl = llvm::dyn_cast<VarDecl>(decl.get())) {
                typecheckVarDecl(*varDecl, true);
            }
        }

        postProcess();
    }

    for (auto& sourceFile : module.getSourceFiles()) {
        for (auto& decl : sourceFile.getTopLevelDecls()) {
            currentModule = &module;
            currentSourceFile = &sourceFile;

            if (!decl->isVarDecl()) {
                typecheckTopLevelDecl(*decl, manifest, importSearchPaths, frameworkSearchPaths);
                postProcess();
            }
        }
    }

    if (module.getName() != "std") {
        checkUnusedDecls(module);
    }

    currentModule = nullptr;
    currentSourceFile = nullptr;
}
