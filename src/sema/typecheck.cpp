#include <iostream>
#include <limits>
#include <unordered_map>
#include <boost/utility/string_ref.hpp>
#include <boost/optional.hpp>
#include "typecheck.h"
#include "../ast/type.h"
#include "../ast/expr.h"
#include "../ast/decl.h"

static std::unordered_map<std::string, Type> symbolTable;

template<typename... Args>
[[noreturn]] static void error(Args&&... args) {
    std::cout << "error: ";
    using expander = int[];
    (void)expander{0, (void(std::cout << std::forward<Args>(args)), 0)...};
    std::cout << '\n';
    exit(1);
}

const Type& typecheck(Expr& expr);

Type typecheck(VariableExpr& expr) {
    auto it = symbolTable.find(expr.identifier);
    if (it == symbolTable.end()) {
        error("unknown identifier '", expr.identifier, "'");
    }
    return it->second;
}

Type typecheck(IntLiteralExpr& expr) {
    if (expr.value >= std::numeric_limits<int32_t>::min()
    &&  expr.value <= std::numeric_limits<int32_t>::max())
        return Type("int");
    else
    if (expr.value >= std::numeric_limits<int64_t>::min()
    &&  expr.value <= std::numeric_limits<int64_t>::max())
        return Type("int64");
    else
        error("integer literal is too large");
}

Type typecheck(BoolLiteralExpr&) {
    return Type("bool");
}

Type typecheck(PrefixExpr& expr) {
    return typecheck(*expr.operand);
}

Type typecheck(BinaryExpr& expr) {
    Type leftType = typecheck(*expr.left);
    Type rightType = typecheck(*expr.right);
    if (leftType != rightType) {
        error("operands to binary expression must have same type");
    }
    return leftType;
}

Type typecheck(CallExpr& expr) {
    auto it = symbolTable.find(expr.funcName);
    if (it == symbolTable.end()) {
        error("unknown function '", expr.funcName, "'");
    }
    if (it->second.getKind() != TypeKind::FuncType) {
        error("'", expr.funcName, "' is not a function");
    }
    const auto& params = it->second.getFuncType().paramTypes;
    if (expr.args.size() < params.size()) {
        error("too few arguments to '", expr.funcName, "', expected ", params.size());
    }
    if (expr.args.size() > params.size()) {
        error("too many arguments to '", expr.funcName, "', expected ", params.size());
    }
    for (int i = 0; i < params.size(); ++i) {
        auto argType = typecheck(expr.args[i]);
        if (argType != params[i]) {
            error("invalid argument #", i + 1, " type '", argType, "' to '",
                expr.funcName, "', expected '", params[i], "'");
        }
    }
    return Type(it->second.getFuncType().returnTypes);
}

const Type& typecheck(Expr& expr) {
    boost::optional<Type> type;
    switch (expr.getKind()) {
        case ExprKind::VariableExpr:    type = typecheck(expr.getVariableExpr()); break;
        case ExprKind::IntLiteralExpr:  type = typecheck(expr.getIntLiteralExpr()); break;
        case ExprKind::BoolLiteralExpr: type = typecheck(expr.getBoolLiteralExpr()); break;
        case ExprKind::PrefixExpr:      type = typecheck(expr.getPrefixExpr()); break;
        case ExprKind::BinaryExpr:      type = typecheck(expr.getBinaryExpr()); break;
        case ExprKind::CallExpr:        type = typecheck(expr.getCallExpr()); break;
    }
    expr.setType(std::move(*type));
    return expr.getType();
}

void typecheck(ReturnStmt& stmt) {
    for (Expr& expr : stmt.values) typecheck(expr);
}

void typecheck(VarDecl& decl);

void typecheck(VariableStmt& stmt) {
    typecheck(*stmt.decl);
}

void typecheck(IncrementStmt& stmt) {
    typecheck(stmt.operand);
    // TODO: check that operand supports increment operation.
}

void typecheck(DecrementStmt& stmt) {
    typecheck(stmt.operand);
    // TODO: check that operand supports decrement operation.
}

void typecheck(Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt:    typecheck(stmt.getReturnStmt()); break;
        case StmtKind::VariableStmt:  typecheck(stmt.getVariableStmt()); break;
        case StmtKind::IncrementStmt: typecheck(stmt.getIncrementStmt()); break;
        case StmtKind::DecrementStmt: typecheck(stmt.getDecrementStmt()); break;
        case StmtKind::CallStmt:      typecheck(stmt.getCallStmt().expr); break;
    }
}

void typecheck(ParamDecl& decl) {
    if (symbolTable.count(decl.name) > 0) {
        error("redefinition of '", decl.name, "'");
    }
    symbolTable.insert({decl.name, Type(decl.type)});
}

static std::vector<Type> mapToTypes(const std::vector<ParamDecl>& params) {
    std::vector<Type> paramTypes;
    paramTypes.reserve(params.size());
    for (const auto& param : params) paramTypes.emplace_back(param.type);
    return paramTypes;
}

void addToSymbolTable(const FuncDecl& decl) {
    if (symbolTable.count(decl.name) > 0) {
        error("redefinition of '", decl.name, "'");
    }
    auto returnTypes = decl.returnType.isTuple()
        ? decl.returnType.getNames() : std::vector<Type>{Type(decl.returnType.getName())};
    symbolTable.insert({decl.name, Type(FuncType{returnTypes, mapToTypes(decl.params)})});
}

void typecheck(FuncDecl& decl) {
    auto symbolTableBackup = symbolTable;
    for (ParamDecl& param : decl.params) typecheck(param);
    for (Stmt& stmt : decl.body) typecheck(stmt);
    symbolTable = symbolTableBackup;
}

void typecheck(VarDecl& decl) {
    if (symbolTable.count(decl.name) > 0) {
        error("redefinition of '", decl.name, "'");
    }
    symbolTable.insert({decl.name, typecheck(decl.initializer)});
}

void typecheck(Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl: typecheck(decl.getParamDecl()); break;
        case DeclKind::FuncDecl:  typecheck(decl.getFuncDecl()); break;
        case DeclKind::VarDecl:   typecheck(decl.getVarDecl()); break;
    }
}

void typecheck(std::vector<Decl>& decls) {
    for (Decl& decl : decls) typecheck(decl);
}