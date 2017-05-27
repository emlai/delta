#include <algorithm> // std::equal
#include <limits>
#include <unordered_map>
#include <vector>
#include <cstdlib>
#include <boost/numeric/conversion/cast.hpp>
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/Optional.h>
#include <llvm/ADT/StringRef.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/ADT/STLExtras.h>
#include <llvm/ADT/iterator_range.h>
#include <llvm/Support/ErrorHandling.h>
#include "typecheck.h"
#include "c-import.h"
#include "../ast/type.h"
#include "../ast/expr.h"
#include "../ast/decl.h"
#include "../ast/module.h"
#include "../ast/token.h"
#include "../ast/mangle.h"
#include "../driver/utility.h"

using namespace delta;

namespace {

class SymbolTable {
public:
    SymbolTable() {
        pushScope(); // The global scope.
    }

    void pushScope() {
        scopes.emplace_back();
    }

    void popScope() {
        scopes.pop_back();
    }

    llvm::ArrayRef<Decl*> find(const std::string& name) const {
        for (const auto& scope : llvm::reverse(scopes)) {
            auto it = scope.find(name);
            if (it != scope.end()) return it->second;
        }
        return {};
    }

    template<typename T>
    T* findWithMatchingParams(const T& toFind) const {
        for (Decl* decl : find(mangle(toFind))) {
            T* t = llvm::dyn_cast<T>(decl);
            if (!t || t->params.size() != toFind.params.size()) continue;
            if (std::equal(toFind.params.begin(), toFind.params.end(), t->params.begin(),
                           [](const ParamDecl& a, const ParamDecl& b) { return a.name == b.name; }))
                return t;
        }
        return nullptr;
    }

    bool contains(const std::string& name) const {
        return !find(name).empty();
    }

    void add(std::string name, Decl* decl) {
        scopes.back()[std::move(name)].push_back(decl);
    }

private:
    std::vector<std::unordered_map<std::string, llvm::SmallVector<Decl*, 1>>> scopes;
};

SymbolTable symbolTable;

/// Storage for Decls that are not in the AST but are referenced by the symbol table.
std::vector<std::unique_ptr<Decl>> nonASTDecls;

std::vector<VarDecl*> globalVariables;
std::unordered_map<std::string, Type> currentGenericArgs;
Type funcReturnType = nullptr;
bool inInitializer = false;
int breakableBlocks = 0;
bool importingC = false;
llvm::ArrayRef<llvm::StringRef> includePaths;

Type typecheck(Expr& expr);
void typecheck(Stmt& stmt);
void typecheck(GenericFuncDecl& decl);
Type typecheck(CallExpr& expr);

Type typecheck(VariableExpr& expr) {
    Decl& decl = findInSymbolTable(expr.identifier, expr.srcLoc);

    switch (decl.getKind()) {
        case DeclKind::VarDecl: return decl.getVarDecl().getType();
        case DeclKind::ParamDecl: return decl.getParamDecl().type;
        case DeclKind::FuncDecl: return decl.getFuncDecl().getFuncType();
        case DeclKind::GenericParamDecl: llvm_unreachable("cannot refer to generic parameters yet");
        case DeclKind::GenericFuncDecl: llvm_unreachable("cannot refer to generic functions yet");
        case DeclKind::InitDecl: llvm_unreachable("cannot refer to initializers yet");
        case DeclKind::DeinitDecl: llvm_unreachable("cannot refer to deinitializers yet");
        case DeclKind::TypeDecl: error(expr.srcLoc, "'", expr.identifier, "' is not a variable");
        case DeclKind::FieldDecl: return decl.getFieldDecl().type;
        case DeclKind::ImportDecl: llvm_unreachable("import statement validation not implemented yet");
    }
    abort();
}

Type typecheck(StrLiteralExpr&) {
    return Type::getString();
}

Type typecheck(IntLiteralExpr& expr) {
    if (expr.value >= std::numeric_limits<int32_t>::min()
    &&  expr.value <= std::numeric_limits<int32_t>::max())
        return Type::getInt();
    else
    if (expr.value >= std::numeric_limits<int64_t>::min()
    &&  expr.value <= std::numeric_limits<int64_t>::max())
        return Type::getInt64();
    else
        error(expr.srcLoc, "integer literal is too large");
}

Type typecheck(FloatLiteralExpr&) {
    return Type::getFloat64();
}

Type typecheck(BoolLiteralExpr&) {
    return Type::getBool();
}

Type typecheck(NullLiteralExpr&) {
    return Type::getNull();
}

Type typecheck(ArrayLiteralExpr& array) {
    Type firstType = typecheck(*array.elements[0]);
    for (auto& element : llvm::drop_begin(array.elements, 1)) {
        Type type = typecheck(*element);
        if (type != firstType) {
            error(element->getSrcLoc(), "mixed element types in array literal (expected '",
                  firstType, "', found '", type, "')");
        }
    }
    return ArrayType::get(firstType, int64_t(array.elements.size()));
}

Type typecheck(PrefixExpr& expr) {
    Type operandType = typecheck(expr.getOperand());

    if (expr.op == NOT) {
        if (!operandType.isBool()) {
            error(expr.getOperand().getSrcLoc(), "invalid operand type '", operandType, "' to logical not");
        }
        return operandType;
    }
    if (expr.op == STAR) { // Dereference operation
        if (!operandType.isPtrType()) {
            error(expr.getOperand().getSrcLoc(), "cannot dereference non-pointer type '", operandType, "'");
        }
        return operandType.getPointee();
    }
    if (expr.op == AND) { // Address-of operation
        return PtrType::get(operandType, true);
    }
    return operandType;
}

bool isValidConversion(Expr&, Type, Type);

Type typecheck(BinaryExpr& expr) {
    Type leftType = typecheck(expr.getLHS());
    Type rightType = typecheck(expr.getRHS());

    if (!expr.isBuiltinOp()) return typecheck((CallExpr&) expr);

    if (expr.op == AND_AND || expr.op == OR_OR) {
        if (leftType.isBool() && rightType.isBool()) {
            return Type::getBool();
        }
        error(expr.srcLoc, "invalid operands to binary expression ('", leftType, "' and '", rightType, "')");
    }

    if (expr.op.isBitwiseOperator() && (leftType.isFloatingPoint() || rightType.isFloatingPoint())) {
        error(expr.srcLoc, "invalid operands to binary expression ('", leftType, "' and '", rightType, "')");
    }

    if (!isValidConversion(expr.getLHS(), leftType, rightType)
    &&  !isValidConversion(expr.getRHS(), rightType, leftType)) {
        error(expr.srcLoc, "invalid operands to binary expression ('", leftType, "' and '", rightType, "')");
    }

    if (expr.op == DOTDOTDOT) return RangeType::get(leftType);

    return expr.op.isComparisonOperator() ? Type::getBool() : leftType;
}

TypeDecl* getTypeDecl(const BasicType& type);

template<typename IntType>
bool checkRange(Expr& expr, int64_t value, llvm::StringRef param) {
    try {
        boost::numeric_cast<IntType>(value);
    } catch (...) {
        error(expr.getSrcLoc(), value, " is out of range for type '", param, "'");
    }
    expr.setType(BasicType::get(param.str()));
    return true;
}

/// Resolves generic parameters to their corresponding types, returns other types as is.
Type resolve(Type type) {
    if (!type.isBasicType()) return type;
    auto it = currentGenericArgs.find(type.getName());
    if (it == currentGenericArgs.end()) return type;
    return it->second;
}

bool isValidConversion(Expr& expr, Type unresolvedSource, Type unresolvedTarget) {
    Type source = resolve(unresolvedSource);
    Type target = resolve(unresolvedTarget);

    if (expr.isLvalue() && source.isBasicType()) {
        TypeDecl* typeDecl = getTypeDecl(llvm::cast<BasicType>(*source));
        if (typeDecl && !typeDecl->passByValue() && !target.isPtrType()) {
            error(expr.getSrcLoc(), "implicit copying of class instances is disallowed");
        }
    }
    if (source.isImplicitlyConvertibleTo(target)) return true;

    // Autocast integer literals to parameter type if within range, error out if not within range.
    if (expr.isIntLiteralExpr() && target.isBasicType()) {
        int64_t value{expr.getIntLiteralExpr().value};
        llvm::StringRef targetTypeName = target.getName();
        if (targetTypeName == "int") return checkRange<int>(expr, value, targetTypeName);
        if (targetTypeName == "uint") return checkRange<unsigned>(expr, value, targetTypeName);
        if (targetTypeName == "int8") return checkRange<int8_t>(expr, value, targetTypeName);
        if (targetTypeName == "int16") return checkRange<int16_t>(expr, value, targetTypeName);
        if (targetTypeName == "int32") return checkRange<int32_t>(expr, value, targetTypeName);
        if (targetTypeName == "int64") return checkRange<int64_t>(expr, value, targetTypeName);
        if (targetTypeName == "uint8") return checkRange<uint8_t>(expr, value, targetTypeName);
        if (targetTypeName == "uint16") return checkRange<uint16_t>(expr, value, targetTypeName);
        if (targetTypeName == "uint32") return checkRange<uint32_t>(expr, value, targetTypeName);
        if (targetTypeName == "uint64") return checkRange<uint64_t>(expr, value, targetTypeName);
        if (targetTypeName == "float" || targetTypeName == "float32"
        ||  targetTypeName == "float64" || targetTypeName == "float80") {
            // TODO: Check that the integer value is losslessly convertible to the target type?
            expr.setType(target);
            return true;
        }
    } else if (expr.isNullLiteralExpr() && target.isPtrType() && !target.isRef()) {
        expr.setType(target);
        return true;
    } else if (expr.isStrLiteralExpr() && target.isPtrType()
               && target.getPointee().isChar()
               && !target.getPointee().isMutable()) {
        // Special case: allow passing string literals as C-strings (const char*).
        expr.setType(target);
        return true;
    } else if (expr.isLvalue() && source.isBasicType() && target.isPtrType()) {
        auto typeDecl = getTypeDecl(llvm::cast<BasicType>(*source));
        if (!typeDecl || typeDecl->passByValue()) {
            error(expr.getSrcLoc(), "cannot implicitly pass value types by reference, add explicit '&'");
        }
        if (source.isImplicitlyConvertibleTo(target.getPointee())) return true;
    }

    return false;
}

bool matchArgs(llvm::ArrayRef<Arg> args, llvm::ArrayRef<ParamDecl> params);
void validateArgs(const std::vector<Arg>& args, const std::vector<ParamDecl>& params,
                  const std::string& funcName, SrcLoc srcLoc);

void setCurrentGenericArgs(GenericFuncDecl& decl, CallExpr& call) {
    // TODO: When too few generic arguments are given, deduce the rest.

    if (call.genericArgs.empty()) {
        call.genericArgs.reserve(decl.genericParams.size());
        // FIXME: The args will also be typechecked by validateArgs()
        // after this function. Get rid of this duplicated typechecking.
        llvm::string

        for (auto& arg : call.args) call.genericArgs.emplace_back(typecheck(*arg.value));

        assert(call.genericArgs.size() == decl.genericParams.size());
    }
    else if (call.genericArgs.size() < decl.genericParams.size()) {
        error(call.srcLoc, "too few generic arguments to '", call.getFuncName(),
              "', expected ", decl.genericParams.size());
    }
    else if (call.genericArgs.size() > decl.genericParams.size()) {
        error(call.srcLoc, "too many generic arguments to '", call.getFuncName(),
              "', expected ", decl.genericParams.size());
    }

    auto genericArg = call.genericArgs.begin();
    for (const GenericParamDecl& genericParam : decl.genericParams) {
        currentGenericArgs.insert({genericParam.name, *genericArg++});
    }
}

Type typecheckBuiltinConversion(CallExpr& expr) {
    if (expr.args.size() != 1) {
        error(expr.srcLoc, "expected single argument to converting initializer");
    }
    if (!expr.genericArgs.empty()) {
        error(expr.srcLoc, "expected no generic arguments to converting initializer");
    }
    if (!expr.args.front().name.empty()) {
        error(expr.srcLoc, "expected unnamed argument to converting initializer");
    }
    typecheck(*expr.args.front().value);
    expr.setType(BasicType::get(expr.getFuncName()));
    return expr.getType();
}

Decl& resolveOverload(CallExpr& expr, llvm::StringRef callee) {
    llvm::SmallVector<Decl*, 1> matches;
    bool isInitCall = false;
    bool atLeastOneFunction = false;
    llvm::ArrayRef<Decl*> decls = findInSymbolTable(callee);

    for (Decl* decl : decls) {
        switch (decl->getKind()) {
            case DeclKind::FuncDecl:
                if (decls.size() == 1) {
                    validateArgs(expr.args, decl->getFuncDecl().params,
                                 callee, expr.func->getSrcLoc());
                    return *decl;
                }
                if (matchArgs(expr.args, decl->getFuncDecl().params)) {
                    matches.push_back(decl);
                }
                break;
            case DeclKind::GenericFuncDecl:
                setCurrentGenericArgs(decl->getGenericFuncDecl(), expr);
                if (decls.size() == 1) {
                    validateArgs(expr.args, decl->getGenericFuncDecl().func->params,
                                 callee, expr.func->getSrcLoc());
                    return *decl;
                }
                if (matchArgs(expr.args, decl->getGenericFuncDecl().func->params)) {
                    matches.push_back(decl);
                }
                currentGenericArgs.clear();
                break;
            case DeclKind::TypeDecl: {
                isInitCall = true;
                auto initDecls = symbolTable.find(mangleInitDecl(decl->getTypeDecl().name));
                for (Decl* initDecl : initDecls) {
                    if (initDecls.size() == 1) {
                        validateArgs(expr.args, initDecl->getInitDecl().params,
                                     callee, expr.func->getSrcLoc());
                        return *initDecl;
                    }
                    if (matchArgs(expr.args, initDecl->getInitDecl().params)) {
                        matches.push_back(initDecl);
                    }
                }
                break;
            }
            default: continue;
        }
        atLeastOneFunction = true;
    }

    switch (matches.size()) {
        case 1: return *matches.front();
        case 0:
            if (decls.size() == 0) {
                error(expr.func->getSrcLoc(), "unknown identifier '", callee, "'");
            } else if (atLeastOneFunction) {
                error(expr.func->getSrcLoc(), "no matching ", isInitCall ?
                      "initializer for '" : "function for call to '", callee, "'");
            } else {
                error(expr.func->getSrcLoc(), "'", callee, "' is not a function");
            }
        default: error(expr.func->getSrcLoc(), "ambiguous reference to '", callee,
                       isInitCall ? ".init'" : "'");
    }
}

Type typecheck(CallExpr& expr) {
    if (!expr.callsNamedFunc()) fatalError("anonymous function calls not implemented yet");

    if (Type::isBuiltinScalar(expr.getFuncName()))
        return typecheckBuiltinConversion(expr);

    Decl* decl;

    if (expr.isMemberFuncCall()) {
        typecheck(*expr.getReceiver());
        decl = &resolveOverload(expr, expr.getMangledFuncName());

        Type receiverType = expr.getReceiver()->getType();
        if (receiverType.isPtrType() && !receiverType.isRef()) {
            error(expr.getReceiver()->getSrcLoc(), "cannot call member function through pointer '",
                  receiverType, "', pointer may be null");
        }
    } else {
        decl = &resolveOverload(expr, expr.getFuncName());
    }

    expr.setCalleeDecl(decl);

    if (decl->isFuncDecl()) {
        return decl->getFuncDecl().getFuncType()->returnType;
    } else if (decl->isInitDecl()) {
        return decl->getInitDecl().getTypeDecl().getType();
    } else if (decl->isGenericFuncDecl()) {
        setCurrentGenericArgs(decl->getGenericFuncDecl(), expr);
        typecheck(decl->getGenericFuncDecl());
        Type returnType = resolve(decl->getGenericFuncDecl().func->getFuncType()->returnType);
        currentGenericArgs.clear();
        return returnType;
    } else {
        llvm_unreachable("all cases handled");
    }
}

bool matchArgs(llvm::ArrayRef<Arg> args, llvm::ArrayRef<ParamDecl> params) {
    if (args.size() != params.size()) return false;

    for (size_t i = 0; i < params.size(); ++i) {
        const Arg& arg = args[i];
        const ParamDecl& param = params[i];

        if (!arg.name.empty() && arg.name != param.name) return false;
        auto argType = typecheck(*arg.value);
        if (!isValidConversion(*arg.value, argType, param.type)) return false;
    }
    return true;
}

void validateArgs(const std::vector<Arg>& args, const std::vector<ParamDecl>& params,
                  const std::string& funcName, SrcLoc srcLoc) {
    if (args.size() < params.size()) {
        error(srcLoc, "too few arguments to '", funcName, "', expected ", params.size());
    }
    if (args.size() > params.size()) {
        error(srcLoc, "too many arguments to '", funcName, "', expected ", params.size());
    }

    for (size_t i = 0; i < params.size(); ++i) {
        const Arg& arg = args[i];
        const ParamDecl& param = params[i];

        if (!arg.name.empty() && arg.name != param.name) {
            error(arg.srcLoc, "invalid argument name '", arg.name,
                  "' for parameter '", param.name, "'");
        }
        auto argType = typecheck(*arg.value);
        if (!isValidConversion(*arg.value, argType, param.type)) {
            error(arg.srcLoc, "invalid argument #", i + 1, " type '", argType,
                  "' to '", funcName, "', expected '", param.type, "'");
        }
    }
}

Type typecheck(CastExpr& expr) {
    Type sourceType = typecheck(*expr.expr);
    Type targetType = expr.type;

    switch (sourceType.getKind()) {
        case TypeKind::BasicType:
            if (sourceType.isBool() && targetType.isInt()) {
                return targetType; // bool -> int
            }
        case TypeKind::ArrayType:
        case TypeKind::RangeType:
        case TypeKind::TupleType:
        case TypeKind::FuncType:
            break;
        case TypeKind::PtrType:
            Type sourcePointee = sourceType.getPointee();
            if (targetType.isPtrType()) {
                Type targetPointee = targetType.getPointee();
                if (sourcePointee.isVoid()
                && (!targetPointee.isMutable() || sourcePointee.isMutable())) {
                    return targetType; // (mutable) void* -> T* / mutable void* -> mutable T*
                }
                if (targetPointee.isVoid()
                && (!targetPointee.isMutable() || sourcePointee.isMutable())) {
                    return targetType; // (mutable) T* -> void* / mutable T* -> mutable void*
                }
            }
            break;
    }
    error(expr.srcLoc, "illegal cast from '", sourceType, "' to '", targetType, "'");
}

Type typecheck(MemberExpr& expr) {
    Type baseType = typecheck(*expr.base);

    if (baseType.isPtrType()) {
        if (!baseType.isRef()) {
            error(expr.base->srcLoc, "cannot access member through pointer '", baseType,
                  "', pointer may be null");
        }
        baseType = baseType.getPointee();
    }

    if (baseType.isArrayType() || baseType.isString()) {
        if (expr.member != "count")
            error(expr.srcLoc, "no member named '", expr.member, "' in '", baseType, "'");
        return Type::getInt();
    }

    Decl& typeDecl = findInSymbolTable(baseType.getName(), SrcLoc::invalid());

    for (const FieldDecl& field : typeDecl.getTypeDecl().fields) {
        if (field.name == expr.member) {
            if (baseType.isMutable()) {
                return field.type;
            } else {
                return field.type.asImmutable();
            }
        }
    }
    error(expr.srcLoc, "no member named '", expr.member, "' in '", baseType, "'");
}

Type typecheck(SubscriptExpr& expr) {
    Type lhsType = typecheck(*expr.array);
    const ArrayType* arrayType;

    if (lhsType.isArrayType()) {
        arrayType = &llvm::cast<ArrayType>(*lhsType);
    } else if (lhsType.isRef() && lhsType.getReferee().isArrayType()) {
        arrayType = &llvm::cast<ArrayType>(*lhsType.getReferee());
    } else {
        error(expr.array->getSrcLoc(), "cannot subscript '", lhsType, "', expected array or reference-to-array");
    }

    Type indexType = typecheck(*expr.index);
    if (!isValidConversion(*expr.index, indexType, Type::getInt())) {
        error(expr.index->getSrcLoc(), "illegal subscript index type '", indexType, "', expected 'int'");
    }

    if (!arrayType->isUnsized() && expr.index->isIntLiteralExpr()
    && expr.index->getIntLiteralExpr().value >= arrayType->size) {
        error(expr.index->getSrcLoc(), "accessing array out-of-bounds with index ",
              expr.index->getIntLiteralExpr().value, ", array size is ", arrayType->size);
    }

    return arrayType->elementType;
}

Type typecheck(UnwrapExpr& expr) {
    Type type = typecheck(*expr.operand);
    if (!type.isNullablePointer())
        error(expr.srcLoc, "cannot unwrap non-pointer type '", type, "'");
    return PtrType::get(type.getPointee(), true);
}

Type typecheck(Expr& expr) {
    llvm::Optional<Type> type;
    switch (expr.getKind()) {
        case ExprKind::VariableExpr:    type = typecheck(expr.getVariableExpr()); break;
        case ExprKind::StrLiteralExpr:  type = typecheck(expr.getStrLiteralExpr()); break;
        case ExprKind::IntLiteralExpr:  type = typecheck(expr.getIntLiteralExpr()); break;
        case ExprKind::FloatLiteralExpr:type = typecheck(expr.getFloatLiteralExpr()); break;
        case ExprKind::BoolLiteralExpr: type = typecheck(expr.getBoolLiteralExpr()); break;
        case ExprKind::NullLiteralExpr: type = typecheck(expr.getNullLiteralExpr()); break;
        case ExprKind::ArrayLiteralExpr:type = typecheck(expr.getArrayLiteralExpr()); break;
        case ExprKind::PrefixExpr:      type = typecheck(expr.getPrefixExpr()); break;
        case ExprKind::BinaryExpr:      type = typecheck(expr.getBinaryExpr()); break;
        case ExprKind::CallExpr:        type = typecheck(expr.getCallExpr()); break;
        case ExprKind::CastExpr:        type = typecheck(expr.getCastExpr()); break;
        case ExprKind::MemberExpr:      type = typecheck(expr.getMemberExpr()); break;
        case ExprKind::SubscriptExpr:   type = typecheck(expr.getSubscriptExpr()); break;
        case ExprKind::UnwrapExpr:      type = typecheck(expr.getUnwrapExpr()); break;
    }
    assert(*type);
    expr.setType(resolve(*type));
    return expr.getType();
}

bool isValidConversion(std::vector<std::unique_ptr<Expr>>& exprs, Type source, Type target) {
    if (!source.isTupleType()) {
        assert(!target.isTupleType());
        assert(exprs.size() == 1);
        return isValidConversion(*exprs[0], source, target);
    }
    assert(target.isTupleType());

    for (size_t i = 0; i < exprs.size(); ++i) {
        if (!isValidConversion(*exprs[i], source.getSubtypes()[i], target.getSubtypes()[i])) {
            return false;
        }
    }
    return true;
}

void typecheck(ReturnStmt& stmt) {
    if (stmt.values.empty()) {
        if (!funcReturnType.isVoid()) {
            error(stmt.srcLoc, "expected return statement to return a value of type '", funcReturnType, "'");
        }
        return;
    }
    std::vector<Type> returnValueTypes;
    for (auto& expr : stmt.values) {
        returnValueTypes.push_back(typecheck(*expr));
    }
    Type returnType = returnValueTypes.size() > 1
        ? TupleType::get(std::move(returnValueTypes)) : returnValueTypes[0];
    if (!isValidConversion(stmt.values, returnType, funcReturnType)) {
        error(stmt.srcLoc, "mismatching return type '", returnType, "', expected '", funcReturnType, "'");
    }
}

void typecheck(VarDecl& decl, bool isGlobal);

void typecheck(VariableStmt& stmt) {
    typecheck(*stmt.decl, false);
}

void typecheck(IncrementStmt& stmt) {
    auto type = typecheck(*stmt.operand);
    if (!type.isMutable()) {
        error(stmt.srcLoc, "cannot increment immutable value");
    }
    // TODO: check that operand supports increment operation.
}

void typecheck(DecrementStmt& stmt) {
    auto type = typecheck(*stmt.operand);
    if (!type.isMutable()) {
        error(stmt.srcLoc, "cannot decrement immutable value");
    }
    // TODO: check that operand supports decrement operation.
}

void typecheck(IfStmt& ifStmt) {
    Type conditionType = typecheck(*ifStmt.condition);
    if (!conditionType.isBool()) {
        error(ifStmt.condition->getSrcLoc(), "'if' condition must have type 'bool'");
    }
    for (auto& stmt : ifStmt.thenBody) typecheck(*stmt);
    for (auto& stmt : ifStmt.elseBody) typecheck(*stmt);
}

void typecheck(SwitchStmt& stmt) {
    Type conditionType = typecheck(*stmt.condition);
    breakableBlocks++;
    for (SwitchCase& switchCase : stmt.cases) {
        Type caseType = typecheck(*switchCase.value);
        if (caseType != conditionType) {
            error(switchCase.value->getSrcLoc(), "case value type '", caseType,
                  "' doesn't match switch condition type '", conditionType, "'");
        }
        for (auto& caseStmt : switchCase.stmts) typecheck(*caseStmt);
    }
    for (auto& defaultStmt : stmt.defaultStmts) typecheck(*defaultStmt);
    breakableBlocks--;
}

void typecheck(WhileStmt& whileStmt) {
    Type conditionType = typecheck(*whileStmt.condition);
    if (!conditionType.isBool()) {
        error(whileStmt.condition->getSrcLoc(), "'while' condition must have type 'bool'");
    }
    breakableBlocks++;
    for (auto& stmt : whileStmt.body) typecheck(*stmt);
    breakableBlocks--;
}

void typecheck(ForStmt& forStmt) {
    if (symbolTable.contains(forStmt.id)) {
        error(forStmt.srcLoc, "redefinition of '", forStmt.id, "'");
    }

    Type rangeType = typecheck(*forStmt.range);
    if (!rangeType.isIterable()) {
        error(forStmt.range->getSrcLoc(), "'for' range expression is not an 'Iterable'");
    }

    symbolTable.pushScope();
    addToSymbolTable(VarDecl(rangeType.getIterableElementType(), std::string(forStmt.id),
                             nullptr, forStmt.srcLoc));
    breakableBlocks++;
    for (auto& stmt : forStmt.body) typecheck(*stmt);
    breakableBlocks--;
    symbolTable.popScope();
}

void typecheck(BreakStmt& breakStmt) {
    if (breakableBlocks == 0) {
        error(breakStmt.srcLoc, "'break' is only allowed inside 'while' and 'switch' statements");
    }
}

void typecheckAssignment(Expr& lhs, Expr& rhs, SrcLoc srcLoc) {
    Type lhsType = typecheck(lhs);
    if (lhsType.isFuncType()) error(srcLoc, "cannot assign to function");
    Type rhsType = typecheck(rhs);
    if (!isValidConversion(rhs, rhsType, lhsType)) {
        error(rhs.getSrcLoc(), "cannot assign '", rhsType, "' to variable of type '", lhsType, "'");
    }
    if (!lhsType.isMutable() && !inInitializer) {
        if (lhs.isVariableExpr()) {
            error(srcLoc, "cannot assign to immutable variable '",
                  lhs.getVariableExpr().identifier, "'");
        } else {
            error(srcLoc, "cannot assign to immutable expression");
        }
    }
}

void typecheck(AssignStmt& stmt) {
    typecheckAssignment(*stmt.lhs, *stmt.rhs, stmt.srcLoc);
}

void typecheck(AugAssignStmt& stmt) {
    // FIXME: Don't create temporary BinaryExpr.
    BinaryExpr expr(stmt.op, std::unique_ptr<Expr>(stmt.lhs.get()),
                    std::unique_ptr<Expr>(stmt.rhs.get()), stmt.srcLoc);
    typecheckAssignment(*stmt.lhs, expr, stmt.srcLoc);
    expr.args[0].value.release();
    expr.args[1].value.release();
}

void typecheck(Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt:    typecheck(stmt.getReturnStmt()); break;
        case StmtKind::VariableStmt:  typecheck(stmt.getVariableStmt()); break;
        case StmtKind::IncrementStmt: typecheck(stmt.getIncrementStmt()); break;
        case StmtKind::DecrementStmt: typecheck(stmt.getDecrementStmt()); break;
        case StmtKind::ExprStmt:      typecheck(*stmt.getExprStmt().expr); break;
        case StmtKind::DeferStmt:     typecheck(*stmt.getDeferStmt().expr); break;
        case StmtKind::IfStmt:        typecheck(stmt.getIfStmt()); break;
        case StmtKind::SwitchStmt:    typecheck(stmt.getSwitchStmt()); break;
        case StmtKind::WhileStmt:     typecheck(stmt.getWhileStmt()); break;
        case StmtKind::ForStmt:       typecheck(stmt.getForStmt()); break;
        case StmtKind::BreakStmt:     typecheck(stmt.getBreakStmt()); break;
        case StmtKind::AssignStmt:    typecheck(stmt.getAssignStmt()); break;
        case StmtKind::AugAssignStmt: typecheck(stmt.getAugAssignStmt()); break;
    }
}

void typecheck(ParamDecl& decl) {
    if (symbolTable.contains(decl.name)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
    symbolTable.add(decl.name, &decl);
}

} // anonymous namespace

void delta::addToSymbolTable(FuncDecl& decl) {
    if (!importingC && symbolTable.findWithMatchingParams(decl)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
    symbolTable.add(mangle(decl), &decl);
}

void delta::addToSymbolTable(GenericFuncDecl& decl) {
    if (symbolTable.contains(decl.func->name)) {
        error(decl.func->srcLoc, "redefinition of '", decl.func->name, "'");
    }
    symbolTable.add(decl.func->name, &decl);
}

void delta::addToSymbolTable(InitDecl& decl) {
    if (symbolTable.findWithMatchingParams(decl)) {
        error(decl.srcLoc, "redefinition of '", decl.getTypeName(), "' initializer");
    }
    Decl& typeDecl = findInSymbolTable(decl.getTypeName(), decl.srcLoc);
    if (!typeDecl.isTypeDecl()) error(decl.srcLoc, "'", decl.getTypeName(), "' is not a class or struct");
    decl.type = &typeDecl.getTypeDecl();

    symbolTable.add(mangle(decl), &decl);
}

void delta::addToSymbolTable(DeinitDecl& decl) {
    if (symbolTable.contains(mangle(decl))) {
        error(decl.srcLoc, "redefinition of '", decl.getTypeName(), "' deinitializer");
    }
    Decl& typeDecl = findInSymbolTable(decl.getTypeName(), decl.srcLoc);
    if (!typeDecl.isTypeDecl()) error(decl.srcLoc, "'", decl.getTypeName(), "' is not a class or struct");
    decl.type = &typeDecl.getTypeDecl();

    symbolTable.add(mangle(decl), &decl);
}

void delta::addToSymbolTable(TypeDecl& decl) {
    if (!importingC && symbolTable.contains(decl.name)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
    symbolTable.add(decl.name, &decl);
}

void delta::addToSymbolTable(VarDecl& decl) {
    if (!importingC && symbolTable.contains(decl.name)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
    symbolTable.add(decl.name, &decl);
    globalVariables.push_back(&decl);
}

void delta::addToSymbolTable(FuncDecl&& decl) {
    std::string name = decl.name;
    nonASTDecls.push_back(llvm::make_unique<FuncDecl>(std::move(decl)));
    symbolTable.add(std::move(name), nonASTDecls.back().get());
}

void delta::addToSymbolTable(TypeDecl&& decl) {
    std::string name = decl.name;
    nonASTDecls.push_back(llvm::make_unique<TypeDecl>(std::move(decl)));
    symbolTable.add(std::move(name), nonASTDecls.back().get());
}

void delta::addToSymbolTable(VarDecl&& decl) {
    std::string name = decl.name;
    nonASTDecls.push_back(llvm::make_unique<VarDecl>(std::move(decl)));
    symbolTable.add(std::move(name), nonASTDecls.back().get());
}

Decl& delta::findInSymbolTable(llvm::StringRef name, SrcLoc srcLoc) {
    llvm::ArrayRef<Decl*> decls = findInSymbolTable(name);
    if (decls.empty()) error(srcLoc, "unknown identifier '", name, "'");
    if (decls.size() > 1) error(srcLoc, "ambiguous reference to '", name, "'");
    return *decls[0];
}

llvm::ArrayRef<Decl*> delta::findInSymbolTable(llvm::StringRef name) {
    return symbolTable.find(name);
}

namespace {

bool returns(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt: return true;
        case StmtKind::IfStmt:
            if (stmt.getIfStmt().thenBody.empty()) return false;
            if (stmt.getIfStmt().elseBody.empty()) return false;
            if (!returns(*stmt.getIfStmt().thenBody.back())) return false;
            if (!returns(*stmt.getIfStmt().elseBody.back())) return false;
            return true;
        case StmtKind::SwitchStmt:
            for (auto& switchCase : stmt.getSwitchStmt().cases) {
                if (!returns(*switchCase.stmts.back())) return false;
            }
            if (!returns(*stmt.getSwitchStmt().defaultStmts.back())) return false;
            return true;
        default: return false;
    }
}

void typecheckMemberFunc(FuncDecl& decl);

void typecheck(FuncDecl& decl) {
    if (!decl.receiverType.empty()) return typecheckMemberFunc(decl);
    if (decl.isExtern()) return;

    symbolTable.pushScope();
    for (ParamDecl& param : decl.params) {
        if (param.type.isMutable()) error(param.srcLoc, "parameter types cannot be 'mutable'");
        typecheck(param);
    }
    if (decl.returnType.isMutable()) error(decl.srcLoc, "return types cannot be 'mutable'");

    auto funcReturnTypeBackup = funcReturnType;
    funcReturnType = decl.returnType;
    for (auto& stmt : *decl.body) typecheck(*stmt);
    funcReturnType = funcReturnTypeBackup;

    symbolTable.popScope();

    if (!decl.returnType.isVoid() && (decl.body->empty() || !returns(*decl.body->back()))) {
        error(decl.srcLoc, "'", decl.name, "' is missing a return statement");
    }
}

void typecheckMemberFunc(FuncDecl& decl) {
    symbolTable.pushScope();
    Decl& receiverType = findInSymbolTable(decl.receiverType, decl.srcLoc);
    if (!receiverType.isTypeDecl()) error(decl.srcLoc, "'", decl.receiverType, "' is not a class or struct");
    addToSymbolTable(VarDecl(receiverType.getTypeDecl().getTypeForPassing(decl.isMutating()),
                             "this", nullptr, SrcLoc::invalid()));
    for (ParamDecl& param : decl.params) typecheck(param);

    auto funcReturnTypeBackup = funcReturnType;
    funcReturnType = decl.returnType;
    for (auto& stmt : *decl.body) typecheck(*stmt);
    funcReturnType = funcReturnTypeBackup;

    symbolTable.popScope();
}

void typecheck(GenericParamDecl& decl) {
    if (symbolTable.contains(decl.name)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
}

void typecheck(GenericFuncDecl& decl) {
    if (currentGenericArgs.empty()) {
        for (auto& genericParam : decl.genericParams)
            typecheck(genericParam);
        return; // Partial type-checking of uninstantiated generic functions not implemented yet.
    }
    typecheck(*decl.func);
}

void typecheck(InitDecl& decl) {
    symbolTable.pushScope();
    Decl& typeDecl = findInSymbolTable(decl.getTypeName(), decl.srcLoc);
    if (!typeDecl.isTypeDecl()) error(decl.srcLoc, "'", decl.getTypeName(), "' is not a class or struct");
    decl.type = &typeDecl.getTypeDecl();
    addToSymbolTable(VarDecl(typeDecl.getTypeDecl().getType(true),
                             "this", nullptr, SrcLoc::invalid()));
    for (ParamDecl& param : decl.params) typecheck(param);
    inInitializer = true;
    for (auto& stmt : *decl.body) typecheck(*stmt);
    inInitializer = false;
    symbolTable.popScope();
}

void typecheck(DeinitDecl& decl) {
    Decl& typeDecl = findInSymbolTable(decl.getTypeName(), decl.srcLoc);
    FuncDecl funcDecl(mangle(decl), {}, typeDecl.getTypeDecl().getType(),
                      std::string(decl.getTypeName()), SrcLoc::invalid());
    funcDecl.body = decl.body;
    decl.type = &typeDecl.getTypeDecl();
    typecheckMemberFunc(funcDecl);
}

void typecheck(TypeDecl&) {
    // TODO
}

TypeDecl* getTypeDecl(const BasicType& type) {
    llvm::ArrayRef<Decl*> decls = findInSymbolTable(type.name);
    if (decls.empty()) return nullptr;
    assert(decls.size() == 1);
    return &decls[0]->getTypeDecl();
}

void typecheck(VarDecl& decl, bool isGlobal) {
    if (!isGlobal && symbolTable.contains(decl.name)) {
        error(decl.srcLoc, "redefinition of '", decl.name, "'");
    }
    Type initType = nullptr;
    if (decl.initializer) {
        initType = typecheck(*decl.initializer);
        if (initType.isFuncType()) {
            error(decl.initializer->getSrcLoc(), "function pointers not implemented yet");
        }
    } else if (isGlobal) {
        error(decl.srcLoc, "global variables cannot be uninitialized");
    }

    if (auto declaredType = decl.getType()) {
        if (initType && !isValidConversion(*decl.initializer, initType, declaredType)) {
            error(decl.initializer->getSrcLoc(), "cannot initialize variable of type '", declaredType,
                "' with '", initType, "'");
        }
    } else {
        if (initType.isNull()) {
            error(decl.srcLoc, "couldn't infer type of '", decl.name, "', add a type annotation");
        }

        initType.setMutable(decl.getType().isMutable());
        decl.type = initType;
    }

    if (!isGlobal) addToSymbolTable(decl);
}

void typecheck(FieldDecl&) {
}

void typecheck(ImportDecl& decl) {
    importingC = true;
    importCHeader(decl.target, includePaths);
    importingC = false;
}

void typecheck(Decl& decl) {
    switch (decl.getKind()) {
        case DeclKind::ParamDecl: typecheck(decl.getParamDecl()); break;
        case DeclKind::FuncDecl:  typecheck(decl.getFuncDecl()); break;
        case DeclKind::GenericParamDecl: typecheck(decl.getGenericParamDecl()); break;
        case DeclKind::GenericFuncDecl: typecheck(decl.getGenericFuncDecl()); break;
        case DeclKind::InitDecl:  typecheck(decl.getInitDecl()); break;
        case DeclKind::DeinitDecl:typecheck(decl.getDeinitDecl()); break;
        case DeclKind::TypeDecl:  typecheck(decl.getTypeDecl()); break;
        case DeclKind::VarDecl:   typecheck(decl.getVarDecl(), true); break;
        case DeclKind::FieldDecl: typecheck(decl.getFieldDecl()); break;
        case DeclKind::ImportDecl:typecheck(decl.getImportDecl()); break;
    }
}

} // anonymous namespace

void delta::typecheck(Module& module, const std::vector<llvm::StringRef>& includePaths) {
    ::includePaths = includePaths;

    // Infer the types of global variables for use before their declaration.
    for (VarDecl* var : globalVariables) {
        ::typecheck(*var, true);
    }

    for (auto& fileUnit : module.getFileUnits()) {
        for (auto& decl : fileUnit.getTopLevelDecls()) {
            if (!decl->isVarDecl()) {
                ::typecheck(*decl);
            }
        }
    }
}
