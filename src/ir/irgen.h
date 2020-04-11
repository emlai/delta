#pragma once

#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/DenseMap.h>
#include <llvm/ADT/StringSet.h>
#pragma warning(pop)
#include "../ast/decl.h"
#include "../ast/expr.h"
#include "../ast/stmt.h"
#include "../ir/ir.h"
#include "../sema/typecheck.h"

// TODO(ir): Rename "IR" to "IL" or "Intermediate" or to avoid confusion with LLVM IR?

namespace delta {

class Module;
struct Type;
class Typechecker;
class IRGenerator;

struct IRGenScope {
    IRGenScope(IRGenerator& irGenerator) : irGenerator(&irGenerator) {}
    void onScopeEnd();
    void clear();

    struct DeferredDestructor {
        IRFunction* function;
        IRValue* value;
        const Decl* decl;
    };

    llvm::SmallVector<const Expr*, 8> deferredExprs;
    llvm::SmallVector<DeferredDestructor, 8> destructorsToCall;
    llvm::DenseMap<const Decl*, IRValue*> valuesByDecl;
    IRGenerator* irGenerator;
};

class IRGenerator {
public:
    IRGenerator();
    IRModule& emitModule(const Module& sourceModule);
    std::vector<IRModule*> getGeneratedModules() { return std::move(generatedModules); }

private:
    friend struct IRGenScope;

    void emitFunctionBody(const FunctionDecl& decl, IRFunction& function);
    void createDestructorCall(IRFunction* destructor, IRValue* receiver);

    /// 'decl' is null if this is the 'this' value.
    void setLocalValue(IRValue* value, const VariableDecl* decl);
    IRValue* getValueOrNull(const Decl* decl);
    IRValue* getValue(const Decl* decl);
    IRValue* getThis(IRType* targetType = nullptr);

    /// Emits and loads value.
    IRValue* emitExpr(const Expr& expr);
    /// Emits value without loading.
    IRValue* emitLvalueExpr(const Expr& expr);
    /// Emits value as a pointer, storing it in a temporary alloca if needed.
    IRValue* emitExprAsPointer(const Expr& expr);
    IRValue* emitExprOrEnumTag(const Expr& expr, IRValue** enumValue);
    IRValue* emitExprWithoutAutoCast(const Expr& expr);
    IRValue* emitAutoCast(IRValue* value, const Expr& expr);
    IRValue* emitVarExpr(const VarExpr& expr);
    IRValue* emitStringLiteralExpr(const StringLiteralExpr& expr);
    IRValue* emitCharacterLiteralExpr(const CharacterLiteralExpr& expr);
    IRValue* emitIntLiteralExpr(const IntLiteralExpr& expr);
    IRValue* emitFloatLiteralExpr(const FloatLiteralExpr& expr);
    IRValue* emitBoolLiteralExpr(const BoolLiteralExpr& expr);
    IRValue* emitNullLiteralExpr(const NullLiteralExpr& expr);
    IRValue* emitUndefinedLiteralExpr(const UndefinedLiteralExpr& expr);
    IRValue* emitArrayLiteralExpr(const ArrayLiteralExpr& expr);
    IRValue* emitTupleExpr(const TupleExpr& expr);
    IRValue* emitImplicitNullComparison(IRValue* operand);
    IRValue* emitNot(const UnaryExpr& expr);
    IRValue* emitUnaryExpr(const UnaryExpr& expr);
    IRValue* emitConstantIncrement(const UnaryExpr& expr, int value);
    IRValue* emitLogicalAnd(const Expr& left, const Expr& right);
    IRValue* emitLogicalOr(const Expr& left, const Expr& right);
    IRValue* emitBinaryExpr(const BinaryExpr& expr);
    void emitAssignment(const BinaryExpr& expr);
    IRValue* emitExprForPassing(const Expr& expr, IRType* targetType);
    IRValue* emitOptionalConstruction(Type wrappedType, IRValue* arg);
    void emitAssert(IRValue* condition, SourceLocation location, llvm::StringRef message = "Assertion failed");
    IRValue* emitEnumCase(const EnumCase& enumCase, llvm::ArrayRef<NamedValue> associatedValueElements);
    IRValue* emitCallExpr(const CallExpr& expr, IRAllocaInst* thisAllocaForInit = nullptr);
    IRValue* emitBuiltinCast(const CallExpr& expr);
    IRValue* emitSizeofExpr(const SizeofExpr& expr);
    IRValue* emitAddressofExpr(const AddressofExpr& expr);
    IRValue* emitMemberAccess(IRValue* baseValue, const FieldDecl* field);
    IRValue* emitMemberExpr(const MemberExpr& expr);
    IRValue* emitTupleElementAccess(const MemberExpr& expr);
    IRValue* emitIndexExpr(const IndexExpr& expr);
    IRValue* emitUnwrapExpr(const UnwrapExpr& expr);
    IRValue* emitLambdaExpr(const LambdaExpr& expr);
    IRValue* emitIfExpr(const IfExpr& expr);
    IRValue* emitImplicitCastExpr(const ImplicitCastExpr& expr);

    void emitDeferredExprsAndDestructorCallsForReturn();
    void emitBlock(llvm::ArrayRef<Stmt*> stmts, IRBasicBlock* continuation);
    void emitReturnStmt(const ReturnStmt& stmt);
    void emitVarStmt(const VarStmt& stmt);
    void emitIfStmt(const IfStmt& ifStmt);
    void emitSwitchStmt(const SwitchStmt& switchStmt);
    void emitForStmt(const ForStmt& forStmt);
    void emitBreakStmt(const BreakStmt&);
    void emitContinueStmt(const ContinueStmt&);
    IRValue* emitAssignmentLHS(const Expr& lhs);
    void emitCompoundStmt(const CompoundStmt& stmt);
    void emitStmt(const Stmt& stmt);

    void emitDecl(const Decl& decl);
    void emitFunctionDecl(const FunctionDecl& decl);
    IRValue* emitVarDecl(const VarDecl& decl);

    IRValue* getFunctionForCall(const CallExpr& call);
    IRFunction* getFunctionProto(const FunctionDecl& decl);
    IRAllocaInst* createEntryBlockAlloca(IRType* type, const llvm::Twine& name = "");
    IRAllocaInst* createEntryBlockAlloca(Type type, const llvm::Twine& name = "");
    IRAllocaInst* createTempAlloca(IRValue* value);
    IRValue* createLoad(IRValue* value);
    void createStore(IRValue* value, IRValue* pointer);
    IRValue* createCall(IRValue* function, llvm::ArrayRef<IRValue*> args);
    void createCondBr(IRValue* condition, IRBasicBlock* trueBlock, IRBasicBlock* falseBlock) {
        insertBlock->insts.push_back(new IRConditionalBranchInst{ValueKind::IRConditionalBranchInst, condition, trueBlock, falseBlock});
    }
    void createBr(IRBasicBlock* destination) { insertBlock->insts.push_back(new IRBranchInst{ValueKind::IRBranchInst, destination}); }

    IRValue* createPhi(std::vector<std::pair<IRValue*, IRBasicBlock*>> valuesAndPredecessors, const llvm::Twine& name = "") {
        auto phi = new IRPhiInst{ValueKind::IRPhiInst, std::move(valuesAndPredecessors), name.str()};
        insertBlock->insts.push_back(phi);
        return phi;
    }

    IRValue* createInsertValue(IRValue* aggregate, IRValue* value, int index) {
        auto inst = new IRInsertValueInst{ValueKind::IRInsertValueInst, aggregate, value, index, ""};
        insertBlock->insts.push_back(inst);
        return inst;
    }
    IRValue* createExtractValue(IRValue* aggregate, int index, const llvm::Twine& name = "") {
        auto inst = new IRExtractValueInst{ValueKind::IRExtractValueInst, aggregate, index, name.str()};
        insertBlock->insts.push_back(inst);
        return inst;
    }

    // Constants // TODO(ir) remove unused
    IRValue* createConstantInt(IRType* type, llvm::APSInt value) { return new IRConstantInt{ValueKind::IRConstantInt, type, std::move(value)}; }
    IRValue* createConstantInt(IRType* type, int64_t value) { return createConstantInt(type, llvm::APSInt::get(value)); }
    IRValue* createConstantInt(Type type, llvm::APSInt value) { return createConstantInt(getILType(type), std::move(value)); }
    IRValue* createConstantInt(Type type, int64_t value) { return createConstantInt(getILType(type), llvm::APSInt::get(value)); }
    IRValue* createConstantFP(IRType* type, llvm::APFloat value) { return new IRConstantFP{ValueKind::IRConstantFP, type, std::move(value)}; }
    IRValue* createConstantFP(IRType* type, double value) { return createConstantFP(type, llvm::APFloat(value)); }
    IRValue* createConstantFP(Type type, llvm::APFloat value) { return createConstantFP(getILType(type), std::move(value)); }
    IRValue* createConstantFP(Type type, double value) { return createConstantFP(getILType(type), llvm::APFloat(value)); }
    IRValue* createConstantBool(bool value) { return new IRConstantBool{ValueKind::IRConstantBool, value}; }
    IRValue* createConstantNull(IRType* type) {
        ASSERT(type->isPointerType());
        return new IRConstantNull{ValueKind::IRConstantNull, type};
    }
    IRValue* createConstantNull(Type type) {
        ASSERT(type.isPointerTypeInLLVM());
        return createConstantNull(getILType(type));
    }
    IRValue* createUndefined(IRType* type) { return new IRUndefined{ValueKind::IRUndefined, type}; }
    IRValue* createUndefined(Type type) { return createUndefined(getILType(type)); }

    // Arithmetic/comparison operations
    IRValue* createBinaryOp(BinaryOperator op, IRValue* left, IRValue* right) {
        // TODO(ir): Move this out of this func to emitBinaryOp?
        if (left->getType()->isPointerType() && left->getType()->getPointee()->equals(right->getType())) {
            left = createLoad(left);
        } else if (right->getType()->isPointerType() && right->getType()->getPointee()->equals(left->getType())) {
            right = createLoad(right);
        }

        ASSERT(left->getType()->equals(right->getType()));
        auto binaryOp = new IRBinaryOp{ValueKind::IRBinaryOp, op, left, right, ""};
        insertBlock->insts.push_back(binaryOp);
        return binaryOp;
    }

    IRValue* createIsNull(IRValue* value, const llvm::Twine& name = "") {
        IRValue* nullValue;
        auto type = value->getType();

        // TODO(ir): refactor it into a separate function.
        if (type->isBool()) {
            nullValue = createConstantBool(false);
        } else if (type->isPointerType()) {
            nullValue = createConstantNull(type);
        } else if (type->isInteger()) {
            nullValue = createConstantInt(type, 0);
        } else if (type->isFloatingPoint()) {
            nullValue = createConstantFP(type, 0);
        } else {
            llvm_unreachable("invalid type passed to createIsNull");
        }

        ASSERT(value->getType()->equals(nullValue->getType()));
        auto op = new IRBinaryOp{ValueKind::IRBinaryOp, Token::Equal, value, nullValue, name.str()};
        insertBlock->insts.push_back(op);
        return op;
    }

    IRValue* createNeg(IRValue* value) {
        auto op = new IRUnaryOp{ValueKind::IRUnaryOp, Token::Minus, value, ""};
        insertBlock->insts.push_back(op);
        return op;
    }

    IRValue* createNot(IRValue* value) {
        auto op = new IRUnaryOp{ValueKind::IRUnaryOp, Token::Not, value, ""};
        insertBlock->insts.push_back(op);
        return op;
    }

    // GEP
    IRValue* createGEP(IRValue* pointer, std::vector<IRValue*> indexes, const llvm::Twine& name = "") {
        auto gep = new IRGetElementPtr{ValueKind::IRGetElementPtr, pointer, std::move(indexes), name.str()};
        insertBlock->insts.push_back(gep);
        return gep;
    }

    IRValue* createGEP(IRValue* pointer, int index0, int index1, const llvm::Twine& name = "") {
        if (pointer->getType()->getPointee()->isArrayType()) {
            ASSERT(index1 < pointer->getType()->getPointee()->getArraySize());
        } else {
            ASSERT(index1 < (int) pointer->getType()->getPointee()->getFields().size());
        }
        auto gep = new IRConstGEP{ValueKind::IRConstGEP, pointer, index0, index1, name.str()};
        insertBlock->insts.push_back(gep);
        return gep;
    }

    // Casts
    IRValue* createCast(IRValue* value, IRType* type, const llvm::Twine& name = "") {
        auto cast = new IRCastInst{ValueKind::IRCastInst, value, type, name.str()};
        insertBlock->insts.push_back(cast);
        return cast;
    }

    IRValue* createCast(IRValue* value, Type type, const llvm::Twine& name = "") { return createCast(value, getILType(type), name); }
    IRValue* createGlobalStringPtr(llvm::StringRef value) { return new IRConstantString{ValueKind::IRConstantString, value}; }
    IRValue* createSizeof(Type type) { return new IRSizeof{ValueKind::IRSizeof, getILType(type), ""}; }

    IRSwitchInst* createSwitch(IRValue* condition, IRBasicBlock* defaultBlock) {
        auto switchInst = new IRSwitchInst{ValueKind::IRSwitchInst, condition, defaultBlock};
        insertBlock->insts.push_back(switchInst);
        return switchInst;
    }

    void createUnreachable() { insertBlock->insts.push_back(new IRUnreachable{ValueKind::IRUnreachable}); }
    void createReturn(IRValue* value) { insertBlock->insts.push_back(new IRReturnInst{ValueKind::IRReturnInst, value}); }

    IRValue* getArrayLength(const Expr& object, Type objectType);
    IRValue* getArrayIterator(const Expr& object, Type objectType);

    void beginScope();
    void endScope();
    void deferEvaluationOf(const Expr& expr);
    DestructorDecl* getDefaultDestructor(TypeDecl& typeDecl);
    void deferDestructorCall(IRValue* receiver, const VariableDecl* decl);
    IRGenScope& globalScope() { return scopes.front(); }
    std::string createName() { return std::to_string(nameCounter++); }

    void setInsertPoint(IRBasicBlock* block);

private:
    struct FunctionInstantiation {
        const FunctionDecl* decl;
        IRFunction* function;
    };

    std::vector<IRGenScope> scopes;

    IRModule* module = nullptr;
    std::vector<IRModule*> generatedModules;

    std::vector<FunctionInstantiation> functionInstantiations;
    const Decl* currentDecl;

    /// The basic blocks to branch to on a 'break'/'continue' statement.
    llvm::SmallVector<IRBasicBlock*, 4> breakTargets;
    llvm::SmallVector<IRBasicBlock*, 4> continueTargets;

    IRBasicBlock* insertBlock;

    static const int optionalHasValueFieldIndex = 0;
    static const int optionalValueFieldIndex = 1;
    int nameCounter = 0;
    llvm::StringSet<> usedNames;
    IRFunction* currentFunction = nullptr;
};

} // namespace delta
