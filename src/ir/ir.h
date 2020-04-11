#pragma once

#include <string>
#include <vector>
#pragma warning(push, 0)
#include <llvm/ADT/APFloat.h>
#include <llvm/ADT/APSInt.h>
#pragma warning(pop)
#include "../ast/token.h"
#include "../ast/type.h"

namespace llvm {
class StringRef;
}

namespace delta {

struct IRFunction;
struct IRBasicBlock;
struct IRBasicType;

enum class IRTypeKind {
    IRBasicType,
    IRPointerType,
    IRFunctionType,
    IRArrayType,
    IRStructType,
    IRUnionType,
};

struct IRType {
    IRTypeKind kind;

    bool isPrimitiveType() { return kind == IRTypeKind::IRBasicType; } // TODO(ir) rename
    bool isPointerType() { return kind == IRTypeKind::IRPointerType; }
    bool isFunctionType() { return kind == IRTypeKind::IRFunctionType; }
    bool isArrayType() { return kind == IRTypeKind::IRArrayType; }
    bool isStruct() { return kind == IRTypeKind::IRStructType; }
    bool isUnion() { return kind == IRTypeKind::IRUnionType; }
    bool isInteger();
    bool isSignedInteger();
    bool isUnsignedInteger();
    bool isFloatingPoint();
    bool isChar();
    bool isBool();
    bool isVoid();
    IRType* getPointee();
    llvm::ArrayRef<IRType*> getFields();
    llvm::StringRef getName();
    IRType* getReturnType();
    llvm::ArrayRef<IRType*> getParamTypes();
    IRType* getElementType();
    int getArraySize();
    IRType* getPointerTo();
    bool equals(IRType* other);
};

struct IRBasicType : IRType {
    std::string name; // TODO(ir)

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRBasicType; }
};

struct IRPointerType : IRType {
    IRType* pointee;

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRPointerType; }
};

struct IRFunctionType : IRType {
    IRType* returnType;
    std::vector<IRType*> paramTypes;

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRFunctionType; }
};

struct IRArrayType : IRType {
    IRType* elementType;
    int size;

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRArrayType; }
};

struct IRStructType : IRType {
    std::vector<IRType*> elementTypes;
    std::string name;

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRStructType; }
};

struct IRUnionType : IRType {
    std::vector<IRType*> elementTypes;
    std::string name;

    static bool classof(const IRType* t) { return t->kind == IRTypeKind::IRUnionType; }
};

IRType* getILType(Type astType, bool toplevel = true);
llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, IRType* type);

// TODO(ir): Rename IRGenerator to IRBuilder? And LLVMGenerator to LLVMBuilder?
// TODO(ir): Reorder these and ensure names are consistent and simple.
enum class ValueKind {
    IRInstruction, // First instruction // TODO(ir): Remove, this is abstract.
    IRAllocaInst,
    IRReturnInst,
    IRBranchInst, // TODO(ir) rename to Goto?
    IRConditionalBranchInst,
    IRPhiInst,
    IRSwitchInst,
    IRLoadInst,
    IRStoreInst,
    IRInsertValueInst,
    IRExtractValueInst,
    IRCallInst,
    IRBinaryOp,
    IRUnaryOp,
    IRGetElementPtr,
    IRConstGEP,
    IRCastInst,
    IRUnreachable,
    IRSizeof, // Last instruction
    IRBasicBlock,
    IRFunction,
    IRParam,
    IRGlobalVariable,
    IRConstantString,
    IRConstantInt,
    IRConstantFP,
    IRConstantBool,
    IRConstantNull,
    IRUndefined,
    IRModule,
};

struct IRValue {
    IRType* getType() const;
    std::string getName() const;
    void print(llvm::raw_ostream& stream) const;

    ValueKind kind;
};

struct IRInstruction : IRValue {
    // TODO(ir) rename "e" in classof functions
    static bool classof(const IRValue* e) { return e->kind >= ValueKind::IRInstruction && e->kind <= ValueKind::IRSizeof; }
};

struct IRAllocaInst : IRInstruction {
    IRType* allocatedType;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRAllocaInst; }
};

struct IRReturnInst : IRInstruction {
    IRValue* value;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRReturnInst; }
};

struct IRBranchInst : IRInstruction {
    IRBasicBlock* destination;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRBranchInst; }
};

struct IRConditionalBranchInst : IRInstruction {
    IRValue* condition;
    IRBasicBlock* trueBlock;
    IRBasicBlock* falseBlock;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConditionalBranchInst; }
};

struct IRPhiInst : IRInstruction {
    std::vector<std::pair<IRValue*, IRBasicBlock*>> valuesAndPredecessors;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRPhiInst; }
};

// TODO(ir) Rename to Match so variable names are more beautiful?
struct IRSwitchInst : IRInstruction {
    IRValue* condition;
    IRBasicBlock* defaultBlock;
    std::vector<std::pair<IRValue*, IRBasicBlock*>> cases;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRSwitchInst; }
};

struct IRLoadInst : IRInstruction {
    IRValue* value;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRLoadInst; }
};

struct IRStoreInst : IRInstruction {
    IRValue* value;
    IRValue* pointer;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRStoreInst; }
};

struct IRInsertValueInst : IRInstruction {
    IRValue* aggregate;
    IRValue* value;
    int index;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRInsertValueInst; }
};

struct IRExtractValueInst : IRInstruction {
    IRValue* aggregate;
    int index;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRExtractValueInst; }
};

struct IRCallInst : IRInstruction {
    IRValue* function;
    std::vector<IRValue*> args;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRCallInst; }
};

struct IRBinaryOp : IRInstruction {
    BinaryOperator op;
    IRValue* left;
    IRValue* right;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRBinaryOp; }
};

struct IRUnaryOp : IRInstruction {
    UnaryOperator op;
    IRValue* operand;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRUnaryOp; }
};

struct IRGetElementPtr : IRInstruction {
    IRValue* pointer;
    std::vector<IRValue*> indexes;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRGetElementPtr; }
};

struct IRConstGEP : IRInstruction {
    IRValue* pointer;
    int index0, index1;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstGEP; }
};

struct IRCastInst : IRInstruction {
    IRValue* value;
    IRType* type;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRCastInst; }
};

struct IRUnreachable : IRInstruction {
    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRUnreachable; }
};

struct IRSizeof : IRInstruction {
    IRType* type;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRSizeof; }
};

// TODO(ir): Rename to simply Block?
struct IRBasicBlock : IRValue {
    std::string name;
    IRFunction* parent;
    std::vector<IRInstruction*> insts;

    IRBasicBlock(std::string name, IRFunction* parent = nullptr);
    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRBasicBlock; }
};

struct IRParam : IRValue {
    IRType* type;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRParam; }
};

struct IRFunction : IRValue {
    std::string mangledName;
    IRType* returnType;
    std::vector<IRParam> params;
    std::vector<IRBasicBlock*> body; // TODO(ir) rename to blocks?
    bool isExtern;
    bool isVariadic;
    SourceLocation location;
    int nameCounter = 0; // TODO(ir) cleanup, merge with IRGenerator::nameCounter?

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRFunction; }
};

struct IRGlobalVariable : IRValue {
    IRValue* value;
    std::string name;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRGlobalVariable; }
};

struct IRConstantString : IRValue {
    std::string value;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstantString; }
};

struct IRConstantInt : IRValue {
    IRType* type;
    llvm::APSInt value;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstantInt; }
};

struct IRConstantFP : IRValue {
    IRType* type;
    llvm::APFloat value;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstantFP; }
};

struct IRConstantBool : IRValue {
    bool value;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstantBool; }
};

struct IRConstantNull : IRValue {
    IRType* type;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRConstantNull; }
};

struct IRUndefined : IRValue {
    IRType* type;

    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRUndefined; }
};

struct IRModule {
    std::string name;
    std::vector<IRFunction*> functions;
    std::vector<IRGlobalVariable*> globalVariables;

    void print(llvm::raw_ostream& stream) const;
    static bool classof(const IRValue* e) { return e->kind == ValueKind::IRModule; }
};

} // namespace delta
