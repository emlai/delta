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

struct Function;
struct Block;
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

    bool isBasicType() { return kind == IRTypeKind::IRBasicType; } // TODO(ir) rename
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
    std::string name;

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

IRType* getILType(Type astType);
llvm::raw_ostream& operator<<(llvm::raw_ostream& stream, IRType* type);

// TODO(ir): Rename IRGenerator to IRBuilder? And LLVMGenerator to LLVMBuilder?
// TODO(ir): Reorder these and ensure names are consistent and simple.
enum class ValueKind {
    AllocaInst,
    ReturnInst,
    BranchInst,
    CondBranchInst,
    PhiInst,
    SwitchInst,
    LoadInst,
    StoreInst,
    InsertInst,
    ExtractInst,
    CallInst,
    BinaryInst,
    UnaryInst,
    GEPInst,
    ConstGEPInst,
    CastInst,
    UnreachableInst,
    SizeofInst,
    Block,
    Function,
    Parameter,
    GlobalVariable,
    ConstantString,
    ConstantInt,
    ConstantFP,
    ConstantBool,
    ConstantNull,
    Undefined,
    IRModule,
};

struct Value {
    IRType* getType() const;
    std::string getName() const;
    bool isTerminator() const { return kind == ValueKind::ReturnInst || kind == ValueKind::BranchInst || kind == ValueKind::CondBranchInst; }
    void print(llvm::raw_ostream& stream) const;

    ValueKind kind;
};

struct Instruction : Value {
    static bool classof(const Value* v) { return v->kind >= ValueKind::AllocaInst && v->kind <= ValueKind::SizeofInst; }
};

struct AllocaInst : Instruction {
    IRType* allocatedType;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::AllocaInst; }
};

struct ReturnInst : Instruction {
    Value* value;

    static bool classof(const Value* v) { return v->kind == ValueKind::ReturnInst; }
};

struct BranchInst : Instruction {
    Block* destination;

    static bool classof(const Value* v) { return v->kind == ValueKind::BranchInst; }
};

struct CondBranchInst : Instruction {
    Value* condition;
    Block* trueBlock;
    Block* falseBlock;

    static bool classof(const Value* v) { return v->kind == ValueKind::CondBranchInst; }
};

struct PhiInst : Instruction {
    std::vector<std::pair<Value*, Block*>> valuesAndPredecessors;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::PhiInst; }
};

struct SwitchInst : Instruction {
    Value* condition;
    Block* defaultBlock;
    std::vector<std::pair<Value*, Block*>> cases;

    static bool classof(const Value* v) { return v->kind == ValueKind::SwitchInst; }
};

struct LoadInst : Instruction {
    Value* value;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::LoadInst; }
};

struct StoreInst : Instruction {
    Value* value;
    Value* pointer;

    static bool classof(const Value* v) { return v->kind == ValueKind::StoreInst; }
};

struct InsertInst : Instruction {
    Value* aggregate;
    Value* value;
    int index;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::InsertInst; }
};

struct ExtractInst : Instruction {
    Value* aggregate;
    int index;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::ExtractInst; }
};

struct CallInst : Instruction {
    Value* function;
    std::vector<Value*> args;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::CallInst; }
};

struct BinaryInst : Instruction {
    BinaryOperator op;
    Value* left;
    Value* right;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::BinaryInst; }
};

struct UnaryInst : Instruction {
    UnaryOperator op;
    Value* operand;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::UnaryInst; }
};

struct GEPInst : Instruction {
    Value* pointer;
    std::vector<Value*> indexes;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::GEPInst; }
};

struct ConstGEPInst : Instruction {
    Value* pointer;
    int index0, index1;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstGEPInst; }
};

struct CastInst : Instruction {
    Value* value;
    IRType* type;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::CastInst; }
};

struct UnreachableInst : Instruction {
    static bool classof(const Value* v) { return v->kind == ValueKind::UnreachableInst; }
};

struct SizeofInst : Instruction {
    IRType* type;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::SizeofInst; }
};

// TODO(ir) rename to BasicBlock?
struct Block : Value {
    std::string name;
    Function* parent;
    std::vector<Instruction*> insts;

    Block(std::string name, Function* parent = nullptr);
    static bool classof(const Value* v) { return v->kind == ValueKind::Block; }
};

struct Parameter : Value {
    IRType* type;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::Parameter; }
};

struct Function : Value {
    std::string mangledName;
    IRType* returnType;
    std::vector<Parameter> params;
    std::vector<Block*> body; // TODO(ir) rename to blocks?
    bool isExtern;
    bool isVariadic;
    SourceLocation location;

    static bool classof(const Value* v) { return v->kind == ValueKind::Function; }
};

struct GlobalVariable : Value {
    Value* value;
    std::string name;

    static bool classof(const Value* v) { return v->kind == ValueKind::GlobalVariable; }
};

struct ConstantString : Value {
    std::string value;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstantString; }
};

struct ConstantInt : Value {
    IRType* type;
    llvm::APSInt value;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstantInt; }
};

struct ConstantFP : Value {
    IRType* type;
    llvm::APFloat value;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstantFP; }
};

struct ConstantBool : Value {
    bool value;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstantBool; }
};

struct ConstantNull : Value {
    IRType* type;

    static bool classof(const Value* v) { return v->kind == ValueKind::ConstantNull; }
};

struct Undefined : Value {
    IRType* type;

    static bool classof(const Value* v) { return v->kind == ValueKind::Undefined; }
};

struct IRModule {
    std::string name;
    std::vector<Function*> functions;
    std::vector<GlobalVariable*> globalVariables;

    void print(llvm::raw_ostream& stream) const;
    static bool classof(const Value* v) { return v->kind == ValueKind::IRModule; }
};

} // namespace delta
