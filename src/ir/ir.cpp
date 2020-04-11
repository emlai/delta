#include "ir.h"
#pragma warning(push, 0)
#include <llvm/ADT/StringSwitch.h>
#pragma warning(pop)
#include "../ast/decl.h"

using namespace delta;

// TODO(ir): Rename IRxxxInst to simply xxxInst, for consistency with xxxDecl, xxxStmt, and xxxExpr.
// TODO(ir): RUN: check_matches_ir %delta -print-il %s

IRBasicBlock::IRBasicBlock(std::string name, delta::IRFunction* parent) : IRValue{ValueKind::IRBasicBlock}, name(std::move(name)), parent(parent) {
    if (parent) {
        parent->body.push_back(this);
    }
}

// TODO(ir) store these in irgenerator?
static std::unordered_map<TypeBase*, IRType*> irTypes = {{nullptr, nullptr}};

// TODO(ir) remove toplevel param?
// TODO(ir) check this is consistent with old version of getLLVMType
IRType* delta::getILType(Type astType, bool toplevel) {
    auto it = irTypes.find(astType.getBase());
    if (it != irTypes.end()) return it->second;

    IRType* irType;

    switch (astType.getKind()) {
        case TypeKind::BasicType: {
            if (astType.isVoid() || Type::isBuiltinScalar(astType.getName())) {
                irType = new IRBasicType{IRTypeKind::IRBasicType, astType.getName()};
            } else if (astType.isOptionalType() && astType.isPointerTypeInLLVM()) {
                irType = getILType(astType.getWrappedType());
            } else if (astType.isEnumType()) {
                auto enumDecl = llvm::cast<EnumDecl>(astType.getDecl());
                auto tagType = getILType(enumDecl->getTagType());

                if (enumDecl->hasAssociatedValues()) {
                    auto unionType = new IRUnionType{IRTypeKind::IRUnionType, {}, ""};
                    irType = new IRStructType{IRTypeKind::IRStructType, {tagType, unionType}, astType.getQualifiedTypeName()};
                    irTypes.emplace(astType.getBase(), irType); // TODO(ir) special case for recursive enum, cleanup
                    auto associatedTypes = map(enumDecl->getCases(), [](const EnumCase& c) { return getILType(c.getAssociatedType()); });
                    unionType->elementTypes = std::move(associatedTypes);
                    return irType;
                } else {
                    irType = tagType;
                }
            } else if (astType.getDecl()) {
                auto structType = new IRStructType{IRTypeKind::IRStructType, {}, astType.getQualifiedTypeName()};
                irTypes.emplace(astType.getBase(), structType); // TODO(ir) special case for recursive struct, cleanup
                auto elementTypes = map(astType.getDecl()->getFields(), [](const FieldDecl& f) { return getILType(f.getType()); });
                structType->elementTypes = std::move(elementTypes);
                return structType;
            } else {
                llvm_unreachable("unknown type");
            }
            break;
        }
        case TypeKind::ArrayType: {
            if (astType.isArrayWithConstantSize()) {
                auto elementType = getILType(astType.getElementType());
                irType = new IRArrayType{IRTypeKind::IRArrayType, elementType, static_cast<int>(astType.getArraySize())};
            } else if (astType.isArrayWithRuntimeSize()) {
                irType = getILType(BasicType::get("ArrayRef", astType.getElementType()));
            } else {
                ASSERT(astType.isArrayWithUnknownSize());
                irType = getILType(astType.getElementType().getPointerTo());
            }
            break;
        }
        case TypeKind::TupleType: {
            auto elementTypes = map(astType.getTupleElements(), [](const TupleElement& e) { return getILType(e.type); });
            irType = new IRStructType{IRTypeKind::IRStructType, std::move(elementTypes), ""};
            break;
        }
        case TypeKind::FunctionType: {
            auto returnType = getILType(astType.getReturnType());
            auto paramTypes = map(astType.getParamTypes(), [](Type t) { return getILType(t); });
            auto functionType = new IRFunctionType{IRTypeKind::IRFunctionType, returnType, std::move(paramTypes)};
            irType = new IRPointerType{IRTypeKind::IRPointerType, functionType};
            break;
        }
        case TypeKind::PointerType: {
            auto pointeeType = getILType(astType.getPointee());
            irType = new IRPointerType{IRTypeKind::IRPointerType, pointeeType};
            break;
        }
        case TypeKind::UnresolvedType:
            llvm_unreachable("cannot convert unresolved type to IR");
    }

    //    if (!astType) return IRType(astType);
    //
    //    // TODO(ir) doesn't work  for T*
    //    if (toplevel) {
    //        if (astType.isFunctionType()) {
    //            return IRType(astType.getPointerTo());
    //        }
    //    }
    //
    //    if (astType.isArrayWithUnknownSize()) {
    //        return IRType(getILType(astType.getElementType(), false).getPointerTo());
    //    }
    //
    //    if (astType.isArrayWithRuntimeSize()) {
    //        return IRType(BasicType::get("ArrayRef", getILType(astType.getElementType())));
    //    }
    //
    //    // TODO(ir): add isConst()
    //    if (!astType.isMutable()) {
    //        return IRType(getILType(astType.withMutability(Mutability::Mutable), false));
    //    }
    //
    //    if (astType.isPointerType()) {
    //        return IRType(getILType(astType.getPointee(), false).getPointerTo());
    //    }
    //
    //    if (astType.isOptionalType() && astType.getWrappedType().isPointerTypeInLLVM()) {
    //        return IRType(getILType(astType.getWrappedType(), false));
    //    }
    //
    //    if (astType.isEnumType()) {
    //        auto enumDecl = llvm::cast<EnumDecl>(astType.getDecl());
    //        if (!enumDecl->hasAssociatedValues()) {
    //            return IRType(enumDecl->getTagType());
    //        }
    //        // TODO(ir): Handle enum with associated values?
    //    }

    irTypes.emplace(astType.getBase(), irType);
    return irType;
}

// TODO(ir) store type in IRValue?
IRType* IRValue::getType() const {
    switch (kind) {
        case ValueKind::IRInstruction:
            llvm_unreachable("unhandled IRInstruction");
        case ValueKind::IRAllocaInst:
            return llvm::cast<IRAllocaInst>(this)->allocatedType->getPointerTo();
        case ValueKind::IRReturnInst:
            llvm_unreachable("unhandled IRReturnInst");
        case ValueKind::IRBranchInst:
            llvm_unreachable("unhandled IRBranchInst");
        case ValueKind::IRConditionalBranchInst:
            llvm_unreachable("unhandled IRConditionalBranchInst");
        case ValueKind::IRPhiInst:
            // TODO(ir): Check incoming values have same type
            return llvm::cast<IRPhiInst>(this)->valuesAndPredecessors[0].first->getType();
        case ValueKind::IRSwitchInst:
            llvm_unreachable("unhandled IRSwitchInst");
        case ValueKind::IRLoadInst:
            return llvm::cast<IRLoadInst>(this)->value->getType()->getPointee();
        case ValueKind::IRStoreInst:
            llvm_unreachable("unhandled IRStoreInst");
        case ValueKind::IRInsertValueInst:
            return llvm::cast<IRInsertValueInst>(this)->aggregate->getType();
        case ValueKind::IRExtractValueInst: {
            auto extract = llvm::cast<IRExtractValueInst>(this);
            return extract->aggregate->getType()->getFields()[extract->index];
        }
        case ValueKind::IRCallInst: {
            auto functionType = llvm::cast<IRCallInst>(this)->function->getType();
            if (functionType->isPointerType()) {
                return functionType->getPointee()->getReturnType();
            } else {
                return functionType->getReturnType();
            }
        }
        case ValueKind::IRBinaryOp: {
            auto binary = llvm::cast<IRBinaryOp>(this);
            switch (binary->op) {
                case Token::AndAnd:
                case Token::OrOr:
                case Token::Equal:
                case Token::NotEqual:
                case Token::PointerEqual:
                case Token::PointerNotEqual:
                case Token::Less:
                case Token::LessOrEqual:
                case Token::Greater:
                case Token::GreaterOrEqual:
                    return getILType(Type::getBool());
                case Token::DotDot:
                case Token::DotDotDot:
                    llvm_unreachable("range operators should be lowered");
                default:
                    return binary->left->getType();
            }
        }
            // TODO(ir)
            //            ASSERT(*llvm::cast<IRBinaryOp>(this)->left->getType() == *llvm::cast<IRBinaryOp>(this)->right->getType());
        case ValueKind::IRUnaryOp: {
            auto unary = llvm::cast<IRUnaryOp>(this);
            switch (unary->op) {
                case Token::Not:
                    return getILType(Type::getBool());
                default:
                    return unary->operand->getType();
            }
        }
        case ValueKind::IRGetElementPtr: {
            auto gep = llvm::cast<IRGetElementPtr>(this);
            auto baseType = gep->pointer->getType();
            for (auto index : llvm::ArrayRef(gep->indexes).drop_front()) {
                switch (baseType->getPointee()->kind) {
                    case IRTypeKind::IRArrayType:
                        baseType = baseType->getPointee()->getElementType()->getPointerTo();
                        break;
                    case IRTypeKind::IRBasicType:
                    case IRTypeKind::IRStructType:
                    case IRTypeKind::IRUnionType:
                    case IRTypeKind::IRFunctionType:
                    case IRTypeKind::IRPointerType:
                        llvm_unreachable("invalid non-const GEP target type");
                }
            }
            return baseType;
        }
        case ValueKind::IRConstGEP: {
            auto gep = llvm::cast<IRConstGEP>(this);
            auto baseType = gep->pointer->getType();
            switch (baseType->getPointee()->kind) {
                case IRTypeKind::IRStructType:
                case IRTypeKind::IRUnionType: {
                    // TODO(ir) cleanup
                    //                    DEBUG_PRINT(baseType);
                    //                    DEBUG_PRINT(baseType.getPointeeInLLVM());

                    ASSERT(gep->index1 < baseType->getPointee()->getFields().size());
                    auto a = baseType->getPointee()->getFields()[gep->index1];
                    ASSERT(a);
                    baseType = a->getPointerTo();
                    break;
                }
                    //                case IRTypeKind::IRUnionType:
                    //                    auto a = baseType->getPointee()->getFields()[gep->index1];
                    //                    ASSERT(a);
                    //                    baseType = a->getPointerTo();
                    //                    break;
                case IRTypeKind::IRArrayType:
                    // TODO(ir) do we gep arrays?
                    baseType = baseType->getPointee()->getElementType()->getPointerTo();
                    break;
                case IRTypeKind::IRBasicType:
                case IRTypeKind::IRFunctionType:
                case IRTypeKind::IRPointerType:
                    llvm_unreachable("invalid const GEP target type");
            }
            return baseType;
        }
        case ValueKind::IRCastInst:
            return llvm::cast<IRCastInst>(this)->type;
        case ValueKind::IRUnreachable:
            llvm_unreachable("unhandled IRUnreachable");
        case ValueKind::IRSizeof:
            return getILType(Type::getInt());
        case ValueKind::IRBasicBlock:
            llvm_unreachable("unhandled IRBasicBlock");
        case ValueKind::IRFunction: {
            auto function = llvm::cast<IRFunction>(this);
            auto paramTypes = map(function->params, [](auto& p) { return p.type; });
            return (new IRFunctionType{IRTypeKind::IRFunctionType, function->returnType, std::move(paramTypes)})->getPointerTo(); // TODO(ir) cache?
        }
        case ValueKind::IRParam:
            return llvm::cast<IRParam>(this)->type;
        case ValueKind::IRGlobalVariable:
            return llvm::cast<IRGlobalVariable>(this)->value->getType()->getPointerTo();
        case ValueKind::IRConstantString:
            return getILType(Type::getChar(Mutability::Const).getPointerTo());
        case ValueKind::IRConstantInt:
            return llvm::cast<IRConstantInt>(this)->type;
        case ValueKind::IRConstantFP:
            return llvm::cast<IRConstantFP>(this)->type;
        case ValueKind::IRConstantBool:
            return getILType(Type::getBool());
        case ValueKind::IRConstantNull:
            return llvm::cast<IRConstantNull>(this)->type;
        case ValueKind::IRUndefined:
            return llvm::cast<IRUndefined>(this)->type;
        case ValueKind::IRModule:
            llvm_unreachable("unhandled IRModule");
    }

    llvm_unreachable("unhandled instruction kind");
}

// TODO(ir): Check empty strings
std::string IRValue::getName() const {
    switch (kind) {
        case ValueKind::IRInstruction:
            llvm_unreachable("unhandled IRInstruction");
        case ValueKind::IRAllocaInst:
            return llvm::cast<IRAllocaInst>(this)->name;
        case ValueKind::IRReturnInst:
            llvm_unreachable("unhandled IRReturnInst");
        case ValueKind::IRBranchInst:
            llvm_unreachable("unhandled IRBranchInst");
        case ValueKind::IRConditionalBranchInst:
            llvm_unreachable("unhandled IRConditionalBranchInst");
        case ValueKind::IRPhiInst:
            return llvm::cast<IRPhiInst>(this)->name;
        case ValueKind::IRSwitchInst:
            llvm_unreachable("unhandled IRSwitchInst");
        case ValueKind::IRLoadInst:
            return llvm::cast<IRLoadInst>(this)->name;
        case ValueKind::IRStoreInst:
            llvm_unreachable("unhandled IRStoreInst");
        case ValueKind::IRInsertValueInst:
            return llvm::cast<IRInsertValueInst>(this)->name;
        case ValueKind::IRExtractValueInst:
            return llvm::cast<IRExtractValueInst>(this)->name;
        case ValueKind::IRCallInst:
            return llvm::cast<IRCallInst>(this)->name;
        case ValueKind::IRBinaryOp:
            return llvm::cast<IRBinaryOp>(this)->name;
        case ValueKind::IRUnaryOp:
            return llvm::cast<IRUnaryOp>(this)->name;
        case ValueKind::IRGetElementPtr:
            return llvm::cast<IRGetElementPtr>(this)->name;
        case ValueKind::IRConstGEP:
            return llvm::cast<IRConstGEP>(this)->name;
        case ValueKind::IRCastInst:
            return llvm::cast<IRCastInst>(this)->name;
        case ValueKind::IRUnreachable:
            llvm_unreachable("unhandled IRUnreachable");
        case ValueKind::IRSizeof:
            return llvm::cast<IRSizeof>(this)->name;
        case ValueKind::IRBasicBlock:
            return llvm::cast<IRBasicBlock>(this)->name;
        case ValueKind::IRFunction:
            return llvm::cast<IRFunction>(this)->mangledName;
        case ValueKind::IRParam:
            return llvm::cast<IRParam>(this)->name;
        case ValueKind::IRGlobalVariable:
            return llvm::cast<IRGlobalVariable>(this)->name;
        case ValueKind::IRConstantString:
            return '"' + llvm::cast<IRConstantString>(this)->value + '"';
        case ValueKind::IRConstantInt:
            return llvm::cast<IRConstantInt>(this)->value.toString(10);
        case ValueKind::IRConstantFP: {
            llvm::SmallString<128> buffer;
            llvm::cast<IRConstantFP>(this)->value.toString(buffer);
            return buffer.str();
        }
        case ValueKind::IRConstantBool:
            return llvm::cast<IRConstantBool>(this)->value ? "true" : "false";
        case ValueKind::IRConstantNull:
            return "null";
        case ValueKind::IRUndefined:
            return "undefined";
        case ValueKind::IRModule:
            llvm_unreachable("unhandled IRModule");
    }

    llvm_unreachable("unhandled instruction kind");
}

static std::unordered_map<const IRValue*, std::string> generatedNames; // TODO(ir) ugly global
static int localNameCounter = 0;

static bool isConstant(const IRValue* inst) {
    return inst->kind == ValueKind::IRConstantInt || inst->kind == ValueKind::IRConstantFP || inst->kind == ValueKind::IRConstantString ||
           inst->kind == ValueKind::IRUndefined || inst->kind == ValueKind::IRConstantNull || inst->kind == ValueKind::IRConstantBool;
}

static std::string formatName(const IRValue* inst) {
    std::string str;
    llvm::raw_string_ostream s(str);

    if (isConstant(inst)) {
        s << inst->getType() << " "; // Always print type for inline constants.
    } else {
        s << "%";
    }

    auto name = inst->getName();
    if (name.empty()) {
        auto it = generatedNames.find(inst);
        if (it != generatedNames.end()) {
            name = it->second;
        } else {
            name = std::to_string(localNameCounter++);
            generatedNames.emplace(inst, name);
        }
    }

    s << name;
    return std::move(s.str());
}

static std::string formatTypeAndName(const IRValue* inst) {
    std::string str;
    llvm::raw_string_ostream s(str);
    if (!isConstant(inst) && inst->kind != ValueKind::IRBasicBlock) {
        s << inst->getType() << " ";
    }
    s << formatName(inst);
    return std::move(s.str());
}

void IRValue::print(llvm::raw_ostream& stream) const {
    const auto indent = "    ";

    switch (this->kind) {
        case ValueKind::IRInstruction:
            llvm_unreachable("unhandled IRInstruction");
        case ValueKind::IRAllocaInst: {
            auto alloca = llvm::cast<IRAllocaInst>(this);
            stream << indent << formatTypeAndName(alloca) << " = alloca " << alloca->allocatedType;
            break;
        }
        case ValueKind::IRReturnInst: {
            auto returnInst = llvm::cast<IRReturnInst>(this);
            stream << indent << "return " << (returnInst->value ? formatName(returnInst->value) : "void");
            break;
        }
        case ValueKind::IRBranchInst: {
            auto branch = llvm::cast<IRBranchInst>(this);
            stream << indent << "goto " << formatName(branch->destination);
            break;
        }
        case ValueKind::IRConditionalBranchInst: {
            auto condBranch = llvm::cast<IRConditionalBranchInst>(this);
            stream << indent << "goto " << formatName(condBranch->condition) << " ? " << condBranch->trueBlock->name << " : " << condBranch->falseBlock->name;
            break;
        }
        case ValueKind::IRPhiInst: {
            auto phi = llvm::cast<IRPhiInst>(this);
            stream << indent << formatTypeAndName(phi) << " = phi ";
            for (auto& p : phi->valuesAndPredecessors) {
                stream << "[" << formatName(p.first) << ", " << p.second->name << "]";
                if (&p != &phi->valuesAndPredecessors.back()) stream << ", ";
            }
            break;
        }
        case ValueKind::IRSwitchInst: {
            auto switchInst = llvm::cast<IRSwitchInst>(this);
            stream << indent << "switch " << formatName(switchInst->condition) << " {\n";
            for (auto& p : switchInst->cases) {
                stream << indent << indent << formatName(p.first) << " -> " << p.second->name << "\n";
            }
            stream << indent << "}";
            break;
        }
        case ValueKind::IRLoadInst: {
            auto load = llvm::cast<IRLoadInst>(this);
            stream << indent << formatTypeAndName(load) << " = load " << formatName(load->value);
            break;
        }
        case ValueKind::IRStoreInst: {
            auto store = llvm::cast<IRStoreInst>(this);
            stream << indent << "store " << formatName(store->value) << " to " << formatName(store->pointer);
            break;
        }
        case ValueKind::IRInsertValueInst: {
            auto insert = llvm::cast<IRInsertValueInst>(this);
            stream << indent << formatTypeAndName(insert) << " = insertvalue " << formatName(insert->aggregate) << ", " << insert->index << ", "
                   << formatName(insert->value);
            break;
        }
        case ValueKind::IRExtractValueInst: {
            auto extract = llvm::cast<IRExtractValueInst>(this);
            stream << indent << formatTypeAndName(extract) << " = extractvalue " << formatName(extract->aggregate) << ", " << extract->index;
            break;
        }
        case ValueKind::IRCallInst: {
            auto call = llvm::cast<IRCallInst>(this);
            stream << indent << formatTypeAndName(call) << " = call " << formatName(call->function) << "(";
            for (auto& arg : call->args) {
                stream << formatTypeAndName(arg);
                if (&arg != &call->args.back()) stream << ", ";
            }
            stream << ")";
            break;
        }
        case ValueKind::IRBinaryOp: {
            auto binaryOp = llvm::cast<IRBinaryOp>(this);
            stream << indent << formatTypeAndName(binaryOp) << " = " << formatName(binaryOp->left) << " " << binaryOp->op << " " << formatName(binaryOp->right);
            break;
        }
        case ValueKind::IRUnaryOp: {
            auto unaryOp = llvm::cast<IRUnaryOp>(this);
            stream << indent << formatTypeAndName(unaryOp) << " = " << unaryOp->op << formatName(unaryOp->operand);
            break;
        }
        case ValueKind::IRGetElementPtr: {
            auto gep = llvm::cast<IRGetElementPtr>(this);
            stream << indent << formatTypeAndName(gep) << " = getelementptr " << formatName(gep->pointer);
            for (auto* index : gep->indexes) {
                stream << ", " << formatName(index);
            }
            break;
        }
        case ValueKind::IRConstGEP: {
            auto gep = llvm::cast<IRConstGEP>(this);
            stream << indent << formatTypeAndName(gep) << " = getelementptr " << formatName(gep->pointer) << ", " << gep->index0 << ", " << gep->index1;
            break;
        }
        case ValueKind::IRCastInst: {
            auto cast = llvm::cast<IRCastInst>(this);
            stream << indent << formatTypeAndName(cast) << " = cast " << formatName(cast->value) << " to " << cast->type;
            break;
        }
        case ValueKind::IRUnreachable: {
            stream << indent << "unreachable";
            break;
        }
        case ValueKind::IRSizeof:
            llvm_unreachable("unhandled IRSizeof");
        case ValueKind::IRBasicBlock:
            llvm_unreachable("handled via IRFunction");
        case ValueKind::IRFunction: {
            localNameCounter = 0;
            auto function = llvm::cast<IRFunction>(this);
            stream << "\n";
            if (function->isExtern) stream << "extern ";
            stream << function->returnType << " " << function->mangledName << "(";
            for (auto& param : function->params) {
                stream << formatTypeAndName(&param);
                if (&param != &function->params.back()) stream << ", ";
            }
            stream << ")";
            if (!function->isExtern) {
                stream << " {\n";
                for (auto& block : function->body) {
                    if (&block != &function->body.front()) {
                        stream << "\n" << block->name << ":\n";
                    }
                    for (auto* i : block->insts) {
                        i->print(stream);
                    }
                }
                stream << "}";
            }
            break;
        }
        case ValueKind::IRParam:
            stream << formatTypeAndName(this);
            break;
        case ValueKind::IRGlobalVariable: {
            auto globalVariable = llvm::cast<IRGlobalVariable>(this);
            stream << "global " << formatName(globalVariable) << " = " << formatTypeAndName(globalVariable->value);
            break;
        }
        case ValueKind::IRConstantString:
            llvm_unreachable("unhandled IRConstantString");
        case ValueKind::IRConstantInt:
            llvm_unreachable("unhandled IRConstantInt");
        case ValueKind::IRConstantFP:
            llvm_unreachable("unhandled IRConstantFP");
        case ValueKind::IRConstantBool:
            llvm_unreachable("unhandled IRConstantBool");
        case ValueKind::IRConstantNull:
            llvm_unreachable("unhandled IRConstantNull");
        case ValueKind::IRUndefined:
            llvm_unreachable("unhandled IRUndefined");
        case ValueKind::IRModule:
            llvm_unreachable("unhandled IRModule");
    }

    stream << "\n";
}

void IRModule::print(llvm::raw_ostream& stream) const {
    for (auto* globalVariable : globalVariables) {
        globalVariable->print(stream);
    }

    for (auto* function : functions) {
        function->print(stream);
    }
}

bool IRType::isInteger() {
    if (!isPrimitiveType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name)
        .Cases("int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", true)
        .Default(false);
}

bool IRType::isSignedInteger() {
    if (!isPrimitiveType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("int", "int8", "int16", "int32", "int64", true).Default(false);
}

bool IRType::isUnsignedInteger() {
    if (!isPrimitiveType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("uint", "uint8", "uint16", "uint32", "uint64", true).Default(false);
}

bool IRType::isFloatingPoint() {
    if (!isPrimitiveType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("float", "float32", "float64", "float80", true).Default(false);
}

bool IRType::isChar() {
    if (!isPrimitiveType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "char";
}

bool IRType::isBool() {
    if (!isPrimitiveType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "bool";
}

bool IRType::isVoid() {
    if (!isPrimitiveType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "void";
}

IRType* IRType::getPointee() {
    return llvm::cast<IRPointerType>(this)->pointee;
}

llvm::ArrayRef<IRType*> IRType::getFields() { // TODO(ir) rename to fieldtypes
    if (isUnion()) return llvm::cast<IRUnionType>(this)->elementTypes;
    return llvm::cast<IRStructType>(this)->elementTypes;
}

llvm::StringRef IRType::getName() {
    if (isPrimitiveType()) return llvm::cast<IRBasicType>(this)->name;
    if (isUnion()) return llvm::cast<IRUnionType>(this)->name;
    return llvm::cast<IRStructType>(this)->name;
}

IRType* IRType::getReturnType() {
    return llvm::cast<IRFunctionType>(this)->returnType;
}

llvm::ArrayRef<IRType*> IRType::getParamTypes() {
    return llvm::cast<IRFunctionType>(this)->paramTypes;
}

IRType* IRType::getElementType() {
    return llvm::cast<IRArrayType>(this)->elementType;
}

int IRType::getArraySize() {
    return llvm::cast<IRArrayType>(this)->size;
}

IRType* IRType::getPointerTo() {
    return new IRPointerType{IRTypeKind::IRPointerType, this}; // TODO(ir) cache?
}

llvm::raw_ostream& delta::operator<<(llvm::raw_ostream& stream, IRType* type) {
    switch (type->kind) {
        case IRTypeKind::IRBasicType:
            return stream << type->getName();

        case IRTypeKind::IRPointerType:
            return stream << type->getPointee() << "*";

        case IRTypeKind::IRFunctionType:
            stream << type->getReturnType() << "(";
            for (auto& paramType : type->getParamTypes()) {
                stream << paramType;
                if (&paramType != &type->getParamTypes().back()) stream << ", ";
            }
            return stream << ")";

        case IRTypeKind::IRArrayType:
            return stream << type->getElementType() << "[" << type->getArraySize() << "]";

        case IRTypeKind::IRStructType:
            if (type->getName() != "") {
                return stream << type->getName();
            } else {
                stream << "{ ";
                for (auto& field : type->getFields()) {
                    stream << field;
                    if (&field != &type->getFields().back()) stream << ", ";
                }
                return stream << " }";
            }

        case IRTypeKind::IRUnionType:
            if (type->getName() != "") {
                return stream << type->getName();
            } else {
                stream << "union { ";
                for (auto& field : type->getFields()) {
                    stream << field;
                    if (&field != &type->getFields().back()) stream << ", ";
                }
                return stream << " }";
            }
    }

    llvm_unreachable("all cases handled");
}

// TODO(ir) beautify?
bool IRType::equals(IRType* other) {
    switch (kind) {
        case IRTypeKind::IRBasicType:
            return other->isPrimitiveType() && getName() == other->getName();

        case IRTypeKind::IRPointerType:
            return other->isPointerType() && getPointee()->equals(other->getPointee());

        case IRTypeKind::IRFunctionType:
            if (!other->isFunctionType()) return false;
            if (!getReturnType()->equals(other->getReturnType())) return false;
            if (getParamTypes().size() != other->getParamTypes().size()) return false;
            for (size_t i = 0; i < getParamTypes().size(); ++i) {
                if (!getParamTypes()[i]->equals(other->getParamTypes()[i])) return false;
            }
            return true;

        case IRTypeKind::IRArrayType:
            if (!other->isArrayType()) return false;
            if (getArraySize() != other->getArraySize()) return false;
            if (!getElementType()->equals(other->getElementType())) return false;
            return true;

        case IRTypeKind::IRStructType:
            if (!other->isStruct()) return false;
            if (getName() != other->getName()) return false;
            if (getName().empty()) {
                if (getFields().size() != other->getFields().size()) return false;
                for (size_t i = 0; i < getFields().size(); ++i) {
                    if (!getFields()[i]->equals(other->getFields()[i])) return false;
                }
            }
            return true;

        case IRTypeKind::IRUnionType:
            if (!other->isUnion()) return false;
            if (getName() != other->getName()) return false;
            return true;
    }

    llvm_unreachable("all cases handled");
}
