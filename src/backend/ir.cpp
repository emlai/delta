#include "ir.h"
#pragma warning(push, 0)
#include <llvm/ADT/StringSet.h>
#include <llvm/ADT/StringSwitch.h>
#pragma warning(pop)
#include "../ast/decl.h"

using namespace delta;

BasicBlock::BasicBlock(std::string name, delta::Function* parent) : Value { ValueKind::BasicBlock }, name(std::move(name)), parent(parent) {
    if (parent) {
        parent->body.push_back(this);
    }
}

static std::unordered_map<TypeBase*, IRType*> irTypes = { { nullptr, nullptr } };

IRType* delta::getIRType(Type astType) {
    auto it = irTypes.find(astType.base);
    if (it != irTypes.end()) return it->second;

    IRType* irType = nullptr;

    switch (astType.getKind()) {
        case TypeKind::BasicType: {
            if (astType.isVoid() || Type::isBuiltinScalar(astType.getName())) {
                irType = new IRBasicType { IRTypeKind::IRBasicType, astType.getName().str() };
            } else if (astType.isOptionalType() && astType.isImplementedAsPointer()) {
                irType = getIRType(astType.getWrappedType());
            } else if (astType.isEnumType()) {
                auto enumDecl = llvm::cast<EnumDecl>(astType.getDecl());
                auto tagType = getIRType(enumDecl->getTagType());

                if (enumDecl->hasAssociatedValues()) {
                    auto unionType = new IRUnionType { IRTypeKind::IRUnionType, {}, "" };
                    irType = new IRStructType { IRTypeKind::IRStructType, { tagType, unionType }, astType.getQualifiedTypeName() };
                    irTypes.emplace(astType.base, irType);
                    auto associatedTypes = map(enumDecl->getCases(), [](const EnumCase& c) { return getIRType(c.getAssociatedType()); });
                    unionType->elementTypes = std::move(associatedTypes);
                    return irType;
                } else {
                    irType = tagType;
                }
            } else if (astType.getDecl()) {
                auto structType = new IRStructType { IRTypeKind::IRStructType, {}, astType.getQualifiedTypeName() };
                irTypes.emplace(astType.base, structType);
                auto elementTypes = map(astType.getDecl()->fields, [](const FieldDecl& f) { return getIRType(f.type); });
                structType->elementTypes = std::move(elementTypes);
                return structType;
            } else {
                llvm_unreachable("unknown type");
            }
            break;
        }
        case TypeKind::ArrayType: {
            if (astType.isArrayWithConstantSize()) {
                auto elementType = getIRType(astType.getElementType());
                irType = new IRArrayType { IRTypeKind::IRArrayType, elementType, static_cast<int>(astType.getArraySize()) };
            } else if (astType.isArrayWithRuntimeSize()) {
                irType = getIRType(BasicType::get("ArrayRef", astType.getElementType()));
            } else {
                ASSERT(astType.isArrayWithUnknownSize());
                irType = getIRType(astType.getElementType().getPointerTo());
            }
            break;
        }
        case TypeKind::TupleType: {
            auto elementTypes = map(astType.getTupleElements(), [](const TupleElement& e) { return getIRType(e.type); });
            irType = new IRStructType { IRTypeKind::IRStructType, std::move(elementTypes), "" };
            break;
        }
        case TypeKind::FunctionType: {
            auto returnType = getIRType(astType.getReturnType());
            auto paramTypes = map(astType.getParamTypes(), [](Type t) { return getIRType(t); });
            auto functionType = new IRFunctionType { IRTypeKind::IRFunctionType, returnType, std::move(paramTypes) };
            irType = new IRPointerType { IRTypeKind::IRPointerType, functionType };
            break;
        }
        case TypeKind::PointerType: {
            auto pointeeType = getIRType(astType.getPointee());
            irType = new IRPointerType { IRTypeKind::IRPointerType, pointeeType };
            break;
        }
        case TypeKind::UnresolvedType:
            llvm_unreachable("cannot convert unresolved type to IR");
    }

    irTypes.emplace(astType.base, irType);
    return irType;
}

IRType* Value::getType() const {
    switch (kind) {
        case ValueKind::AllocaInst:
            return llvm::cast<AllocaInst>(this)->allocatedType->getPointerTo();
        case ValueKind::ReturnInst:
            llvm_unreachable("unhandled ReturnInst");
        case ValueKind::BranchInst:
            llvm_unreachable("unhandled BranchInst");
        case ValueKind::CondBranchInst:
            llvm_unreachable("unhandled CondBranchInst");
        case ValueKind::SwitchInst:
            llvm_unreachable("unhandled SwitchInst");
        case ValueKind::LoadInst:
            return llvm::cast<LoadInst>(this)->value->getType()->getPointee();
        case ValueKind::StoreInst:
            llvm_unreachable("unhandled StoreInst");
        case ValueKind::InsertInst:
            return llvm::cast<InsertInst>(this)->aggregate->getType();
        case ValueKind::ExtractInst: {
            auto extract = llvm::cast<ExtractInst>(this);
            return extract->aggregate->getType()->getElements()[extract->index];
        }
        case ValueKind::CallInst: {
            auto functionType = llvm::cast<CallInst>(this)->function->getType();
            if (functionType->isPointerType()) {
                return functionType->getPointee()->getReturnType();
            } else {
                return functionType->getReturnType();
            }
        }
        case ValueKind::BinaryInst: {
            auto binary = llvm::cast<BinaryInst>(this);
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
                    return getIRType(Type::getBool());
                case Token::DotDot:
                case Token::DotDotDot:
                    llvm_unreachable("range operators should be lowered");
                default:
                    return binary->left->getType();
            }
        }
        case ValueKind::UnaryInst: {
            auto unary = llvm::cast<UnaryInst>(this);
            switch (unary->op) {
                case Token::Not:
                    return getIRType(Type::getBool());
                default:
                    return unary->operand->getType();
            }
        }
        case ValueKind::GEPInst: {
            auto gep = llvm::cast<GEPInst>(this);
            auto baseType = gep->pointer->getType();
            for (size_t i = 1; i < gep->indexes.size(); ++i) {
                switch (baseType->getPointee()->kind) {
                    case IRTypeKind::IRArrayType:
                        baseType = baseType->getPointee()->getElementType()->getPointerTo();
                        break;
                    default:
                        llvm_unreachable("invalid non-const GEP target type");
                }
            }
            return baseType;
        }
        case ValueKind::ConstGEPInst: {
            auto gep = llvm::cast<ConstGEPInst>(this);
            auto baseType = gep->pointer->getType()->getPointee();
            switch (baseType->kind) {
                case IRTypeKind::IRStructType:
                    ASSERT(gep->index < baseType->getElements().size());
                    return baseType->getElements()[gep->index]->getPointerTo();
                case IRTypeKind::IRArrayType:
                    ASSERT(gep->index < baseType->getArraySize());
                    return baseType->getElementType()->getPointerTo();
                default:
                    llvm_unreachable("invalid const GEP target type");
            }
        }
        case ValueKind::CastInst:
            return llvm::cast<CastInst>(this)->type;
        case ValueKind::UnreachableInst:
            llvm_unreachable("unhandled UnreachableInst");
        case ValueKind::SizeofInst:
            return getIRType(Type::getInt());
        case ValueKind::BasicBlock:
            llvm_unreachable("unhandled BasicBlock");
        case ValueKind::Function: {
            auto function = llvm::cast<Function>(this);
            auto paramTypes = map(function->params, [](auto& p) { return p.type; });
            return (new IRFunctionType { IRTypeKind::IRFunctionType, function->returnType, std::move(paramTypes) })->getPointerTo();
        }
        case ValueKind::Parameter:
            return llvm::cast<Parameter>(this)->type;
        case ValueKind::GlobalVariable:
            return llvm::cast<GlobalVariable>(this)->value->getType()->getPointerTo();
        case ValueKind::ConstantString:
            return getIRType(Type::getChar(Mutability::Const).getPointerTo());
        case ValueKind::ConstantInt:
            return llvm::cast<ConstantInt>(this)->type;
        case ValueKind::ConstantFP:
            return llvm::cast<ConstantFP>(this)->type;
        case ValueKind::ConstantBool:
            return getIRType(Type::getBool());
        case ValueKind::ConstantNull:
            return llvm::cast<ConstantNull>(this)->type;
        case ValueKind::Undefined:
            return llvm::cast<Undefined>(this)->type;
    }

    llvm_unreachable("unhandled instruction kind");
}

const Expr* Value::getExpr() const {
    switch (kind) {
        case ValueKind::CallInst:
            return llvm::cast<CallInst>(this)->expr;
        case ValueKind::BinaryInst:
            return llvm::cast<BinaryInst>(this)->expr;
        case ValueKind::UnaryInst:
            return llvm::cast<UnaryInst>(this)->expr;
        case ValueKind::ConstGEPInst:
            return llvm::cast<ConstGEPInst>(this)->expr;
        default:
            return nullptr;
    }
}

std::string Value::getName() const {
    switch (kind) {
        case ValueKind::AllocaInst:
            return llvm::cast<AllocaInst>(this)->name;
        case ValueKind::ReturnInst:
            llvm_unreachable("unhandled ReturnInst");
        case ValueKind::BranchInst:
            llvm_unreachable("unhandled BranchInst");
        case ValueKind::CondBranchInst:
            llvm_unreachable("unhandled CondBranchInst");
        case ValueKind::SwitchInst:
            llvm_unreachable("unhandled SwitchInst");
        case ValueKind::LoadInst:
            return llvm::cast<LoadInst>(this)->name;
        case ValueKind::StoreInst:
            llvm_unreachable("unhandled StoreInst");
        case ValueKind::InsertInst:
            return llvm::cast<InsertInst>(this)->name;
        case ValueKind::ExtractInst:
            return llvm::cast<ExtractInst>(this)->name;
        case ValueKind::CallInst:
            return llvm::cast<CallInst>(this)->name;
        case ValueKind::BinaryInst:
            return llvm::cast<BinaryInst>(this)->name;
        case ValueKind::UnaryInst:
            return llvm::cast<UnaryInst>(this)->name;
        case ValueKind::GEPInst:
            return llvm::cast<GEPInst>(this)->name;
        case ValueKind::ConstGEPInst:
            return llvm::cast<ConstGEPInst>(this)->name;
        case ValueKind::CastInst:
            return llvm::cast<CastInst>(this)->name;
        case ValueKind::UnreachableInst:
            llvm_unreachable("unhandled UnreachableInst");
        case ValueKind::SizeofInst:
            return llvm::cast<SizeofInst>(this)->name;
        case ValueKind::BasicBlock:
            return llvm::cast<BasicBlock>(this)->name;
        case ValueKind::Function:
            return llvm::cast<Function>(this)->mangledName;
        case ValueKind::Parameter:
            return llvm::cast<Parameter>(this)->name;
        case ValueKind::GlobalVariable:
            return llvm::cast<GlobalVariable>(this)->name;
        case ValueKind::ConstantString:
            return '"' + llvm::cast<ConstantString>(this)->value + '"';
        case ValueKind::ConstantInt:
            return llvm::cast<ConstantInt>(this)->value.toString(10);
        case ValueKind::ConstantFP: {
            llvm::SmallString<128> buffer;
            llvm::cast<ConstantFP>(this)->value.toString(buffer);
            return std::string(buffer);
        }
        case ValueKind::ConstantBool:
            return llvm::cast<ConstantBool>(this)->value ? "true" : "false";
        case ValueKind::ConstantNull:
            return "null";
        case ValueKind::Undefined:
            return "undefined";
    }

    llvm_unreachable("unhandled instruction kind");
}

Value* Value::getBranchArgument() const {
    if (auto branch = llvm::dyn_cast<BranchInst>(this)) {
        return branch->argument;
    } else if (auto condBranch = llvm::dyn_cast<CondBranchInst>(this)) {
        return condBranch->argument;
    } else {
        llvm_unreachable("value has no branch argument");
    }
}

static bool isConstant(const Value* inst) {
    return inst->kind == ValueKind::ConstantInt || inst->kind == ValueKind::ConstantFP || inst->kind == ValueKind::ConstantString ||
           inst->kind == ValueKind::Undefined || inst->kind == ValueKind::ConstantNull || inst->kind == ValueKind::ConstantBool;
}

static std::unordered_map<const Value*, std::string> valuesNames;
static llvm::StringSet usedNames;

static std::string formatName(const Value* inst) {
    std::string str;
    llvm::raw_string_ostream s(str);

    if (isConstant(inst)) {
        s << inst->getType() << " " << inst->getName(); // Always print type for inline constants.
        return s.str();
    }

    std::string name;
    auto it = valuesNames.find(inst);
    if (it != valuesNames.end()) {
        name = it->second;
    } else {
        auto instName = inst->getName();
        name = instName;
        int i = 0;

        while (name.empty() || usedNames.count(name)) {
            name = instName + '_' + std::to_string(i++);
        }

        usedNames.insert(name);
        valuesNames.emplace(inst, name);
    }

    s << name;
    return std::move(s.str());
}

static std::string formatTypeAndName(const Value* inst) {
    std::string str;
    llvm::raw_string_ostream s(str);
    if (!isConstant(inst) && inst->kind != ValueKind::BasicBlock) {
        s << inst->getType() << " ";
    }
    s << formatName(inst);
    return std::move(s.str());
}

void Value::print(llvm::raw_ostream& stream) const {
    const auto indent = "    ";

    switch (this->kind) {
        case ValueKind::AllocaInst: {
            auto alloca = llvm::cast<AllocaInst>(this);
            stream << indent << formatTypeAndName(alloca) << " = alloca " << alloca->allocatedType;
            break;
        }
        case ValueKind::ReturnInst: {
            auto returnInst = llvm::cast<ReturnInst>(this);
            stream << indent << "return " << (returnInst->value ? formatName(returnInst->value) : "void");
            break;
        }
        case ValueKind::BranchInst: {
            auto branch = llvm::cast<BranchInst>(this);
            stream << indent << "br " << formatName(branch->destination);
            if (branch->argument) stream << "(" << formatName(branch->argument) << ")";
            break;
        }
        case ValueKind::CondBranchInst: {
            auto condBranch = llvm::cast<CondBranchInst>(this);
            stream << indent << "br " << formatName(condBranch->condition) << ", " << condBranch->trueBlock->name;
            if (condBranch->argument) stream << "(" << formatName(condBranch->argument) << ")";
            stream << ", " << condBranch->falseBlock->name;
            if (condBranch->argument) stream << "(" << formatName(condBranch->argument) << ")";
            break;
        }
        case ValueKind::SwitchInst: {
            auto switchInst = llvm::cast<SwitchInst>(this);
            stream << indent << "switch " << formatName(switchInst->condition) << " {\n";
            for (auto& p : switchInst->cases) {
                stream << indent << indent << formatName(p.first) << " -> " << p.second->name << "\n";
            }
            stream << indent << "}";
            break;
        }
        case ValueKind::LoadInst: {
            auto load = llvm::cast<LoadInst>(this);
            stream << indent << formatTypeAndName(load) << " = load " << formatName(load->value);
            break;
        }
        case ValueKind::StoreInst: {
            auto store = llvm::cast<StoreInst>(this);
            stream << indent << "store " << formatName(store->value) << " to " << formatName(store->pointer);
            break;
        }
        case ValueKind::InsertInst: {
            auto insert = llvm::cast<InsertInst>(this);
            stream << indent << formatTypeAndName(insert) << " = insertvalue " << formatName(insert->aggregate) << ", " << insert->index << ", "
                   << formatName(insert->value);
            break;
        }
        case ValueKind::ExtractInst: {
            auto extract = llvm::cast<ExtractInst>(this);
            stream << indent << formatTypeAndName(extract) << " = extractvalue " << formatName(extract->aggregate) << ", " << extract->index;
            break;
        }
        case ValueKind::CallInst: {
            auto call = llvm::cast<CallInst>(this);
            stream << indent << formatTypeAndName(call) << " = call " << formatName(call->function) << "(";
            for (auto& arg : call->args) {
                stream << formatTypeAndName(arg);
                if (&arg != &call->args.back()) stream << ", ";
            }
            stream << ")";
            break;
        }
        case ValueKind::BinaryInst: {
            auto binaryOp = llvm::cast<BinaryInst>(this);
            stream << indent << formatTypeAndName(binaryOp) << " = " << formatName(binaryOp->left) << " " << binaryOp->op << " " << formatName(binaryOp->right);
            break;
        }
        case ValueKind::UnaryInst: {
            auto unaryOp = llvm::cast<UnaryInst>(this);
            stream << indent << formatTypeAndName(unaryOp) << " = " << unaryOp->op << formatName(unaryOp->operand);
            break;
        }
        case ValueKind::GEPInst: {
            auto gep = llvm::cast<GEPInst>(this);
            stream << indent << formatTypeAndName(gep) << " = getelementptr " << formatName(gep->pointer);
            for (auto* index : gep->indexes) {
                stream << ", " << formatName(index);
            }
            break;
        }
        case ValueKind::ConstGEPInst: {
            auto gep = llvm::cast<ConstGEPInst>(this);
            stream << indent << formatTypeAndName(gep) << " = getelementptr " << formatName(gep->pointer) << ", " << gep->index;
            break;
        }
        case ValueKind::CastInst: {
            auto cast = llvm::cast<CastInst>(this);
            stream << indent << formatTypeAndName(cast) << " = cast " << formatName(cast->value) << " to " << cast->type;
            break;
        }
        case ValueKind::UnreachableInst: {
            stream << indent << "unreachable";
            break;
        }
        case ValueKind::SizeofInst:
            llvm_unreachable("unhandled SizeofInst");
        case ValueKind::BasicBlock:
            llvm_unreachable("handled via Function");
        case ValueKind::Function: {
            for (auto it = valuesNames.begin(); it != valuesNames.end();) {
                if (!it->first->isGlobal()) {
                    usedNames.erase(it->second);
                    it = valuesNames.erase(it);
                } else {
                    it++;
                }
            }

            auto function = llvm::cast<Function>(this);
            stream << "\n";
            if (function->isExtern) stream << "extern ";
            stream << function->returnType << " " << function->mangledName << "(";
            for (auto& param : function->params) {
                if (function->isExtern) {
                    stream << param.type;
                } else {
                    stream << formatTypeAndName(&param);
                }
                if (&param != &function->params.back()) stream << ", ";
            }
            stream << ")";
            if (!function->isExtern) {
                stream << " {\n";
                for (auto& block : function->body) {
                    if (&block != &function->body.front()) {
                        stream << "\n" << formatName(block);
                        if (block->parameter) stream << "(" << formatTypeAndName(block->parameter) << ")";
                        stream << ":\n";
                    }
                    for (auto* i : block->body) {
                        i->print(stream);
                    }
                }
                stream << "}";
            }
            break;
        }
        case ValueKind::Parameter:
            stream << formatTypeAndName(this);
            break;
        case ValueKind::GlobalVariable: {
            auto globalVariable = llvm::cast<GlobalVariable>(this);
            stream << "global " << formatName(globalVariable) << " = " << formatTypeAndName(globalVariable->value);
            break;
        }
        case ValueKind::ConstantString:
            llvm_unreachable("unhandled ConstantString");
        case ValueKind::ConstantInt:
            llvm_unreachable("unhandled ConstantInt");
        case ValueKind::ConstantFP:
            llvm_unreachable("unhandled ConstantFP");
        case ValueKind::ConstantBool:
            llvm_unreachable("unhandled ConstantBool");
        case ValueKind::ConstantNull:
            llvm_unreachable("unhandled ConstantNull");
        case ValueKind::Undefined:
            llvm_unreachable("unhandled Undefined");
    }

    stream << "\n";
}

bool Value::loads(Value* value, int gepIndex) {
    if (auto load = llvm::dyn_cast<LoadInst>(this)) {
        if (gepIndex == -1) {
            return load->value == value;
        } else {
            if (auto gep = llvm::dyn_cast<ConstGEPInst>(load->value)) {
                if (gep->pointer == value && gep->index == gepIndex) return true;
                if (auto valueLoad = llvm::dyn_cast<LoadInst>(value)) {
                    if (auto gepPointerLoad = llvm::dyn_cast<LoadInst>(gep->pointer)) {
                        if (valueLoad->value == gepPointerLoad->value) {
                            return true;
                        }
                    }
                }
                return false;
            }
        }
    }
    return false;
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
    if (!isBasicType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name)
        .Cases("int", "int8", "int16", "int32", "int64", "uint", "uint8", "uint16", "uint32", "uint64", true)
        .Default(false);
}

bool IRType::isSignedInteger() {
    if (!isBasicType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("int", "int8", "int16", "int32", "int64", true).Default(false);
}

bool IRType::isUnsignedInteger() {
    if (!isBasicType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("uint", "uint8", "uint16", "uint32", "uint64", true).Default(false);
}

bool IRType::isFloatingPoint() {
    if (!isBasicType()) return false;
    return llvm::StringSwitch<bool>(llvm::cast<IRBasicType>(this)->name).Cases("float", "float32", "float64", "float80", true).Default(false);
}

bool IRType::isChar() {
    if (!isBasicType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "char";
}

bool IRType::isBool() {
    if (!isBasicType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "bool";
}

bool IRType::isVoid() {
    if (!isBasicType()) return false;
    return llvm::cast<IRBasicType>(this)->name == "void";
}

IRType* IRType::getPointee() {
    return llvm::cast<IRPointerType>(this)->pointee;
}

llvm::ArrayRef<IRType*> IRType::getElements() {
    if (isUnion()) return llvm::cast<IRUnionType>(this)->elementTypes;
    return llvm::cast<IRStructType>(this)->elementTypes;
}

llvm::StringRef IRType::getName() {
    if (isBasicType()) return llvm::cast<IRBasicType>(this)->name;
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
    return new IRPointerType { IRTypeKind::IRPointerType, this };
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
                for (auto& element : type->getElements()) {
                    stream << element;
                    if (&element != &type->getElements().back()) stream << ", ";
                }
                return stream << " }";
            }

        case IRTypeKind::IRUnionType:
            if (type->getName() != "") {
                return stream << type->getName();
            } else {
                stream << "union { ";
                for (auto& element : type->getElements()) {
                    if (element) { // TODO: Element type should exist for all associated values
                        stream << element;
                    } else {
                        stream << "void";
                    }
                    if (&element != &type->getElements().back()) stream << ", ";
                }
                return stream << " }";
            }
    }

    llvm_unreachable("all cases handled");
}

bool IRType::equals(IRType* other) {
    switch (kind) {
        case IRTypeKind::IRBasicType:
            return other->isBasicType() && getName() == other->getName();

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
                if (getElements().size() != other->getElements().size()) return false;
                for (size_t i = 0; i < getElements().size(); ++i) {
                    if (!getElements()[i]->equals(other->getElements()[i])) return false;
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
