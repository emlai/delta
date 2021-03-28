#include "mangle.h"
#include <cctype>
#pragma warning(push, 0)
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/StringSwitch.h>
#pragma warning(pop)
#include "../ast/decl.h"
#include "../ast/module.h"

using namespace delta;

static void mangleType(llvm::raw_string_ostream& stream, Type type);

static const char deltaPrefix[] = "_E";

static bool isOperator(const FunctionDecl& functionDecl) {
    char ch = functionDecl.getName().front();
    return !std::isalpha(ch) && ch != '_';
}

static const char* operatorName(const FunctionDecl& functionDecl) {
    // TODO: Handle unary +, -, *, &.
    return llvm::StringSwitch<const char*>(functionDecl.getName())
        .Case("+", "pl")
        .Case("-", "mi")
        .Case("*", "ml")
        .Case("/", "dv")
        .Case("%", "rm")
        .Case("==", "eq")
        .Case("!=", "ne")
        .Case("<", "lt")
        .Case(">", "gt")
        .Case("<=", "le")
        .Case(">=", "ge")
        .Case("++", "pp")
        .Case("--", "mm")
        .Case("[]", "ix")
        .Case("[]=", "ixa")
        .Default(nullptr);
}

static void mangleIdentifier(llvm::raw_string_ostream& stream, llvm::StringRef name) {
    stream << name.size();
    stream << name;
}

static void mangleGenericArgs(llvm::raw_string_ostream& stream, llvm::ArrayRef<Type> genericArgs) {
    if (!genericArgs.empty()) {
        stream << 'I';
        for (Type genericArg : genericArgs) {
            mangleType(stream, genericArg);
        }
        stream << 'E';
    }
}

static void mangleType(llvm::raw_string_ostream& stream, Type type) {
    switch (type.getKind()) {
        case TypeKind::BasicType:
            if (type.isOptionalType()) {
                stream << 'O';
                mangleType(stream, type.getWrappedType());
            } else {
                mangleIdentifier(stream, type.getName());
                mangleGenericArgs(stream, type.getGenericArgs());
            }
            break;
        case TypeKind::ArrayType:
            stream << 'A';
            switch (type.getArraySize()) {
                case ArrayType::runtimeSize:
                    stream << 'R';
                    break;
                case ArrayType::unknownSize:
                    stream << 'U';
                    break;
                default:
                    ASSERT(type.getArraySize() > 0);
                    stream << type.getArraySize();
                    break;
            }
            stream << '_';
            mangleType(stream, type.getElementType());
            break;
        case TypeKind::TupleType:
            stream << 'T';
            for (auto& element : type.getTupleElements()) {
                mangleType(stream, element.type);
            }
            stream << '_';
            break;
        case TypeKind::FunctionType:
            stream << 'F';
            for (Type paramType : type.getParamTypes()) {
                mangleType(stream, paramType);
            }
            stream << '_';
            mangleType(stream, type.getReturnType());
            break;
        case TypeKind::PointerType:
            stream << 'P';
            mangleType(stream, type.getPointee());
            break;
        case TypeKind::UnresolvedType:
            llvm_unreachable("invalid unresolved type");
    }
}

std::string delta::mangleFunctionDecl(const FunctionDecl& functionDecl) {
    std::string mangled;
    llvm::raw_string_ostream stream(mangled);

    if (functionDecl.isExtern() || functionDecl.isMain()) {
        stream << functionDecl.getName();
    } else {
        stream << deltaPrefix;
        stream << 'N';

        mangleIdentifier(stream, functionDecl.getModule()->getName());

        if (auto* typeDecl = functionDecl.getTypeDecl()) {
            mangleIdentifier(stream, typeDecl->getName());
            mangleGenericArgs(stream, typeDecl->genericArgs);
        }

        if (isOperator(functionDecl)) {
            const char* name = operatorName(functionDecl);
            ASSERT(name);
            stream << name;
        } else {
            mangleIdentifier(stream, functionDecl.getName());
        }

        mangleGenericArgs(stream, functionDecl.genericArgs);
        stream << 'E';

        for (auto& param : functionDecl.getParams()) {
            if (param.isPublic) {
                mangleIdentifier(stream, param.getName());
            }
            mangleType(stream, param.type);
        }
    }

    stream.flush();
    return mangled;
}
