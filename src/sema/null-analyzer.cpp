#include "null-analyzer.h"
#include "../ast/decl.h"
#include "../backend/ir.h"

using namespace delta;

bool NullAnalyzer::isDefinitelyNotNull(Value* nullableValue, Instruction* startFrom) {
    visited.clear();
    return isDefinitelyNotNullRecursive(nullableValue, startFrom);
}

bool NullAnalyzer::isDefinitelyNotNullRecursive(Value* nullableValue, Instruction* startFrom, int gepIndex) {
    auto block = NOTNULL(startFrom->parent);
    int startFromIndex = -1;

    for (int i = 0; i < block->body.size(); i++) {
        if (block->body[i] == startFrom) {
            startFromIndex = i;
            break;
        }
    }

    ASSERT(startFromIndex != -1);

    for (int i = startFromIndex - 1; i >= 0; i--) {
        Instruction* currentInst = block->body[i];

        // If gepIndex is specified, i.e. non-negative, we're searching for a GEP accessing nullableValue at that index.
        if (currentInst == nullableValue && gepIndex == -1) {
            if (auto load = llvm::dyn_cast<LoadInst>(nullableValue)) {
                if (auto gep = llvm::dyn_cast<ConstGEPInst>(load->value)) {
                    return isDefinitelyNotNullRecursive(gep->pointer, gep, gep->index);
                }
                return isDefinitelyNotNullRecursive(load->value, load);
            } else if (auto cast = llvm::dyn_cast<CastInst>(nullableValue)) {
                return isDefinitelyNotNullRecursive(cast->value, cast);
            } else if (auto gep = llvm::dyn_cast<ConstGEPInst>(nullableValue)) {
                return isDefinitelyNotNullRecursive(gep->pointer, gep, gep->index);
            }
        }

        if (auto store = llvm::dyn_cast<StoreInst>(currentInst)) {
            if (gepIndex == -1) {
                if (store->pointer == nullableValue) {
                    return isDefinitelyNotNullRecursive(store->value, store);
                }
            } else {
                if (auto gep = llvm::dyn_cast<ConstGEPInst>(store->pointer)) {
                    if (gep->pointer == nullableValue && gep->index == gepIndex) {
                        return false;
                    }
                    if (auto valueLoad = llvm::dyn_cast<LoadInst>(nullableValue)) {
                        if (auto gepPointerLoad = llvm::dyn_cast<LoadInst>(gep->pointer)) {
                            if (valueLoad->value == gepPointerLoad->value) {
                                return false;
                            }
                        }
                    }
                }
            }
        }
    }

    if (block->predecessors.empty()) {
        return false;
    }

    visited.insert(block);

    for (auto predecessor : block->predecessors) {
        if (visited.count(predecessor) || !isNotNullWhenComingFrom(nullableValue, predecessor, block, gepIndex)) {
            return false;
        }
    }

    return true;
}

bool NullAnalyzer::isNotNullWhenComingFrom(Value* nullableValue, BasicBlock* predecessor, BasicBlock* destination, int gepIndex) {
    auto lastInst = predecessor->body.back();

    if (auto condBr = llvm::dyn_cast<CondBranchInst>(lastInst)) {
        if (auto binary = llvm::dyn_cast<BinaryInst>(condBr->condition)) {
            if ((binary->left == nullableValue || binary->left->loads(nullableValue, gepIndex)) && binary->right->kind == ValueKind::ConstantNull) {
                if (binary->op == Token::Equal) {
                    return destination == condBr->falseBlock;
                } else if (binary->op == Token::NotEqual) {
                    return destination == condBr->trueBlock;
                }
                llvm_unreachable("invalid null comparison operator");
            }
        }
    }

    return isDefinitelyNotNullRecursive(nullableValue, lastInst, gepIndex);
}

void NullAnalyzer::analyze(IRModule* module) {
    for (auto function : module->functions) {
        for (auto block : function->body) {
            for (auto inst : block->body) {
                analyze(inst);
            }
        }
    }
}

void NullAnalyzer::analyze(Value* value) {
    switch (value->kind) {
        case ValueKind::CallInst: {
            auto call = llvm::cast<CallInst>(value);
            if (call->expr) {
                if (auto receiverType = call->expr->getReceiverType()) {
                    // isConstructorDecl check filters out Optional() calls.
                    if (receiverType.isOptionalType() && !call->expr->getCalleeDecl()->isConstructorDecl() && !isDefinitelyNotNull(call->args[0], call)) {
                        // TODO: Store the implicit 'this' receiver to the call expr during typechecking to simplify this code.
                        auto location = call->expr->getReceiver() ? call->expr->getReceiver()->location : call->expr->location;
                        WARN(location, "receiver may be null; unwrap it with a postfix '!' to silence this warning");
                    }
                }
            }
            break;
        }
        case ValueKind::BinaryInst: {
            auto binary = llvm::cast<BinaryInst>(value);
            if (llvm::isa<ConstantNull>(binary->right)) {
                ASSERT(binary->op == Token::Equal || binary->op == Token::NotEqual);
                if (binary->getExpr() && isDefinitelyNotNull(binary->left, binary)) {
                    WARN(binary->getExpr()->location, "value cannot be null here; null check can be removed");
                }
            }
            break;
        }
        case ValueKind::LoadInst: {
            auto load = llvm::cast<LoadInst>(value);
            if (auto expr = llvm::dyn_cast_or_null<UnaryExpr>(load->expr)) {
                if (expr->getOperand().type.isOptionalType() && !isDefinitelyNotNull(load->value, load)) {
                    WARN(expr->location, "dereferenced pointer may be null; unwrap it with a postfix '!' to silence this warning");
                }
            }
            break;
        }
        case ValueKind::ConstGEPInst: {
            auto gep = llvm::cast<ConstGEPInst>(value);
            if (gep->expr) {
                if (gep->expr->getBaseExpr()->type.isOptionalType() && !gep->expr->getBaseExpr()->isThis() && !isDefinitelyNotNull(gep->pointer, gep)) {
                    WARN(gep->expr->getBaseExpr()->location, "value may be null; unwrap it with a postfix '!' to silence this warning");
                }
            }
            break;
        }
        default:
            break;
    }
}
