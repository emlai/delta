#include "irgen.h"

using namespace delta;

void IRGenerator::emitReturnStmt(const ReturnStmt& stmt) {
    // TODO: Emit deferred expressions and destructor calls after the evaluation of the return
    // value, but before emitting the return instruction. The return value expression may depend on
    // the values that the deferred expressions and/or destructor calls could deallocate.
    emitDeferredExprsAndDestructorCallsForReturn();

    if (auto* returnValue = stmt.getReturnValue()) {
        createReturn(emitExprForPassing(*returnValue, insertBlock->parent->returnType));
    } else {
        createReturn(currentDecl->isMain() ? createConstantInt(Type::getInt(), 0) : nullptr);
    }
}

void IRGenerator::emitVarStmt(const VarStmt& stmt) {
    auto* alloca = createEntryBlockAlloca(stmt.getDecl().getType(), stmt.getDecl().getName());
    setLocalValue(alloca, &stmt.getDecl());
    auto* initializer = stmt.getDecl().getInitializer();

    if (auto* callExpr = llvm::dyn_cast<CallExpr>(initializer)) {
        if (callExpr->getCalleeDecl()) {
            if (auto* constructorDecl = llvm::dyn_cast<ConstructorDecl>(callExpr->getCalleeDecl())) {
                if (constructorDecl->getTypeDecl()->getType() == stmt.getDecl().getType()) {
                    emitCallExpr(*callExpr, alloca);
                    return;
                }
            }
        }
    }

    if (!initializer->isUndefinedLiteralExpr()) {
        createStore(emitExprForPassing(*initializer, alloca->allocatedType), alloca);
    }
}

void IRGenerator::emitBlock(llvm::ArrayRef<Stmt*> stmts, IRBasicBlock* continuation) {
    beginScope();

    for (const auto& stmt : stmts) {
        emitStmt(*stmt);
        if (stmt->isReturnStmt() || stmt->isBreakStmt() || stmt->isContinueStmt()) break;
    }

    endScope();

    if (insertBlock->insts.empty() || !insertBlock->insts.back()->isTerminator()) {
        createBr(continuation);
    }
}

void IRGenerator::emitIfStmt(const IfStmt& ifStmt) {
    auto* condition = emitExpr(ifStmt.getCondition());

    // FIXME: Lower implicit null checks such as `if (ptr)` and `if (!ptr)` to null comparisons.
    if (condition->getType()->isPointerType()) {
        condition = emitImplicitNullComparison(condition);
    } else if (ifStmt.getCondition().getType().isOptionalType() && !ifStmt.getCondition().getType().getWrappedType().isPointerType()) {
        condition = createExtractValue(condition, optionalHasValueFieldIndex);
    }

    auto* function = insertBlock->parent;
    auto* thenBlock = new IRBasicBlock("if.then", function);
    auto* elseBlock = new IRBasicBlock("if.else", function);
    auto* endIfBlock = new IRBasicBlock("if.end", function);
    createCondBr(condition, thenBlock, elseBlock);

    setInsertPoint(thenBlock);
    emitBlock(ifStmt.getThenBody(), endIfBlock);

    setInsertPoint(elseBlock);
    emitBlock(ifStmt.getElseBody(), endIfBlock);

    setInsertPoint(endIfBlock);
}

void IRGenerator::emitSwitchStmt(const SwitchStmt& switchStmt) {
    IRValue* enumValue = nullptr;
    IRValue* condition = emitExprOrEnumTag(switchStmt.getCondition(), &enumValue);

    auto* function = insertBlock->parent;
    auto* insertBlockBackup = insertBlock;
    auto caseIndex = 0;

    auto cases = map(switchStmt.getCases(), [&](const SwitchCase& switchCase) {
        auto* value = emitExprOrEnumTag(*switchCase.getValue(), nullptr);
        auto* block = new IRBasicBlock("switch.case." + std::to_string(caseIndex++), function);
        return std::make_pair(value, block);
    });

    setInsertPoint(insertBlockBackup);
    auto* defaultBlock = new IRBasicBlock("switch.default", function);
    auto* end = new IRBasicBlock("switch.end", function);
    breakTargets.push_back(end);
    auto* switchInst = createSwitch(condition, defaultBlock);

    auto casesIterator = cases.begin();
    for (auto& switchCase : switchStmt.getCases()) {
        auto* value = casesIterator->first;
        auto* block = casesIterator->second;
        setInsertPoint(block);

        if (auto* associatedValue = switchCase.getAssociatedValue()) {
            auto type = associatedValue->getType().getPointerTo();
            auto* associatedValuePtr = createCast(createGEP(enumValue, 0, 1), type, associatedValue->getName());
            setLocalValue(associatedValuePtr, associatedValue);
        }

        emitBlock(switchCase.getStmts(), end);
        switchInst->cases.emplace_back(value, block);
        ++casesIterator;
    }

    setInsertPoint(defaultBlock);
    emitBlock(switchStmt.getDefaultStmts(), end);

    breakTargets.pop_back();
    setInsertPoint(end);
}

void IRGenerator::emitForStmt(const ForStmt& forStmt) {
    if (forStmt.getVariable()) {
        emitVarStmt(*forStmt.getVariable());
    }

    auto* increment = forStmt.getIncrement();
    auto* function = insertBlock->parent;
    auto* condition = new IRBasicBlock("loop.condition", function);
    auto* body = new IRBasicBlock("loop.body", function);
    auto* afterBody = increment ? new IRBasicBlock("loop.increment", function) : condition;
    auto* end = new IRBasicBlock("loop.end", function);

    breakTargets.push_back(end);
    continueTargets.push_back(afterBody);
    createBr(condition);

    setInsertPoint(condition);
    auto* conditionValue = emitExpr(*forStmt.getCondition());
    if (conditionValue->getType()->isPointerType()) {
        conditionValue = emitImplicitNullComparison(conditionValue);
    }
    createCondBr(conditionValue, body, end);

    setInsertPoint(body);
    emitBlock(forStmt.getBody(), afterBody);

    if (increment) {
        setInsertPoint(afterBody);
        emitExpr(*increment);
        createBr(condition);
    }

    breakTargets.pop_back();
    continueTargets.pop_back();
    setInsertPoint(end);
}

void IRGenerator::emitBreakStmt(const BreakStmt&) {
    ASSERT(!breakTargets.empty());
    createBr(breakTargets.back());
}

void IRGenerator::emitContinueStmt(const ContinueStmt&) {
    ASSERT(!continueTargets.empty());
    createBr(continueTargets.back());
}

void IRGenerator::emitCompoundStmt(const CompoundStmt& compoundStmt) {
    beginScope();

    for (auto& stmt : compoundStmt.getBody()) {
        emitStmt(*stmt);
    }

    endScope();
}

void IRGenerator::emitStmt(const Stmt& stmt) {
    switch (stmt.getKind()) {
        case StmtKind::ReturnStmt:
            emitReturnStmt(llvm::cast<ReturnStmt>(stmt));
            break;
        case StmtKind::VarStmt:
            emitVarStmt(llvm::cast<VarStmt>(stmt));
            break;
        case StmtKind::ExprStmt:
            emitLvalueExpr(llvm::cast<ExprStmt>(stmt).getExpr());
            break;
        case StmtKind::DeferStmt:
            deferEvaluationOf(llvm::cast<DeferStmt>(stmt).getExpr());
            break;
        case StmtKind::IfStmt:
            emitIfStmt(llvm::cast<IfStmt>(stmt));
            break;
        case StmtKind::SwitchStmt:
            emitSwitchStmt(llvm::cast<SwitchStmt>(stmt));
            break;
        case StmtKind::WhileStmt:
            llvm_unreachable("WhileStmt should be lowered into a ForStmt");
            break;
        case StmtKind::ForStmt:
            emitForStmt(llvm::cast<ForStmt>(stmt));
            break;
        case StmtKind::ForEachStmt:
            llvm_unreachable("ForEachStmt should be lowered into a ForStmt");
            break;
        case StmtKind::BreakStmt:
            emitBreakStmt(llvm::cast<BreakStmt>(stmt));
            break;
        case StmtKind::ContinueStmt:
            emitContinueStmt(llvm::cast<ContinueStmt>(stmt));
            break;
        case StmtKind::CompoundStmt:
            emitCompoundStmt(llvm::cast<CompoundStmt>(stmt));
            break;
    }
}
