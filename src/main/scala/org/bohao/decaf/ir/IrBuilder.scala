package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/9/6.
  */
class IrBuilder {
    var insertBlock: BasicBlock = null

    def setInsertPoint(block : BasicBlock): Unit = {
        insertBlock = block
    }

    def getInsertBlock: BasicBlock = insertBlock

    def createBr(block: BasicBlockOperand): Quad2 = {
        insertBlock.addInst(Br(block))
    }

    def createCondBr(cond: Operand, thenBody: BasicBlockOperand,
                     elseBody: BasicBlockOperand): Quad2 = {
        insertBlock.addInst(CondBr(cond, thenBody, elseBody))
    }

    def createStore(startVal: Operand, alloca: MemoryPointerOperand): Quad2 = {
        insertBlock.addInst(Store(alloca, startVal))
    }

    def createLoad(alloca: MemoryPointerOperand): Quad2 = {
        insertBlock.addInst(Load(TempVarOperand(), alloca))
    }

    def createGetElement(mem: MemoryPointerOperand, size: Operand): Quad2 = {
        insertBlock.addInst(GetElement(TempArrayOperand(), mem, size))
    }

    def createIAdd(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IAdd(TempVarOperand(), value1, value2))
    }

    def createISub(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ISub(TempVarOperand(), value1, value2))
    }

    def createIMul(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IMul(TempVarOperand(), value1, value2))
    }

    def createIDiv(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IDiv(TempVarOperand(), value1, value2))
    }

    def createIMod(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IMod(TempVarOperand(), value1, value2))
    }

    def createITesteq(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITesteq(TempVarOperand(), value1, value2))
    }

    def createITestneq(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITestneq(TempVarOperand(), value1, value2))
    }

    def createITestl(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITestl(TempVarOperand(), value1, value2))
    }

    def createITestle(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITestle(TempVarOperand(), value1, value2))
    }

    def createITestg(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITestg(TempVarOperand(), value1, value2))
    }

    def createITestge(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ITestge(TempVarOperand(), value1, value2))
    }


    def createRet(value: Operand): Quad2 = {
        insertBlock.addInst(Ret(value))
    }

    def createRet(): Quad2 = {
        insertBlock.addInst(Ret0)
    }

    def createCall(func: String, args: List[Operand]): Quad2 = {
        insertBlock.addInst(Call(TempVarOperand(), func, args))
    }

    def createAssign(dest: Lhs, value: Operand): Quad2 = {
        insertBlock.addInst(Assign(dest, value))
    }

    def createNeg(dest: Lhs, value: Operand): Quad2 = {
        insertBlock.addInst(Neg(dest, value))
    }

    def createRev(dest: Lhs, value: Operand): Quad2 = {
        insertBlock.addInst(Rev(dest, value))
    }
}
