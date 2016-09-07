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

    def createIAdd(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IAdd(TempVarOperand(), value1, value2))
    }

    def createISub(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(ISub(TempVarOperand(), value1, value2))
    }

    def createIMul(value1: Operand, value2: Operand): Quad2 = {
        insertBlock.addInst(IMul(TempVarOperand(), value1, value2))
    }

    def createICmp(value: Operand): Quad2 = {
        insertBlock.addInst(ICmp(TempVarOperand(), value))
    }

    def createRet(value: Operand): Quad2 = {
        insertBlock.addInst(Ret(value))
    }

    def createCall(func: IFunction, args: List[Operand]): Unit = {
        insertBlock.addInst(Call(TempVarOperand(), func, args))
    }
}
