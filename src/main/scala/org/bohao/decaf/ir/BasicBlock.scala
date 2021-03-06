package org.bohao.decaf.ir

import scala.collection.mutable

/**
  * Created by bohao on 2016/8/31.
  */
class BasicBlock(var name: String, var parent: IFunction) {
    var hasExit = false
    var insts = mutable.LinkedList[Quad2]()

    def addInst(quad: Quad2): Quad2 = {
        // HACK: ignore rest instructions if break/continue/return encountered
        if (!hasExit) {
            quad match {
                case Br(block) => hasExit = true
                case CondBr(cond, thenBody, elseBody) => hasExit = true
                case Ret(value) => hasExit = true
                case Ret0 => hasExit = true
                case _ =>
            }
            insts = insts :+ quad
        }
        quad
    }
}

object BasicBlock {
    private val blockNames = mutable.Map[String, Int]()

    def create(name: String, func: IFunction): BasicBlock = {
        var id = 0
        if (blockNames.get(name).isDefined) {
            id = blockNames.get(name).get + 1
        }
        blockNames.update(name, id)

        val name0 = s"$name$id"
        val block = new BasicBlock(name0, func)
        func.addBlock(block)

        block
    }

    def create(name: String): BasicBlock = {
        var id = 0
        if (blockNames.get(name).isDefined) {
            id = blockNames.get(name).get + 1
        }
        blockNames.update(name, id)

        val name0 = s"$name$id"
        val block = new BasicBlock(name0, null)

        block
    }
}