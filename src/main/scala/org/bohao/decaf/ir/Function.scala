package org.bohao.decaf.ir

import scala.collection.mutable

/**
  * Created by bohao on 2016/9/6.
  */
class Function(val functionType: FunctionType) {
    var blocks = mutable.Seq[BasicBlock]()

    def addBlock(block: BasicBlock): Unit = {
        blocks = blocks :+ block
    }
}

object Function {
    def create(f: FunctionType): Function = {
        new Function(f)
    }
}