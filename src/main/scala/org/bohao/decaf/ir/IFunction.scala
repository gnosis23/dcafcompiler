package org.bohao.decaf.ir

import scala.collection.mutable

/**
  * Created by bohao on 2016/9/6.
  */
case class IFunction(functionType: FunctionType, name: String) {
    var blocks = mutable.Seq[BasicBlock]()

    def addBlock(block: BasicBlock): Unit = {
        blocks = blocks :+ block
    }
}
