package org.bohao.decaf.asm

import scala.collection.mutable

/**
  * Created by bohao on 2016/9/11.
  */
class FuncCode(var name: String) {
    var inst : mutable.MutableList[Inst] = mutable.MutableList()

    var stackSize : Int = 0

    def add(inst: Inst): Unit = {
        this.inst += inst
    }
}
