package org.bohao.decaf.asm

import scala.collection.mutable

/**
  * Created by bohao on 2016/9/11.
  */
class Code {
    var stringLiterals = Map[String, String]()

    val funcCodes = mutable.MutableList[FuncCode]()
}
