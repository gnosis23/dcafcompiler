package org.bohao.decaf.compile

import scala.collection.mutable

/**
  * Created by bohao on 2016/8/27.
  */
class ErrorHandler {
    var errorCount = 0
    var errorMsgs: mutable.MutableList[String] = new mutable.MutableList[String]

    def hasError: Boolean = errorCount > 0

    def addError(msg: String)  {
        errorCount += 1
        errorMsgs.+=(msg)
    }

}
