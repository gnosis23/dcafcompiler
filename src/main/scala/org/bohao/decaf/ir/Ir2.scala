package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/9/7.
  */
class Ir2(var functions: List[IFunction]) {
    def dump(): Unit = {
        functions.foreach(f => {
            val proto = f.functionType
            print(s"${proto.retType} @${f.name}(")
            proto.args.foreach(pr => {
                print(s"[${pr._2} %${pr._1}] ")
            })
            println(") {")

            println("}")
        })
    }
}
