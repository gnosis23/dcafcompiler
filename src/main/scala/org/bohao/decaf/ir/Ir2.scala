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

            f.blocks.foreach(block => {
                dump(block)
            })

            println("}")
        })
    }

    def dump(block: BasicBlock): Unit = {
        println(block.name + ":")
        block.insts.foreach(inst =>
            println("  " + dump(inst))
        )
    }

    def dump(inst: Quad2): String = {
        inst match {
            case Br(block) =>
            case CondBr(cond, thenBody, elseBody) =>
            case Store(mem, startVal) =>
            case Load(dest, mem) =>
            case IAdd(dest, src1, src2) =>
                return s"${s(dest)} = iadd ${s(src1)}, ${s(src2)}"
            case ISub(dest, src1, src2) =>
                return s"${s(dest)} = isub ${s(src1)}, ${s(src2)}"
            case IMul(dest, src1, src2) =>
                return s"${s(dest)} = imul ${s(src1)}, ${s(src2)}"
            case ICmp(dest, src1) =>
            case Ret(value) => return s"Ret ${s(value)}"
            case Ret0 => return s"Ret"
            case Call(retValue, func, args) =>
                val argString = args.foldLeft("")((msg, x) => s"$msg [${s(x)}] ")
                return s"${s(retValue)} = call @${func.name}($argString)"
            case _ =>
        }
        ""
    }

    private def s(op: Operand): String = {
        op match {
            case IntOperand(value) => value.toString
            case StrOperand(value) => s"'$value'"
            case BasicBlockOperand(block) => s"#${block.name}"
            case TempVarOperand(id) => s"%t$id"
            case ParamOperand(name) => "%" + name
            case _ => op.toString
        }
    }
}
