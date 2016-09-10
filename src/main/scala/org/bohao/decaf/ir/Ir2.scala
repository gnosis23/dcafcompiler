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

            // dump allocas
            f.allocas.foreach(alloc => {
                println("  " + dump(alloc))
            })

            // dump control-block
            f.blocks.foreach(block => {
                dump(block)
            })

            println("}\n")
        })
    }

    def dump(block: BasicBlock): Unit = {
        println(block.name + ":")
        block.insts.foreach(inst =>
            println("  " + dump(inst))
        )
        println("")
    }

    def dump(inst: Quad2): String = {
        inst match {
            case Br(block) =>
                return s"br ${s(block)}"
            case CondBr(cond, thenBody, elseBody) =>
                return s"br ${s(cond)} ${s(thenBody)}, ${s(elseBody)}"
            case Store(mem, startVal) =>
                return s"${s(mem)} = store ${s(startVal)}"
            case Load(dest, mem) =>
                return s"${s(dest)} = load ${s(mem)}"
            case IAdd(dest, src1, src2) =>
                return s"${s(dest)} = iadd ${s(src1)}, ${s(src2)}"
            case ISub(dest, src1, src2) =>
                return s"${s(dest)} = isub ${s(src1)}, ${s(src2)}"
            case IMul(dest, src1, src2) =>
                return s"${s(dest)} = imul ${s(src1)}, ${s(src2)}"
            case ITesteq(dest, src1, src2) =>
                return s"${s(dest)} = i== ${s(src1)}, ${s(src2)}"
            case ITestneq(dest, src1, src2) =>
                return s"${s(dest)} = i!= ${s(src1)}, ${s(src2)}"
            case ITestg(dest, src1, src2) =>
                return s"${s(dest)} = itestg ${s(src1)}, ${s(src2)}"
            case ITestge(dest, src1, src2) =>
                return s"${s(dest)} = itestge ${s(src1)}, ${s(src2)}"
            case ITestl(dest, src1, src2) =>
                return s"${s(dest)} = itestl ${s(src1)}, ${s(src2)}"
            case ITestle(dest, src1, src2) =>
                return s"${s(dest)} = itestle ${s(src1)}, ${s(src2)}"
            case ICmp(dest, src1) =>
                return s"${s(dest)} = icmp ${s(src1)}"
            case Ret(value) => return s"Ret ${s(value)}"
            case Ret0 => return s"Ret"
            case Call(retValue, func, args) =>
                val argString = args.foldLeft("")((msg, x) => s"$msg [${s(x)}] ")
                return s"${s(retValue)} = call @$func($argString)"
            case Assign(dest, value) =>
                return s"${s(dest)} = ${s(value)}"
            case Alloca(mem) =>
                return s"alloc mem[${mem.varname},${mem.id}]"
            case AllocaArray(mem, size) =>
                return s"alloc mem[${mem.varname},${mem.id}][${s(size)}]"
            case GetElement(dest, mem, index) =>
                return s"${s(dest)} = GetElement ${s(mem)}[${s(index)}]"
            case Neg(dest, src) =>
                return s"${s(dest)} = Neg ${s(src)}"
            case Rev(dest, src) =>
                return s"${s(dest)} = Rev ${s(src)}"
            case _ =>
        }
        inst.toString
    }

    private def s(op: Operand): String = {
        op match {
            case IntOperand(value) => value.toString
            case StrOperand(value) => s"'$value'"
            case BasicBlockOperand(block) => s"#${block.name}"
            case TempVarOperand(id) => s"%t$id"
            case ParamOperand(name) => "%" + name
            case VarOperand(v) => "%" + v
            case MemoryPointerOperand(name, id) => s"mem[$name,$id]"
            case _ => op.toString
        }
    }
}
