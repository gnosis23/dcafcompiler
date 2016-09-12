package org.bohao.decaf.asm

/**
  * Created by bohao on 2016/9/12.
  */
object AsmDumper {
    def dump(code : Code): Unit = {
        code.funcCodes.foreach(f => {
            dump(f)
        })

        code.stringLiterals.foreach(node => {
            println(node._1 + ":")
            println("  .string \"%s\"".format(node._2.replaceAll("\n", "\\\\n")))
        })
    }

    def dump(funcCode: FuncCode): Unit = {
        if (funcCode.name == "main") {
            println("  .globl main")
        }
        println(funcCode.name + ":")
        println(s"  enter  $$(${funcCode.stackSize}), $$0")
        funcCode.inst.foreach(i => println(dump(i)))
        println("  leave")
        println("  ret\n")
    }

    def dump(inst: Inst) : String = {
        inst match {
            case Mov(src, dest) => return s"  mov  ${s(src)}, ${s(dest)}"
            case Cmove(src, dest) => return s"  cmove  ${s(src)}, ${s(dest)}"
            case Cmovne(src, dest) =>
            case Cmovg(src, dest) =>
            case Cmovl(src, dest) =>
            case Cmovge(src, dest) =>
            case Cmovle(src, dest) =>
            case Enter(x) =>
            case Leave =>
            case Push(src) =>
            case Pop(dest) =>
            case Call(func) => return s"  call  ${s(func)}"
            case Ret =>
            case Jmp(label) => return s"  jmp  .${s(label)}"
            case Je(label) => return s"  je  .${s(label)}"
            case Jne(label) => return s"  jne  .${s(label)}"
            case Add(src, dest) => return s"  add  ${s(src)}, ${s(dest)}"
            case Sub(src, dest) =>
            case Imul(src, dest) =>
            case Idiv(src, dest) =>
            case Shr(src, dest) =>
            case Shl(src, dest) =>
            case Ror(src, dest) =>
            case Cmp(src, dest) => return s"  cmp  ${s(src)}, ${s(dest)}"
            case Tag(label) => return s".${s(label)}:"
        }
        throw new Error(inst.toString)
    }

    def s(op : Op) : String = {
        op match {
            case Imm(v) => s"$$$v"
            case RelativeMem(reg, offset) => s"$offset(%$reg)"
            case Register(reg) => "%" + reg.toString
            case Func(name) => name
            case Label(name) => name
        }
    }
}
