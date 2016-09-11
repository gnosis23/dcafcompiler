package org.bohao.decaf.asm

import org.bohao.decaf.asm.RegisterType.RegisterType

/**
  * Created by bohao on 2016/9/11.
  */
object RegisterType extends Enumeration {
    type RegisterType = Value
    val rax, rbx, rcx, rdx, rsp, rbp, rsi, rdi, r8, r9, r10, r11,
        r12, r13, r14, r15 = Value
}
