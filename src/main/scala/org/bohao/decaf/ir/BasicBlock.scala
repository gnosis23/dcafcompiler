package org.bohao.decaf.ir

/**
  * Created by bohao on 2016/8/31.
  */
class BasicBlock(var head: Quad = null) {


    def add(quad: Quad): Unit = {
        if (head == null) {
            head = quad
            head.next = head
            head.prev = head
        } else {
            head.prev.next = quad
            quad.prev = head.prev
            head.prev = quad
        }
    }
}
