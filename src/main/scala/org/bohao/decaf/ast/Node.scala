package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/23.
  */

abstract class Node {
    val loc: Location

    def location(): Location = loc
}