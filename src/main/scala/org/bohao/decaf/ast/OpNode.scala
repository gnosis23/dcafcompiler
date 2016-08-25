package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//    bin_op
//    : arith_op
//    | rel_op
//    | eq_op
//    | cond_op
//    ;
abstract class OpNode extends Node

case class ArithOpNode(loc: Location, op: String) extends OpNode {
}

case class RelOpNode(loc: Location, op: String) extends OpNode {
}

case class EqOpNode(loc: Location, op: String) extends OpNode {
}

case class CondOpNode(loc: Location, op: String) extends OpNode {
}
