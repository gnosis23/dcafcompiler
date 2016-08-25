package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */

//    method_call
//    : method_name '(' (expr (',' expr)*)? ')'
//    | method_name '(' (callout_arg (',' callout_arg)*)? ')'
//    ;
abstract class MethodCallNode extends Node

case class ExpArgsMethodCallNode(name: MethodNameNode, arguments: java.util.List[ExpNode])
    extends MethodCallNode

case class CalloutArgsMethodCallNode(name: MethodNameNode, arguments: java.util.List[CalloutArgNode])
    extends MethodCallNode

case class MethodNameNode(id: VarNode) extends Node
