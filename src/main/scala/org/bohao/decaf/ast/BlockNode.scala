package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//    block
//    : '{' field_decl* statement* '}'
//    ;
case class BlockNode(loc: Location,
                     decls: java.util.List[FieldDeclNode],
                     Stmts: java.util.List[StmtNode])
    extends Node {
}
