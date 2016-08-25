package org.bohao.decaf.ast

/**
  * Created by bohao on 2016/8/25.
  */
//    field_decl
//    : type (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']')
//    (',' (IDENTIFIER | IDENTIFIER '[' INTLITERAL ']'))* ';'
//    ;
case class FieldDeclNode(loc: Location, t: TypeNode,
                         names: java.util.List[NameNode])
    extends Node
