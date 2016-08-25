package org.bohao.decaf.ast

import java.util

/**
  * Created by bohao on 2016/8/25.
  */
case class ProgramNode(callouts: util.List[CalloutDeclNode],
                       fields: util.List[FieldDeclNode],
                       methods: util.List[MethodDeclNode])
    extends Node
{
    override def location(): Location = new Location(0, 0)

    override val loc: Location = null
}