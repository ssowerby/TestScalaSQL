package com.bossfish.sql

import _root_.javax.management.ValueExp


trait SelectorMaker[I,E]
{
   def makeSelector( i:I ) : Selector[E]
   def makeExtractor : ResultExtractor[E]

}

trait Selector[T] extends SqlFragment
{
  def select() = this
  def expr:Expr[T]
}


case class UnaliasedSelector[T]( expr:Expr[T] ) extends Selector[T] with SqlFragment
{
  def as( alias:String ) = AliasedSelector(expr, alias)
  def toSql(exp: SqlExpander) = exp(expr)
}

case class AliasedSelector[T]( expr:Expr[T], alias:String ) extends Selector[T] with SqlFragment
{
  def toSql(exp: SqlExpander) = exp(expr) + " as \"" + alias + "\""
}

