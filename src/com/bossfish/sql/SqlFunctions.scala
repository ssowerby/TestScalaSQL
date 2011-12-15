package com.bossfish.sql


object SqlFunctions
{
  def count( expr:Expr[_] ) = FunctionCall[Long]("count", List(expr))
  def sum[T:SqlNumeric]( expr:Expr[T] ) = FunctionCall[T]("sum", List(expr))
  def min[T:SqlComparable]( expr:Expr[T] ) = FunctionCall[T]("min", List(expr))
  def max[T:SqlComparable]( expr:Expr[T] ) = FunctionCall[T]("max", List(expr))
  def nvl[T:SqlType]( expr:Expr[Option[T]], dv:Expr[T] ) = FunctionCall[T]("nvl", List(expr,dv))
}