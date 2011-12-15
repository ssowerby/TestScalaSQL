package com.bossfish.sql

sealed trait SortDirection
{
  val sql:String
}

object Ascending extends SortDirection { val sql = "ASC"}
object Descending extends SortDirection { val sql = "DESC" }


trait OrderBy extends SqlFragment
{
  val expr:Expr[_]
  val direction:SortDirection

  def toSql( exp:SqlExpander ) = exp(expr) + " " + direction.sql
}

case class OrderByWithUnspecifiedSort( expr:Expr[_] ) extends OrderBy
{
  val direction = Ascending
  def ascending = OrderByWithSpecifiedOrdering(expr, Ascending)
  def descending = OrderByWithSpecifiedOrdering(expr, Descending)
}


case class OrderByWithSpecifiedOrdering( expr:Expr[_], direction:SortDirection ) extends OrderBy


object OrderBy
{
  implicit def exprToOrderBy[T<%Expr[_]]( t:T ): OrderBy = OrderByWithUnspecifiedSort(t:Expr[_])
}
