package com.bossfish.sql


abstract class Join extends SqlFragment
{
  private val ON = Sql(" on ")

  type Ref = TableReference[_ <: Table]
   
  val ref1:Join#Ref
  val ref2:Join#Ref
  val join:Option[Expr[Boolean]] 

  val joinType:String
  def toSql( exp:SqlExpander ): Sql = exp(ref1) + " " + joinType + " join " + exp(ref2) + exp.optionally(" on (", join, ")")
}


case class InnerJoin( ref1:Join#Ref,
                      ref2:Join#Ref,
                      join:Option[Expr[Boolean]] )
      extends Join
{
  val joinType = "inner"

  def on(join: Expr[Boolean] ) = new InnerJoin(ref1,ref2,Some(join))
}


case class OuterJoin( ref1:Join#Ref,
                      ref2:Join#Ref,
                      join:Option[Expr[Boolean]],
                      joinType:String )
      extends Join
{
  def on(join: Expr[Boolean]) = new OuterJoin(ref1,ref2,Some(join),joinType)
}
