package com.bossfish.sql


case class ParameterBinding[T]( param:Parameter[T], value:T )


class Parameter[T] extends Expr[T] with SqlFragment
{
  def toSql( exp:SqlExpander ) = ("?",List(this))
}


class Binding
{
  private val parameters = new scala.collection.mutable.HashMap[Parameter[_],Any]()


  def update[T]( p:Parameter[T], v:T ) {
    parameters(p) = v
  }
  
  def apply[T]( p:Parameter[T] ) = parameters(p).asInstanceOf[T]


  def add[T]( pb:ParameterBinding[T] ) { parameters(pb.param) = pb.value}
}



object Binding
{
   def apply( params:ParameterBinding[_]* ) = {
     val b = new Binding()
     params.foreach { p =>
       b.add(p)
     }
     b
   }
   
}


