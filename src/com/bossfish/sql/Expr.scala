package com.bossfish.sql


trait Expr[T] extends SqlFragment {

   def in ( values:Expr[T]* ) = new MultiValueExpr[T](this, "in", List(values:_*))
   def notin ( values:Expr[T]* ) = new MultiValueExpr[T](this, "not in", List(values:_*))

   def in( subselect:Query[T] ) = new SubqueryExpr[T](this,"in",subselect)
   def notin( subselect:Query[T] ) = new SubqueryExpr[T](this,"not in",subselect)

   def between ( v1:Expr[T], v2:Expr[T] )( implicit ev:SqlComparable[T] ) = new BetweenExpr[T](this, v1, v2)

   def := ( rhs:Expr[T] )(implicit ev:SqlComparable[T] ) = new ComparisonExpr[T]("=", this, rhs)
   def != ( rhs:Expr[T] )(implicit ev:SqlComparable[T] ) = new ComparisonExpr[T]("!=", this, rhs)

   def +( rhs:T )(implicit ev:SqlNumeric[T] ) = new BinaryOperation[T]("+", this, rhs)
   def -( rhs:T )(implicit ev:SqlNumeric[T] ) = new BinaryOperation[T]("-", this, rhs)
   def *( rhs:T )(implicit ev:SqlNumeric[T] ) = new BinaryOperation[T]("*", this, rhs)
   def /( rhs:T )(implicit ev:SqlNumeric[T] ) = new BinaryOperation[T]("/", this, rhs)

}


case class ConcatenatedStringExpr( first:Expr[String], second:Expr[String] ) extends Expr[String]
{
   override def toSql( exp:SqlExpander ) = exp(first) + " + " + exp(second)
}


case class ConstraintConjunction( combine:String, left:Expr[Boolean], right:Expr[Boolean] ) extends Expr[Boolean]
{
   def toSql( exp:SqlExpander ) = Sql("(") + exp(left) + ") " + combine + " (" + exp(right) + ")"
}


case class ValueExpr[T]( value:T ) extends Expr[T]
{
   val NoValue = None.asInstanceOf[T]

   def toSql( exp:SqlExpander ): Sql = expand(exp, value)


   private def expand( exp:SqlExpander, value:Any ) : Sql = value match {
      case Some(x) => expand(exp, x)
      case NoValue => "null"
      case null => "null"
      case s:String => Sql("'" + s.replaceAll("'", "''") + "'")
      case _ => Sql(value.toString)
   }
}

trait LogicalExpression extends Expr[Boolean]
{
   expr : Expr[Boolean] =>

   def or( cons:Expr[Boolean] ) = new ConstraintConjunction("OR", expr, cons)
   def and( cons:Expr[Boolean] ) = new ConstraintConjunction("AND", expr, cons)
}


case class BinaryOperation[T:SqlNumeric]( op:String, lhs:Expr[T], rhs:Expr[T] ) extends Expr[T]
{
   def toSql( exp:SqlExpander ) = exp(lhs) + " " + op + " " + exp(rhs)
}

case class ColumnExpr[T]( alias:String, column:Column[T] ) extends Expr[T]
{
   def toSql( exp:SqlExpander ) = alias + "." + column.name
}

case class ComparisonExpr[T]( op:String, lhs:Expr[T], rhs:Expr[T] ) extends LogicalExpression
{
   def toSql( exp:SqlExpander ) = exp(lhs) + " " + op + " " + exp(rhs)
}

case class MultiValueExpr[T] ( expr:Expr[T],
                               op:String,
                               params:List[Expr[T]] ) extends LogicalExpression
{
   def toSql( exp:SqlExpander ) = exp(expr) + " " + op + " (" + exp.list("",params) + ")"
}

case class SubqueryExpr[T] ( expr:Expr[T],
                             op:String,
                             subquery:Query[T] ) extends LogicalExpression
{
   def toSql( exp:SqlExpander ) = exp(expr) + " " + op + " (" + exp(subquery) + ")"
}

case class BetweenExpr[T]( expr:Expr[T], min:Expr[T], max:Expr[T] ) extends LogicalExpression
{
   def toSql( exp:SqlExpander ) =  exp(expr) + " BETWEEN " + exp(min) + " AND " + exp(max)
}

case class FunctionCall[T]( name:String, params:List[Expr[_]] ) extends Expr[T]
{
   def toSql( exp:SqlExpander ) = Sql(name) + "(" + exp.list("",params) + ")"
}


/*
object Expr
{

   implicit def concatExpression[T]( e:T )( implicit conv:ExprMaker[T,String] ) = new AnyRef {
      def + ( rhs:Expr[String] ) = new ConcatenatedStringExpr(conv(e), rhs)
   }

}
   */
