package com.bossfish {

   import sql._

   package object sql  {

      implicit object IntIsNumeric extends SqlType[Int] with SqlComparable[Int] with SqlNumeric[Int]
      implicit object LongIsNumeric extends SqlType[Long] with SqlComparable[Long] with SqlNumeric[Long]
      implicit object FloatIsNumeric extends SqlType[Float] with SqlComparable[Float] with SqlNumeric[Float]
      implicit object DoubleIsNumeric extends SqlType[Double] with SqlComparable[Double] with SqlNumeric[Double]
      implicit object StringIsComparable extends SqlType[String] with SqlComparable[String] with SqlConcat[String]
      implicit object BooleanIsType extends SqlType[Boolean]

      implicit object OptionalIntIsNumeric extends SqlType[Option[Int]] with SqlComparable[Option[Int]] with SqlNumeric[Option[Int]]
      implicit object OptionalLongIsNumeric extends SqlType[Option[Long]] with SqlComparable[Option[Long]] with SqlNumeric[Option[Long]]
      implicit object OptionalFloatIsNumeric extends SqlType[Option[Float]] with SqlComparable[Option[Float]] with SqlNumeric[Option[Float]]
      implicit object OptionalDoubleIsNumeric extends SqlType[Option[Double]] with SqlComparable[Option[Double]] with SqlNumeric[Option[Double]]
      implicit object OptionalStringIsComparable extends SqlType[Option[String]] with SqlComparable[Option[String]] with SqlConcat[Option[String]]
      implicit object OptionalBooleanIsType extends SqlType[Option[Boolean]]


      private val currentTable = new ThreadLocal[TableReference[_]]()


      private def generateAlias( col:Column[_] ) : String = {
         val ref = currentTable.get()
         val alias = if (ref == null) col.table.name else ref.alias
         currentTable.remove
         return alias
      }

      implicit def followReference[T <: Table]( ref:TableReference[T] ) : T = {
         currentTable.set(ref)
         ref.table
      }


      implicit def makeColumnExpr[T:SqlType]( col : Column[T] ) : Expr[T] = new ColumnExpr[T](generateAlias(col), col)
  
      implicit def makeValueExpr[T:SqlType]( value:T ) : Expr[T] = new ValueExpr[T](value)

      implicit def makeOptionValueExpr[T:SqlType]( value:T ) : Expr[Option[T]] = new ValueExpr[Option[T]](Some(value))

      
      implicit def valueToSelector[E]( implicit $t:SqlType[E], extractor:ResultExtractor[E] ) : SelectorMaker[E,E] = new SelectorMaker[E,E] {
         def makeSelector(i: E) = UnaliasedSelector[E](new ValueExpr[E](i))
         def makeExtractor = extractor
      }

      implicit def toSelector[C[_], E]( implicit $t:SqlType[E], extractor:ResultExtractor[E], maker:C[E]=>Expr[E] ) : SelectorMaker[C[E],E] = new SelectorMaker[C[E],E] {
         def makeSelector(i: C[E]) = UnaliasedSelector[E](maker(i))
         def makeExtractor = extractor
      }


      implicit def comparableExpression[C[_],E]( e:C[E] )( implicit maker:C[E]=>Expr[E], $evt:SqlComparable[E] ) = new AnyRef {
         def > ( rhs:Expr[E] ) = new ComparisonExpr[E](">", maker(e), rhs)
         def < ( rhs:Expr[E] ) = new ComparisonExpr[E]("<", maker(e), rhs)
         def <= ( rhs:Expr[E] ) = new ComparisonExpr[E]("<=", maker(e), rhs)
         def >= ( rhs:Expr[E] ) = new ComparisonExpr[E](">=", maker(e), rhs)
      }


      implicit def numericExpression[C[_],E]( e:C[E] )( implicit maker:C[E]=>Expr[E], $evt:SqlNumeric[E] ) = new AnyRef {
         def + ( rhs:Expr[E] ) = new BinaryOperation[E]("+", maker(e), rhs)
         def - ( rhs:Expr[E] ) = new BinaryOperation[E]("-", maker(e), rhs)
         def * ( rhs:Expr[E] ) = new BinaryOperation[E]("*", maker(e), rhs)
         def / ( rhs:Expr[E] ) = new BinaryOperation[E]("/", maker(e), rhs)
      }
      

      implicit def makeReference[T<:Table]( table:T ) = new TableReference[T](table,table.name)

      implicit def paramValueTupleToParameterBinding[T]( tuple:(Parameter[T],Any) ) = ParameterBinding[T](tuple._1, tuple._2.asInstanceOf[T])

      implicit def removeOptionFromExpr[T]( e:Expr[Option[T]] ) = e.asInstanceOf[Expr[T]]


     def parameter[T]( ctype:ColumnType[T] ) = new Parameter[T]()

     def alias[T <: Table]( table:T, alias:String ) = new TableReference[T](table, alias)

   }

}
