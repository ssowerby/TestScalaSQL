package com.bossfish.sql

import scala.collection.mutable._


class AliasedColumn[T]( val table:Table, val alias:String, val column:Column[T] ) extends SqlFragment
{
   def toSql( exp:SqlExpander ): Sql = alias + "." + column.name
}


class TableReference[T <: Table]( val table:T, alias:Option[String] = None ) extends SqlFragment
{
   def toSql( exp:SqlExpander ) = exp.optionalString("", table.schema, ".") +  table.name + exp.optionalString(" ", alias, "")

   def join( ref:Join#Ref ) = new InnerJoin(this, ref, None)
   def leftOuterJoin( ref:Join#Ref ) = new OuterJoin(this, ref, None, "left outer")
   def rightOuterJoin( ref:Join#Ref ) = new OuterJoin(this, ref, None, "right outer")
   def aliasForPrefix:String = alias.getOrElse(table.name)
}


case class Table( name:String,
                  schema : Option[String] = None )
{
   val columns = new ListBuffer[Column[_]]()
   val idColumns = new ListBuffer[Column[_]]()


   def idColumn[T]( name:String, ctype:ColumnType[T] ) : Column[T] = {
      val col = column[T](name, ctype)
      idColumns += col
      col
   }


   def column[T]( name:String, ctype:ColumnType[T] ) : Column[T] = {
      val col = new Column[T](this, name, ctype)
      columns += col
      col
   }


   def getSingleIdColumn[T]() : Column[T] = idColumns.size match {
      case 0 => error("No id column")
      case 1 => idColumns(0).asInstanceOf[Column[T]]
      case _ => error("More than one id column")
   }


   def creationSQL() : Sql = {
      "create table " + name + " (" +
            columns.map{ _.creationSQL() }.reduceLeft { _ + "," + _ } +
            ")"
   }
}


object Table
{


}
