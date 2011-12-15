package com.bossfish.sql

sealed trait ColumnType[T]
{
    self:ColumnType[T] =>
  
    val maximumSize : Option[Int]

    def makeColumn( table:Table, name:String ) = new Column[T](table, name, self)

    val sqlType:String

    protected def options = "not null"

    def toSql() = sqlType + maximumSize.map( (size) => "(" + size + ")" ).getOrElse("")  + " " + options

    def nullable = new Nullable[T](this)
}


trait SqlType[T]
trait SqlComparable[T] extends SqlType[T]
trait SqlNumeric[T] extends SqlType[T]
trait SqlConcat[T] extends SqlType[T]


case class VARCHAR( size:Int ) extends ColumnType[String]
{
    val sqlType = "varchar"
    val maximumSize = Some(size)
}

case object INTEGER extends ColumnType[Int]
{
    val sqlType = "integer"
    val maximumSize = None
}

case object FLOAT extends ColumnType[Float]
{
   val sqlType = "float"
   val maximumSize = None
}

case class Nullable[T]( ctype:ColumnType[T] ) extends ColumnType[Option[T]]
{
   val sqlType = ctype.sqlType
   val maximumSize = ctype.maximumSize
   override def options = ""
}
