package com.bossfish.sql


case class Column[T]( table : Table,
                      name : String,
                      ctype : ColumnType[T] )
{
  def creationSQL() = name + " " + ctype.toSql()
}
