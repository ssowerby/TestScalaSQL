package com.bossfish.sql


case class Sql( sql:String, params:List[Any] )
{
    def + ( g2:Sql) = Sql(sql+g2.sql, params++g2.params)
    def +: ( g2:Sql) = Sql(g2.sql+sql, g2.params++params)
}

object Sql
{
   def apply( sql:String ) = new Sql(sql,Nil)
  
   implicit def tupleToGeneratedSql( tuple:(String,List[Any]) ) : Sql = Sql(tuple._1, tuple._2)
   implicit def stringToSql( sql:String ) : Sql = Sql(sql,Nil)

   val EMPTY = Sql("", Nil)

}