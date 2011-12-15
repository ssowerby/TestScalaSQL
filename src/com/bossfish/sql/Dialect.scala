package com.bossfish.sql

import java.sql.PreparedStatement

trait Dialect
{
   
   def buildExpander() : SqlExpander = new StandardSqlExpander(None)


   def prepareStatement( connection:Connection,
                         sql:Sql,
                         b:Binding
         ) : PreparedStatement = {
     val ps = connection.connection.prepareStatement(sql.sql)
     sql.params.zipWithIndex.map { case (param,index) =>
       val v = param match {
         case p: Parameter[_] => b(p)
         case _ => param
       }
       bindParameter(ps, index+1, v)
       v
     }
     ps
   }


   def createStatement( connection:Connection) = connection.connection.createStatement()


   def execute( connection:Connection,
                sql:Sql,
                binding:Option[Binding] = None
         ) = {
     if (sql.params.isEmpty) {
       val stmt = createStatement(connection)
       stmt.execute(sql.sql)
     }
     else {
       val ps = prepareStatement(connection, sql, binding.getOrElse(error("No binding provided for parameterised statement")))
       ps.execute()
     }
   }

   
   def executeQuery( connection:Connection,
                sql:Sql,
                binding:Option[Binding] = None
         ) : ExecutingQuery = {
     if (sql.params.isEmpty) {
        val stmt = createStatement(connection)
        ExecutingQuery(stmt,stmt.executeQuery(sql.sql))
     }
     else {
       val ps = prepareStatement(connection, sql, binding.getOrElse(error("No binding provided for parameterised query")))
       ExecutingQuery(ps,ps.executeQuery())
     }
   }

   
   protected def bindParameter( ps:java.sql.PreparedStatement, index:Int, param:Any ) {
       param match {

         case x:Int => ps.setInt(index, x)
         case x:Long => ps.setLong(index, x)
         case x:Float => ps.setFloat(index, x)
         case x:Double => ps.setDouble(index, x)
         case x:Boolean => ps.setBoolean(index, x)
         case x:String => ps.setString(index, x)
         case _ => error("Unsupported type")

       }
     }

}