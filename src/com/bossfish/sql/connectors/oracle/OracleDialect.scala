package com.bossfish.sql.connectors.oracle
import com.bossfish.sql._


case class OracleDialect( defaultSchema : Option[String] = None ) extends Dialect
{
   private val functionMapper = new FunctionMapper(Map("nvl" -> ("ifnull",None)))
   private val expander = new StandardSqlExpander(defaultSchema)


   override def buildExpander() = new SqlExpander()
   {
      def apply( f:SqlFragment ) : Sql = f match {

        case fc:FunctionCall[_] => functionMapper(fc).toSql(expander)

        case _ => f.toSql(expander)

      }     
   }

}