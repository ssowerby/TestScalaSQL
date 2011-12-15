package com.bossfish.sql.connectors.hsql
import com.bossfish.sql._

case class HsqlDialect( defaultSchema : Option[String] = None ) extends Dialect
{
   override def buildExpander() = new StandardSqlExpander(defaultSchema)
}
