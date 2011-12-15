package com.bossfish.sql.connectors.sqlserver

import com.bossfish.sql._


class SQLServerDialect( defaultSchema : Option[String] = None ) extends Dialect
{
   override def buildExpander() = new StandardSqlExpander(defaultSchema)

}