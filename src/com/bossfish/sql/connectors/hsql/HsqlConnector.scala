package com.bossfish.sql.connectors.hsql

import java.sql.DriverManager
import com.bossfish.sql._


object HsqlConnector
{
  DriverManager.registerDriver(new org.hsqldb.jdbcDriver)

   
  implicit val hsqldbMemHandler:Connector[HsqlMemoryConnectionDetails] = new Connector[HsqlMemoryConnectionDetails]()
  {
    def connect(details: HsqlMemoryConnectionDetails) : Connection = {
      val url = "jdbc:hsqldb:mem:" + details.name
      val conn = DriverManager.getConnection(url)
      println("HSQLDB connection : " + url)
      new Connection(conn, new HsqlDialect(details.defaultSchema))
    }

  }


   case class HsqlMemoryConnectionDetails( name:String,
                                           defaultSchema:Option[String] = None ) extends ConnectionDetails
}


