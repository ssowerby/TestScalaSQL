package com.bossfish.sql.connectors.sqlserver
import com.bossfish.sql._

object SqlServerConnector
{
  implicit val connectionHandler:Connector[SqlServerConnectionDetails] = new Connector[SqlServerConnectionDetails]()
  {
    def connect(details: SqlServerConnectionDetails) : Connection = {
      println("SQLServer connection :  server=" + details.server + ", port = " + details.port + ", db = " + details.database)
      new Connection(null, new SQLServerDialect(details.defaultSchema))
    }
  }


   case class SqlServerConnectionDetails( server : String,
                                          database : String,
                                          port : Int = 1433,
                                          defaultSchema : Option[String] = None )
         extends ConnectionDetails

}

