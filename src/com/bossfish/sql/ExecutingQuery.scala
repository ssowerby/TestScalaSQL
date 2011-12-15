package com.bossfish.sql

import java.sql._

case class ExecutingQuery( stmt:Statement, results:ResultSet )
{
   def finish() {
      results.close()
      stmt.close()
   }
}



