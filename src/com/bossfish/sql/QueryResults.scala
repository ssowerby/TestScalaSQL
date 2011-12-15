package com.bossfish.sql

import java.sql.ResultSet


case class QueryResults[R]( query:ExecutingQuery,
                            mapper:ResultSet => R) extends Iterable[R]
{
   def iterator = new Iterator[R] {

      var exhausted = false

      private def fetchNext  {
         if (!query.results.next) {
            exhausted = true
            query.finish()
         }
      }

      fetchNext

      def next() = {
         val r = if (exhausted) error("Result set exhausted") else mapper(query.results)
         fetchNext
         r
      }

      def hasNext = !exhausted

   }
}
