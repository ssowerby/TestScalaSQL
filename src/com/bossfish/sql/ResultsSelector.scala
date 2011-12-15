package com.bossfish.sql

import com.bossfish.sql.auto.QueryArity
import java.sql.ResultSet


trait ResultsProvider[T]
{
   def extractResultRow( rs:ResultSet ) : T
   def provideResults( query:ExecutingQuery ) = new QueryResults[T](query, extractResultRow)
}


trait ResultExtractor[T]
{
   def extract( rs:ResultSet, index:Int ) : T
}


object ResultsSelector extends QueryArity
{
   implicit val intExtractor = new ResultExtractor[Int] {
      def extract(rs: ResultSet, index: Int) = rs.getInt(index)
   }

   implicit val longExtractor = new ResultExtractor[Long] {
      def extract(rs: ResultSet, index: Int) = rs.getLong(index)
   }

   implicit val floatExtractor = new ResultExtractor[Float] {
      def extract(rs: ResultSet, index: Int) = rs.getFloat(index)
   }

   implicit val doubleExtractor = new ResultExtractor[Double] {
      def extract(rs: ResultSet, index: Int) = rs.getDouble(index)
   }
   
   implicit val booleanExtractor = new ResultExtractor[Boolean] {
      def extract(rs: ResultSet, index: Int) = rs.getBoolean(index)
   }

   implicit val stringExtractor = new ResultExtractor[String] {
      def extract(rs: ResultSet, index: Int) = rs.getString(index)
   }


   // TODO - other extractors


   implicit def optionalExtractor[T]( implicit $t : SqlType[T], raw:ResultExtractor[T] ) = new ResultExtractor[Option[T]] {
      def extract(rs: ResultSet, index: Int) = {
         val res = raw.extract(rs, index)
         if (rs.wasNull) None else Some(res)
      }
   }

  
}

