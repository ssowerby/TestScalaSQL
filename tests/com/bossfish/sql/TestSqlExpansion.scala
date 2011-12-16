package com.bossfish.sql

import com.bossfish.sql.SqlFunctions._
import org.scalatest.matchers.{ShouldMatchers, MustMatchers}
import org.scalatest.{FunSuite, FeatureSpec, GivenWhenThen}
import collection.immutable._


class TestSqlExpansion extends FunSuite with ShouldMatchers {

   val exp = new StandardSqlExpander

   object TEST extends Table("TEST")
   {
      val C1 = column("C1", INTEGER)
      val C2 = column("C2", VARCHAR(32))
      val C3 = column("C3", VARCHAR(16) nullable)
   }

   object TEST2 extends Table("TEST2")
   {
      val C4 = column("C4", INTEGER)
      val C5 = column("C5", VARCHAR(32))
      val C6 = column("C6", VARCHAR(16) nullable)
   }
   
   import TEST._
   import ResultsSelector._



   test("unaliased tables are referenced as the table name") {
      val sel = select(C1) from (TEST)
      val sql = sel.toSql(exp)
      sql.sql should equal ("select TEST.C1 from TEST")
      sql.params should be (Nil)
   }

   test("alias applied to selectors and constraints") {
      val t1 = alias(TEST, "t1")
      val sel = select(t1.C1) from (t1) where (t1.C1 > 2)
      val sql = sel.toSql(exp)
      sql.sql should equal ("select t1.C1 from TEST t1 where t1.C1 > 2")
      sql.params should be (Nil)
   }

   test("multi value expressions are expanded") {
      val sel = select(C1) from (TEST) where (C1 in (2,3,4))
      val sql = sel.toSql(exp)
      sql.sql should equal ("select TEST.C1 from TEST where TEST.C1 in (2,3,4)")
      sql.params should be (Nil)
   }

   test("function calls are expanded") {
      val sel = select(nvl(C3,"EMPTY")) from (TEST)
      val sql = sel.toSql(exp)
      sql.sql should equal ("select nvl(TEST.C3,'EMPTY') from TEST")
      sql.params should be (Nil)
   }

   test("inner join expanded") {
      val sel = select(TEST.C1,TEST2.C5) from (TEST join TEST2 on (TEST.C1:=TEST2.C4))
      val sql = sel.toSql(exp)
      sql.sql should equal ("select TEST.C1,TEST2.C5 from TEST inner join TEST2 on (TEST.C1 = TEST2.C4)")
      sql.params should be (Nil)
   }

   test("left outer join expanded") {
      val sel = select(TEST.C1,TEST2.C5) from (TEST leftOuterJoin TEST2 on (TEST.C1:=TEST2.C4))
      val sql = sel.toSql(exp)
      sql.sql should equal ("select TEST.C1,TEST2.C5 from TEST left outer join TEST2 on (TEST.C1 = TEST2.C4)")
      sql.params should be (Nil)
   }

   test("right outer join expanded") {
      val sel = select(TEST.C1,TEST2.C5) from (TEST rightOuterJoin TEST2 on (TEST.C1:=TEST2.C4))
      val sql = sel.toSql(exp)
      sql.sql should equal ("select TEST.C1,TEST2.C5 from TEST right outer join TEST2 on (TEST.C1 = TEST2.C4)")
      sql.params should be (Nil)
   }

}
