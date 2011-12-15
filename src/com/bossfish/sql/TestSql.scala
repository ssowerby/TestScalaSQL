package com.bossfish.sql

import com.bossfish.sql.SqlFunctions._
import com.bossfish.storage._


object FOOBAR extends Table("FOOBAR") {
   val ID = column("ID", INTEGER)
   val VALUE = column("VALUE", VARCHAR(255) nullable)
   val FLOAT_VALUE = column("FLOAT_VALUE", FLOAT)
}


object TestSchema
{
   object Test extends Table("TEST") {

   }

   case object Type1 extends DataType("type1", "Test") with TypeWithId[Int] {
      val primarySource = new PrimaryDataSource(Type1, "A_TABLE") with WithId[Int]{
         val idColumn = id("TEST_ID", INTEGER)
      }
   }

   case object Type2 extends DataType("type2", "Something") with TypeWithId[Int] {
      val primarySource = new PrimaryDataSource(Type2, "SOME_TABLE") with WithId[Int] {
         val parent = parentLink("parent", "Parent thing", "PARENT_X", Type1)
         val idColumn = id("TEST_ID", INTEGER)
         val label = attribute("label", "Presentation label", VARCHAR(100))
         val link = linkTo("link", "A link", "A_LINK", Type1)
      }
      val source2 = new SecondaryDataSource(Type2, "source2", Some("Source2"), "SOME_SECONDARY_TABLE") with WithId[Int] {
         val idColumn = id("TEST_ID", INTEGER)
         val anotherVal = attribute("another-val", "Another value", FLOAT)
         val classVal = classification("class-val", "Classification value", INTEGER) {
            new LookupTable[Int]("SOME_LOOKUP") {
               val idColumn = column("SOME_ID", INTEGER)
               val labelColumn = column("SOME_LABEL", VARCHAR(255))
            }
         }
      }
   }
}


object TestSql {

   import FOOBAR._


   def main( args:Array[String] ) {

      import connectors.hsql._
      import TestSchema._
      import ResultsSelector._


      implicit val conn = Connection.connect(HsqlConnector.HsqlMemoryConnectionDetails("test"))

      /*
      val query = select(FOOBAR.ID as("xyz")) from FOOBAR where (FOOBAR.VALUE < "foo")

       println("SQL = " + conn.provideExpander.apply(query))

      val source = source(conn)

      source.ensureCreated(FOOBAR)

      source.execute(query).eachResult { r =>
        println("ID = " + r(FOOBAR.ID) + ", value=" + r(FOOBAR.VALUE))
      }

      */

      val ddl = FOOBAR.creationSQL()
      println("ddl = " + ddl)
      conn.execute(FOOBAR.creationSQL())
      
      val t2 = alias(FOOBAR, "foo")

      val query2 = select(t2.ID) from t2 where ((t2.ID+5 > t2.ID-3) and (t2.VALUE := "foo'bar"))
      println("query2 = " + conn.provideExpander.apply(query2))

      val query3 = select(FOOBAR.ID) from FOOBAR where ((FOOBAR.ID := 5) or ((FOOBAR.VALUE < "foo") and (FOOBAR.ID between (10,15))))
      println("query3 = " + conn.provideExpander.apply(query3))

      val query4 = select(FOOBAR.ID+2,3) from (FOOBAR join t2 on (FOOBAR.ID > t2.ID)) where (FOOBAR.ID in (select(FOOBAR.ID) from FOOBAR where (FOOBAR.VALUE>"foo")))
      println("query4 = " + conn.provideExpander.apply(query4))

      val query5 = select(FOOBAR.ID,FOOBAR.VALUE) from (FOOBAR join t2 on (FOOBAR.ID := t2.ID)) where ((FOOBAR.ID := 2) or (FOOBAR.ID := 3))
      println("query5 = " + conn.provideExpander.apply(query5))

      val P1 = parameter(INTEGER)
      val P2 = parameter(FLOAT)

      val query6 = select(FOOBAR.ID,FOOBAR.VALUE) from (FOOBAR leftOuterJoin t2 on (FOOBAR.ID := t2.ID)) where ((FOOBAR.ID := P1) or (FOOBAR.FLOAT_VALUE > P2))
      println("query6 = " + conn.provideExpander.apply(query6))

      query6.results(Some(Binding(P1->2,P2->3.4F)))

      val b = new Binding()
      b(P1) = 17
      b(P2) = 3.4F
      query6.results(Some(b))

   /*
      val query7= select(count(ID)) from FOOBAR
      query7.results()

      val query8 = select(nvl(ID,3)) from FOOBAR
      query8.results()
    */

      conn.execute("insert into foobar (id,value,float_value) values (1, 'foo', 2.1)")
      conn.execute("insert into foobar (id,value,float_value) values (2, 'bar', 3.2)")
      conn.execute("insert into foobar (id,value,float_value) values (3, 'baz', 4.5)")

      val minv = parameter(INTEGER)
      val maxv = parameter(INTEGER)


      val t = select(1) from (FOOBAR) where (ID >= 2)

      import FOOBAR._
      
     val qsel = select(count(VALUE),sum(FLOAT_VALUE)) from(FOOBAR) where (ID >= minv and ID <= maxv ) /* groupBy(ID) orderBy(ID) */

     for ( (x,y) <- qsel.results(conn, minv->1,maxv->2)) {
       println(x + "," + y)
     }


      val qsel2 = select(ID,VALUE,FLOAT_VALUE) from(FOOBAR)

      for ( (x,y,z) <- qsel2.results()) {
         println(x + " , " + y + " , " + z)
      }


      val qsel3 = select(Type2.idColumn,Type2.primarySource.link.column) from (Type2.table)

      println("SQL 3 = " + conn.provideExpander.apply(qsel3))



      val qsel4 = select(ID,VALUE,FLOAT_VALUE) from(FOOBAR) where2( (_:Int) == 2 )
      println("SQL 4 = " + conn.provideExpander.apply(qsel4))

      /*
import com.bossfish.storage.installer.Installer._
import StorageDictionarySchema._

val installer = new Installer()
installer.installEntity(DataType)
installer.installEntity(DataSource)
installer.installEntity(Type1)
      */


   }

}