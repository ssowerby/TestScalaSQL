package com.bossfish.dao


import java.sql._
import com.bossfish.sql._
import com.bossfish.sql.ResultsSelector._


class Results( val results:ResultSet )
{

}

trait Dao[R,T]
{
   val table : Table
//   implicit val connection : com.bossfish.sql.Connection
   
	def query() : Query[R]
   
	def setup() {
	   //table.create(source)
	}


	def build( row:R ) : T
}


class Foo( var id:Int, var value:String )
{

}


object FooDao extends Dao[(Int,String),Foo]
{
	val table = new Table("TABLE_NAME") {
	   val colId = column("TEST_ID", INTEGER)
	   val colValue = column("VALUE", VARCHAR(255))
	}

	//def findByValue( value:String ) : List[Foo] = query().where(table.colValue := value).results().iterator.toList()


   /*
	def resetValues() {
		update(table).set(colValue to ("Hello "+colId)).execute()
	}
   */
   
	//def testOrConstraints() = query().where((colId < 5) or (colId > 10)).toList()


   def query() = select(table.colId, table.colValue) from (table)

   def build( row:(Int,String) ) = new Foo(row._1, row._2)

}
