package com.bossfish.sql.auto

import com.bossfish.sql._
import java.sql.ResultSet


trait QueryArity  {

   def select[I,E]( e1:I )( implicit s1:SelectorMaker[I,E] ) = {
      val p = new ResultsProvider[E] {
         def extractResultRow(rs: ResultSet) = s1.makeExtractor.extract(rs, 1)
      }
      new Query[E](p,List(s1.makeSelector(e1)))
   }

   def select[I1,I2,E1,E2]( e1:I1, e2:I2 )( implicit s1:SelectorMaker[I1,E1] ,
                                                     s2:SelectorMaker[I2,E2] ) = {
      val p = new ResultsProvider[(E1,E2)] {
         def extractResultRow(rs: ResultSet) = (s1.makeExtractor.extract(rs,1), s2.makeExtractor.extract(rs, 2))
      }
      new Query[(E1,E2)](p,List(s1.makeSelector(e1), s2.makeSelector(e2)))
   }

   def select[I1,I2,I3,E1,E2,E3]( e1:I1, e2:I2, e3:I3 )( implicit s1:SelectorMaker[I1,E1],
                                                                  s2:SelectorMaker[I2,E2],
                                                                  s3:SelectorMaker[I3,E3] ) = {
      val p = new ResultsProvider[(E1,E2,E3)] {
         def extractResultRow(rs: ResultSet) = (s1.makeExtractor.extract(rs,1), s2.makeExtractor.extract(rs, 2), s3.makeExtractor.extract(rs,3))
      }
      new Query[(E1,E2,E3)](p,List(s1.makeSelector(e1), s2.makeSelector(e2), s3.makeSelector(e3)))
   }
}

// TODO - should be auto-generated
