package com.bossfish.sql.auto

import com.bossfish.sql.ResultsProvider


trait FromArity {

   def from[S,R]( s:S )( builder: S=>ResultsProvider[R] ) = builder(s)
   
   def from[S1,S2,R]( s1:S1, s2:S2 )( builder: (S1,S2)=>ResultsProvider[R] ) = builder(s1,s2)

}


// TODO - should be auto-generated
