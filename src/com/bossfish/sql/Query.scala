package com.bossfish.sql


case class Query[R] (
              resultsProvider:ResultsProvider[R],
              selectors:List[Selector[_]],
              froms:Option[SqlFragment] = None,
              constraint:Option[Expr[Boolean]] = None,
              groupBy:List[Expr[_]] = Nil,
              orderBy:List[OrderBy] = Nil,
              binding:Option[Binding] = None
         )
        extends SqlFragment {

  def from( from:SqlFragment ) : Query[R] = copy(froms=Some(from))
  
  def where( constraint:Expr[Boolean] ) : Query[R] = copy(constraint=Some(constraint))

  def where2[T]( code:scala.reflect.Code[T => Boolean] ) : Query[R] = {
   println("Code = " + code)
   this
  }

  def groupBy( groupings:Expr[_]* ) : Query[R] = copy(groupBy=List(groupings:_*))

  def orderBy( orderings:OrderBy* ) : Query[R] = copy(orderBy=List(orderings:_*))

  def bind( b:Binding ) : Query[R] = copy(binding=Some(b))
  

  def toSql( exp:SqlExpander ) : Sql =
            exp.list("select ", selectors) +
            exp.optionally(" from ", froms, "") +
            exp.optionally(" where ", constraint, "") +
            exp.list(" group by ", groupBy) +
            exp.list(" order by ", orderBy)

   
  def results( b:Option[Binding] = None )( implicit conn:Connection ) : QueryResults[R] = {
    val eq = conn.dialect.executeQuery(conn, toSql(conn.provideExpander), b)
    resultsProvider.provideResults(eq)
  }


  def results( conn:Connection, params:ParameterBinding[_]*) : QueryResults[R] = results(Some(Binding(params:_*)))(conn)
  
}
