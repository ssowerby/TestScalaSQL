package com.bossfish.sql


class FunctionMapper( val functions:Map[String,(String,Option[List[Int]])] ) extends Function1[FunctionCall[_],FunctionCall[_]]
{

  def apply( fc:FunctionCall[_] ) : FunctionCall[_] = functions.get(fc.name) match {

    case None => fc

    case Some((mappedName:String,None)) => FunctionCall(mappedName,fc.params)

    case Some((mappedName:String,Some(paramIndices))) =>
       val mappedParams = paramIndices.map { index => fc.params(index-1) }
       FunctionCall(mappedName, mappedParams)
  
  }
  
}