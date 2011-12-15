package com.bossfish.sql

trait SqlExpander extends Function1[SqlFragment,Sql] with ExpanderProvider
{
  def list[F <: SqlFragment]( prefix: =>String, list:List[F] ) : Sql =
    if (list.isEmpty) "" else prefix +: list.map(apply(_)).reduceLeft{ (f1,f2) => f1 + "," + f2 }
 
  def optionally[F <: SqlFragment]( prefix: =>String,
                                    opt:Option[F],
                                    suffix: =>String
                                  ) : Sql  =
    if (opt.isDefined) prefix +: (apply(opt.get) + suffix) else ""


  def optionalString( prefix : =>String,
                      option : Option[String],
                      suffix : =>String
                    ) : Sql =
    if (option.isDefined) prefix + option.get + suffix else ""


   def provideExpander = this
}


class StandardSqlExpander( schema : Option[String] = None
                         ) extends SqlExpander
{
  def apply( f:SqlFragment ) : Sql = f match {

    case t:TableReference[_] if (schema.isDefined && !t.table.schema.isDefined) =>
        Sql(schema.get + ".") + t.toSql(this)

    case _ => f.toSql(this)
  }
  
}