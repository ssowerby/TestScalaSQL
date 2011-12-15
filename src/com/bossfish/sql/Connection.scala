package com.bossfish.sql

trait ConnectionDetails


trait Connector[T <: ConnectionDetails]
{
  def connect( details:T ) : Connection
}


class Connection( val connection:java.sql.Connection,
                  val dialect:Dialect ) extends ExpanderProvider
{

  def provideExpander = dialect.buildExpander

  def execute( sql:Sql, binding:Option[Binding] = None ) = dialect.execute(this, sql, binding)
  def prepare( sql:Sql, binding:Binding ) = dialect.prepareStatement(this, sql, binding)
}


object Connection
{
  def connect[T <: ConnectionDetails]( details:T )( implicit handler:Connector[T] ) = handler.connect(details)

}

