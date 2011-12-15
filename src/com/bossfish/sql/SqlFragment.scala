package com.bossfish.sql


trait SqlFragment
{
  def toSql( expander:SqlExpander ) : Sql
}