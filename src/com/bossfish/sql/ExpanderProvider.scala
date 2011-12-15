package com.bossfish.sql

trait ExpanderProvider {

   def provideExpander : SqlExpander
}