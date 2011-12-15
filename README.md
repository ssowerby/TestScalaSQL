Test Scala SQL DSL.
Do not use - this is just play code.
Use ScalaQuery or Squeryl instead.

This is an attempt to explore a SQL library which provides the following:

   * As close to real SQL as possible
   * Not an ORM, just a convenient way to build and execute SQL
   * Type-safe selectors and constraints
   * Ability to include the same table more than once
   * Database-specific SQL generation including function name resolving and parameter reordering)
   * Bind parameter support
   * Provision of nullable columns via Option
   * Table model which can be extended to include metadata

