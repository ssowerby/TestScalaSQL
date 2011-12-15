package com.bossfish.storage

import com.bossfish.sql.{INTEGER, VARCHAR}

object StorageDictionarySchema
{
 case object DataType extends DataType("data-type", "Data type") with TypeWithId[String] {
    val primarySource = new PrimaryDataSource(DataType, "SI_SD_DATA_TYPE") with WithId[String]{
      val idColumn = id("TYPE_ID", VARCHAR(32))
      val label = attribute("label", "Presentation label", VARCHAR(255))

    }
    child("data-sources", DataSource)
  }

  case object DataSource extends DataType("data-source", "Data source") with TypeWithId[String] {
    val primarySource = new PrimaryDataSource(DataSource, "SI_SD_DATA_SOURCE") with WithId[String] {
      val parent = parentLink("parent", "Parent type", "PARENT_TYPE", DataType)
      val idColumn = id("SOURCE_ID", VARCHAR(32))
      val label = attribute("label", "Presentation label", VARCHAR(100))
      val metaType = attribute("meta-type", "Meta type", VARCHAR(255))
      // FIXME - classification
    }
  }  

}