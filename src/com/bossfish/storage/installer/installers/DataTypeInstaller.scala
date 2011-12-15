package com.bossfish.storage
package installer
package installers

class DataTypeInstaller extends UpdatableEntityInstaller[DataType]
{
  def preinstall(installer: Installer, entity: DataType, context: InstallerContext) = {

    //installer.installEntity(
  }


  def install( installer:Installer, entity:DataType, context: InstallerContext) = {
    println("DataType install : " + entity)
  }

  def uninstall( installer:Installer, entity:DataType ) = {

  }


  def update( update: DataType, existing: DataType, context: InstallerContext) {

  }


  def load(id: String, context: InstallerContext) = {
    println("Attempting to load data type : " + id)
    None
  }
}
