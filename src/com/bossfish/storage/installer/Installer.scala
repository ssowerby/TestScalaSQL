package com.bossfish.storage
package installer

import installer.installers.DataTypeInstaller
import xml.Elem
import scala.collection._


class Installer( implicit val provider:InstallerProvider )
{
  val context = new InMemoryInstallerContext


  def installEntity[T<:Identifiable]( entity:T)
    {
      provider.provideInstaller(entity) match {
        
        case installer:UpdatableEntityInstaller[_] =>
          installer.preinstall(this, entity, context)
          installer.load(entity.id, context) match {
            case None =>
              installer.install(this, entity, context)

            case Some(existing) =>
              installer.update(entity, existing, context)
          }

        case installer:EntityInstaller[_] =>
          installer.preinstall(this, entity, context)
          installer.install(this, entity, context)

      }
    }

}




object Installer
{
  implicit val tagScannerProvider = new TagScannerProvider()
  {
    def provideScanner(tag: Elem): TagScanner = {
      tag match {
        case <data-type/> => new TypeTagScanner()
      }
    }

  }


  implicit val installerProvider = new InstallerProvider()
  {
    def provideInstaller[T]( entity:T ) = entity match {

      case _:DataType => new DataTypeInstaller().asInstanceOf[EntityInstaller[T]]

      case _ => null
    }
  }
}



trait InstallerContext
{
  def find[T]( clazz:Class[T], id:String ) : Option[T]
  def register[T]( entity:T, id:String )( implicit m:Manifest[T] )
}

trait EntityInstaller[T]
{
  def preinstall( installer:Installer, entity:T, context:InstallerContext )
  def install( installer:Installer, entity:T, context:InstallerContext )
  def uninstall( installer:Installer, entity:T )
}


trait UpdatableEntityInstaller[T] extends EntityInstaller[T]
{
  def load( id:String, context:InstallerContext ) : Option[T]
  def update( update:T, existing:T, context:InstallerContext )
}


trait TagScanner
{
  def read( element:Elem, context:InstallerContext )
}

trait TagScannerProvider
{
  def provideScanner( tag:Elem ) : TagScanner
}



trait InstallerProvider
{
  def provideInstaller[T]( entity:T ) : EntityInstaller[T]
}


class TypeTagScanner extends TagScanner
{
  def read(element: Elem, context: InstallerContext) = {
    println("Reading data-type tag")
  }
}


class InMemoryInstallerContext extends InstallerContext
{
  private val entities = mutable.Map[Class[_],mutable.Map[String,Any]]()

  def find[T]( clazz:Class[T], id: String ) = entities.get(clazz) match {
    case None =>
      // Do nothing
      None

    case Some(map) =>
      map.get(id).map( _.asInstanceOf[T] )
  }

  def register[T]( entity: T, id: String )( implicit m:Manifest[T] ) {
    entities.getOrElseUpdate(m.erasure, new mutable.HashMap[String,Any]).put(id, entity)
  }
  
}
