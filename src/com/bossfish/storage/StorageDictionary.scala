package com.bossfish.storage

import scala.collection._
import com.bossfish.sql._


case object BasicType extends Enumeration
{
  type BasicType = Value
  val INTEGER = Value
  val TEXT = Value
  val FLOAT = Value
  val BOOLEAN = Value
  val UNICODE = Value
  val DATE = Value
  val TIMESTAMP = Value
}


abstract class LookupTable[T](name:String) extends Table(name) with Map[T,String]
{
   val idColumn : Column[T]
   val labelColumn : Column[String]
   val orderColumn : Option[Column[_]] = None

   def get(key: T) = Some("poo")

   def -(key: T) = null

   def +[B1 >: String](kv: (T, B1)) = null

   def iterator = null
}


trait Identifiable
{
  def id:String
}


trait DataDetail[T] extends Identifiable
{
  def id = name

  def parentSource : DataSource
  def name : String
  def column : Column[T]
  def label : String
}


case class DataAttribute[T]( parentSource : DataSource,
                             name : String,
                             column : Column[T],
                             label : String,
                             keyLookup : Option[Map[T,String]] = None) extends DataDetail[T]
{
}


case class DataLink[T]( parentSource : DataSource,
                        name : String,
                        column : Column[T],
                        label : String,
                        linkedType : DataType ) extends DataDetail[T]
{
}


case class DataChild( source : DataSource,
                      name : String,
                      childType : DataType,
                      parentColumn : String) extends Identifiable
{
  val id = name
}


trait DataSource extends Identifiable
{
  idColumnExtractor:IdColumnProvider[_] =>
  
  def id = name

  def dataType : DataType
  def name : String
  def isPrimary : Boolean
  def sourceLabel : Option[String]
  def table : Table
  def attributes : Iterable[DataAttribute[_]]
  def links : Iterable[DataLink[_]]

  def findAttribute[T]( name : String ) : Option[DataAttribute[T]]
  def findLink[T]( name : String ) : Option[DataLink[T]]
  def findDetail[T]( name : String ) : Option[DataDetail[T]] =
    findAttribute[T](name).orElse(findLink[T](name))

}


abstract class DataType( val name : String,
                         val label : String
                       ) extends Identifiable
{
    primarySourceProvider : PrimarySourceProvider =>
  
    val id = name
    private val sourcesByName = mutable.Map[String,DataSource]()


    private[storage] def registerSource( source:DataSource ) {
       sourcesByName.put(source.id, source)
    }

    protected def child( name : String,
                         childType : DataType ) = {
      
    }

    lazy val sources : Iterable[DataSource] = sourcesByName.valuesIterator.toList

}


trait StorageDictionary
{
  def getDataTypes() : Iterator[DataType]
  def findDataType( name:String ) : Option[DataType]
}


object StorageDictionary
{

  def columnTypeToBasicType( columnType:ColumnType[_] ) = columnType match {

    case VARCHAR(_) => BasicType.TEXT
    case INTEGER => BasicType.INTEGER
    case FLOAT => BasicType.FLOAT

    case _ => error("Unknown column type " + columnType)
  }

}


class SimpleStorageDictionary extends StorageDictionary
{
  private val types = new mutable.HashMap[String,DataType]


  def findDataType( name:String ) : Option[DataType] = types.get(name)

  def getDataTypes() = types.valuesIterator
}


abstract class AbstractDataSource( val dataType:DataType,
                                   tableName:String,
                                   val sourceLabel:Option[String]
        ) extends DataSource
{
  idColumnExtractor : IdColumnProvider[_] =>

  private val attributesByName = mutable.Map[String,DataAttribute[_]]()
  private val linksByName = mutable.Map[String,DataLink[_]]()

  val table = new Table(tableName)

  dataType.registerSource(this)
   
  lazy val attributes = attributesByName.valuesIterator.toList
  lazy val links = linksByName.valuesIterator.toList


  def id[T]( name:String, ctype:ColumnType[T] ) : Column[T] = table.idColumn[T](name,ctype)


  private def detailNameToColumnName( detailName:String ) = detailName.toUpperCase.replaceAll("-","_")


  def parentLink[T]( name : String,
                    label : String,
                    columnName : String,
                    parentType : DataType with WithId[T] ) : DataLink[T] = {

    val columnType : ColumnType[T] = parentType.idColumn.ctype
    val column = columnType.makeColumn(table, columnName)

    val link = new DataLink[T](this, name, column, label, parentType)
    linksByName += name -> link

    link
  }


  def attribute[T]( name : String,
                    label : String,
                    columnType : ColumnType[T],
                    columnName : String = detailNameToColumnName(name)
          ) : DataAttribute[T] = {
    val column = columnType.makeColumn(table, columnName)
    val attr = new DataAttribute[T](this, name, column, label, None)
    attributesByName += name -> attr
    attr
  }


   def classification[T]( name : String,
                          label : String,
                          columnType : ColumnType[T],
                          columnName : String = detailNameToColumnName(name)
           )( classification: =>Map[T,String] ) : DataAttribute[T] = {
     val column = columnType.makeColumn(table, columnName)
     val attr = new DataAttribute[T](this, name, column, label, Some(classification))
     attributesByName += name -> attr
     attr
   }
   

  def linkTo[T]( name : String,
                 label : String,
                 columnName : String,
                 linkedType : DataType with WithId[T]
          ) : DataLink[T] = {

    val columnType = linkedType.idColumn.ctype
    val column = columnType.makeColumn(table, columnName)
    val link = new DataLink[T](this, name, column, label, linkedType)
    linksByName += name -> link
    link
  }


  def findAttribute[T]( name:String ) = attributesByName.get(name).asInstanceOf[Option[DataAttribute[T]]]
  def findLink[T]( name:String ) = linksByName.get(name).asInstanceOf[Option[DataLink[T]]]

}


class PrimaryDataSource( override val dataType : DataType,
                         tableName : String
        ) extends AbstractDataSource(dataType,tableName,None)
{
  idColumnExtractor : IdColumnProvider[_] =>
  
  val isPrimary = true
  val name = dataType.name
}


class SecondaryDataSource( override val dataType : DataType,
                           val name : String,
                           override val sourceLabel : Option[String],
                           tableName : String
        ) extends AbstractDataSource(dataType,tableName,sourceLabel) {

  idColumnExtractor : IdColumnProvider[_] =>

  val isPrimary = false
}


trait IdColumnProvider[T]
{
  def getIdColumn() : Option[Column[T]]
}


trait WithId[T] extends IdColumnProvider[T]
{
  val idColumn : Column[T]
  def getIdColumn() = Some(idColumn)
}


trait NoId extends IdColumnProvider[Nothing]
{
  def getIdColumn() = None
}


trait PrimarySourceProvider
{
  type PrimarySource <: PrimaryDataSource
  val primarySource : PrimarySource
  lazy val table = primarySource.table
}


trait TypeWithId[T] extends DataType with WithId[T] with PrimarySourceProvider
{
  type PrimarySource = PrimaryDataSource with WithId[T]

  lazy val idColumn = primarySource.idColumn
}


trait TypeWithoutId extends DataType with PrimarySourceProvider
{
  type PrimarySource = PrimaryDataSource
}
