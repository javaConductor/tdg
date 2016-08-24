package com.objectdynamics.tdg.storage
import com.objectdynamics.tdg.builder.model.IDataRow
import org.mongodb.scala.MongoClient

/**
  * Created by lcollins on 8/24/2016.
  */
class MongoStorage(mongoClient: MongoClient, requestId:String, dataSetName:String) extends Storage {

  def masterCollection = mongoClient.getDatabase("tdg").getCollection("tdgMaster")


  override def getDataSetName(): String = ???

  override def getRequestId(): String = ???

  override def updateRow(i: Int, iDataRow: IDataRow): Unit = ???

  override def getRow(i: Int): IDataRow = {

    mongoClient.getDatabase("tdg").getCollection("tdgMaster")
  }

  override def getGroup(group: Group): List[IDataRow] = ???

  override def findGroups(fields: List[String]): List[Group] = ???
}
