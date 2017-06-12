package models

import java.util.UUID
import play.api.Play
import play.api.db.slick.DatabaseConfigProvider
import scala.concurrent.Future
import slick.driver.JdbcProfile
import slick.driver.H2Driver.api._
//import slick.driver.MySQLDriver.api._
import scala.concurrent.ExecutionContext.Implicits.global
import slick.lifted.ProvenShape.proveShapeOf

case class DBDetail (
  userID: String,
  topic: String,
  detail: String,
  img: String
)

class Detail(tag: Tag) extends Table[DBDetail](tag, "detail") {
  def userID = column[String]("userID", O.PrimaryKey)
  def topic = column[String]("topic")
  def detail = column[String]("detail")
  def img = column[String]("img")
  def * = (userID, topic, detail, img) <> (DBDetail.tupled, DBDetail.unapply)
}

object ObjDetails {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val detail = TableQuery[Detail]

  def add(de: DBDetail): Future[String] = {
    dbConfig.db.run(detail += de).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }

  }

  def getdata(userID: String): Future[Option[DBDetail]] = {
    dbConfig.db.run(detail.filter(_.userID === userID).result.headOption)
  }

  def listAll: Future[Seq[DBDetail]] = {
    dbConfig.db.run(detail.result)
  }

  def delete(userID: String): Future[Int] = {
    dbConfig.db.run(detail.filter(_.userID === userID).delete)
  }
}
