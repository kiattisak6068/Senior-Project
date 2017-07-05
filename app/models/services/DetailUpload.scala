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

case class DetailUpload (
  id: Option[Long],
  detail: String,
  userID: String,
  lesson : String,
  time : String
)


class detailUp(tag: Tag) extends Table[DetailUpload](tag, "detailUp") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def detail = column[String]("detail")
  def userID = column[String]("userID")
  def lesson = column[String]("lesson")
  def time = column[String]("time")
  def * = (id.?, detail, userID,lesson,time) <> (DetailUpload.tupled, DetailUpload.unapply)
}

object ObjDetailUp {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val com = TableQuery[detailUp]

  def add(comment: DetailUpload): Future[String] = {

    dbConfig.db.run(com += comment).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }

  }

  def update(comment: DetailUpload): Future[String] = {
    dbConfig.db.run(com.filter(_.id === comment.id).update(comment)).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def get(userID: String , lesson : String): Future[Seq[DetailUpload]] = {
    val q = for{
      a <- com if (a.userID === userID) && (a.lesson === lesson)
    }yield a
    dbConfig.db.run(q.result)
  }

  def listAll(userID : String): Future[Seq[DetailUpload]] = {
    dbConfig.db.run(com.filter(_.userID === userID).result)
  }

  def delete(id: Long): Future[Int] = {
    dbConfig.db.run(com.filter(_.id === id).delete)
  }
}
