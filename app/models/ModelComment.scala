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

case class DBComment (
  id: Option[Long],
  detail: String,
  userID: String,
  projectID: String
)

case class MComment (
  detail: String,
  userID: String,
  projectID: String
)

class Comment(tag: Tag) extends Table[DBComment](tag, "comment") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def detail = column[String]("detail")
  def userID = column[String]("userID")
  def projectID = column[String]("projectID")
  def * = (id.?, detail, userID, projectID) <> (DBComment.tupled, DBComment.unapply)
}

object ObjComment {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val com = TableQuery[Comment]

  def add(comment: MComment): Future[String] = {

    val a = DBComment(
      id = Some(0),
      detail = comment.detail,
      userID = comment.userID,
      projectID = comment.projectID
    )

    dbConfig.db.run(com += a).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }

  }

  def getUser(id: Long): Future[Option[DBComment]] = {
    dbConfig.db.run(com.filter(_.id === id).result.headOption)
  }

  def listAll: Future[Seq[DBComment]] = {
    dbConfig.db.run(com.result)
  }

  def delete(id: Long): Future[Int] = {
    dbConfig.db.run(com.filter(_.id === id).delete)
  }
}
