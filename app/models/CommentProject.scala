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

case class DBCommentProject (
  id: Option[Long],
  detail: String,
  userID: String,
  projectID: String,
  lesson : String
)


class commentPro(tag: Tag) extends Table[DBCommentProject](tag, "ComProject") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def detail = column[String]("detail")
  def userID = column[String]("userID")
  def projectID = column[String]("projectID")
  def lesson = column[String]("lesson")
  def * = (id.?, detail, userID, projectID,lesson) <> (DBCommentProject.tupled, DBCommentProject.unapply)
}

object ObjCommentProject {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val com = TableQuery[commentPro]

  def add(comment: DBCommentProject): Future[String] = {

    dbConfig.db.run(com += comment).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }

  }

  def get(id: Long): Future[Option[DBCommentProject]] = {
    dbConfig.db.run(com.filter(_.id === id).result.headOption)
  }

  def listAll: Future[Seq[DBCommentProject]] = {
    dbConfig.db.run(com.result)
  }

  def delete(id: Long): Future[Int] = {
    dbConfig.db.run(com.filter(_.id === id).delete)
  }
}
