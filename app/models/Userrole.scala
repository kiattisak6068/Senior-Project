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
import models.daos._

case class DBUserRole (
  userID: String,
  role: String
)

case class UserRole (
  userID: String,
  role: String
)

class DBUserRoles(tag: Tag) extends Table[DBUserRole](tag, "userrole") {
  def userID = column[String]("userID", O.PrimaryKey)
  def role = column[String]("role")
  override def * = (userID, role) <> (DBUserRole.tupled, DBUserRole.unapply)
}

object Userroles {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val slickUsersRole = TableQuery[DBUserRoles]

  def add(role: DBUserRole): Future[String] = {
    dbConfig.db.run(slickUsersRole += role).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def delete(userID: String): Future[Int] = {
    dbConfig.db.run(slickUsersRole.filter(_.userID === userID).delete)
  }

  def get(userID: String): Future[Option[DBUserRole]] = {
    dbConfig.db.run(slickUsersRole.filter(_.userID === userID).result.headOption)
  }

  def listAll: Future[Seq[DBUserRole]] = {
    dbConfig.db.run(slickUsersRole.result)
  }

  def update(id : String): Future[String] = {
    val db = DBUserRole(
      userID = id,
      role = "นักศึกษาโปรเจค"
    )
    dbConfig.db.run(slickUsersRole.filter(_.userID === id).update(db)).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def updateRole(id : String): Future[String] = {
    val db = DBUserRole(
      userID = id,
      role = "นักศึกษา"
    )
    dbConfig.db.run(slickUsersRole.filter(_.userID === id).update(db)).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }
}
