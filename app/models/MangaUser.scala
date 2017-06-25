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
import models.Userroles._


case class DBUser (
  userID: String,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  avatarURL: Option[String]
)

case class Modellist (
  userID: String,
  firstName: Option[String],
  lastName: Option[String],
  fullName: Option[String],
  email: Option[String],
  avatarURL: Option[String],
  role: String
)

class Users(tag: Tag) extends Table[DBUser](tag, "user") {
  def id = column[String]("userID", O.PrimaryKey)
  def firstName = column[Option[String]]("firstName")
  def lastName = column[Option[String]]("lastName")
  def fullName = column[Option[String]]("fullName")
  def email = column[Option[String]]("email")
  def avatarURL = column[Option[String]]("avatarURL")
  def * = (id, firstName, lastName, fullName, email, avatarURL) <> (DBUser.tupled, DBUser.unapply)
}

object ListUser {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val users = TableQuery[Users]

  def getUser(userID: String): Future[Option[DBUser]] = {
    dbConfig.db.run(users.filter(_.id === userID).result.headOption)
  }

  def listAll: Future[Seq[DBUser]] = {
    dbConfig.db.run(users.result)
  }

  def delete(userID: String): Future[Int] = {
    dbConfig.db.run(users.filter(_.id === userID).delete)
  }

def listUserCs: Future[Seq[Modellist]] = {
  var a = DBUser
  val models = for{
    a <- users
    b <- slickUsersRole
    if a.id === b.userID
  }yield (a,b)
  dbConfig.db.run(models.result).map{ userseq =>
    userseq.map{
      case (user,role) =>
        Modellist(
          user.userID,
          user.firstName,
          user.lastName,
          user.fullName,
          user.email,
          user.avatarURL,
          role.role
        )
    }
  }
}

def listUserStudent: Future[Seq[Modellist]] = {
  var a = DBUser
  val models = for{
    a <- users
    b <- slickUsersRole
    if a.id === b.userID && b.role === "นักศึกษา" || b.role === "นักศึกษาโปรเจค"
  }yield (a,b)
  dbConfig.db.run(models.result).map{ userseq =>
    userseq.map{
      case (user,role) =>
        Modellist(
          user.userID,
          user.firstName,
          user.lastName,
          user.fullName,
          user.email,
          user.avatarURL,
          role.role
        )
    }
  }
}
def listUserTeacher: Future[Seq[Modellist]] = {
  var a = DBUser
  val models = for{
    a <- users
    b <- slickUsersRole
    if a.id === b.userID && b.role === "อาจารย์"
  }yield (a,b)
  dbConfig.db.run(models.result).map{ userseq =>
    userseq.map{
      case (user,role) =>
        Modellist(
          user.userID,
          user.firstName,
          user.lastName,
          user.fullName,
          user.email,
          user.avatarURL,
          role.role
        )
    }
  }
}


}
