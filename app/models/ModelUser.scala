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
import models.Userroles._

case class DBLoginInfo (
  id: Option[Long],
  providerID: String,
  providerKey: String
)

class LoginInfos(tag: Tag) extends Table[DBLoginInfo](tag, "logininfo") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def providerID = column[String]("providerID")
  def providerKey = column[String]("providerKey")
  def * = (id.?, providerID, providerKey) <> (DBLoginInfo.tupled, DBLoginInfo.unapply)
}

case class DBUserLoginInfo (
  userID: String,
  loginInfoId: Long
)

class UserLoginInfos(tag: Tag) extends Table[DBUserLoginInfo](tag, "userlogininfo") {
  def userID = column[String]("userID")
  def loginInfoId = column[Long]("loginInfoId")
  def * = (userID, loginInfoId) <> (DBUserLoginInfo.tupled, DBUserLoginInfo.unapply)
}

case class DBPasswordInfo (
  hasher: String,
  password: String,
  salt: Option[String],
  loginInfoId: Long
)

class PasswordInfos(tag: Tag) extends Table[DBPasswordInfo](tag, "passwordinfo") {
  def hasher = column[String]("hasher")
  def password = column[String]("password")
  def salt = column[Option[String]]("salt")
  def loginInfoId = column[Long]("loginInfoId")
  def * = (hasher, password, salt, loginInfoId) <> (DBPasswordInfo.tupled, DBPasswordInfo.unapply)
}

object MUser {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val loginInfos = TableQuery[LoginInfos]
  val userLoginInfos = TableQuery[UserLoginInfos]
  val passwordInfos = TableQuery[PasswordInfos]

  def deletepasswordInfos(userID: String): Future[Int] = {
    val getUser = for{
      x <- userLoginInfos.filter(_.userID === userID)
      c <- passwordInfos.filter(_.loginInfoId === x.loginInfoId)
    }yield c
    dbConfig.db.run(getUser.delete)
  }

  def deleteuserLoginInfos(userID: String): Future[Int] = {
    dbConfig.db.run(userLoginInfos.filter(_.userID === userID).delete)
  }

  def deleteloginInfos(userID: String): Future[Int] = {
    val get = for{
      x <- userLoginInfos.filter(_.userID === userID)
      a <- loginInfos.filter(_.id === x.loginInfoId)
    }yield a
    dbConfig.db.run(get.delete)
  }

  def find(userID: String): Future[Option[DBLoginInfo]] = {
    val getlogin = for{
      x <- userLoginInfos.filter(_.userID === userID)
      a <- loginInfos.filter(_.id === x.loginInfoId)
    }yield a
    dbConfig.db.run(getlogin.result.headOption)
  }
}
