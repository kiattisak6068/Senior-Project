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

case class DBAdviser (
  id: Option[Long],
  stuID: String,
  teaID: String
)
case class Adviser (
  stuID: String,
  teaID: String
)


class DBAdvisers(tag: Tag) extends Table[DBAdviser](tag, "adviser") {
  def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
  def stuID = column[String]("stuID")
  def teaID = column[String]("teaID")
  override def * = (id.? , stuID , teaID) <> (DBAdviser.tupled, DBAdviser.unapply)
}

object Advisers {

  val dbConfig = DatabaseConfigProvider.get[JdbcProfile](Play.current)

  val ad = TableQuery[DBAdvisers]

  def add(adviser: Adviser): Future[String] = {
    val a = DBAdviser(
      id = Some(0),
      stuID = adviser.stuID,
      teaID = adviser.teaID
    )
    deleteUser(adviser.stuID)
    dbConfig.db.run(ad += a).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def update(adviser: DBAdviser): Future[String] = {
    dbConfig.db.run(ad.filter(_.id === adviser.id).update(adviser)).map(res => "successfully").recover {
      case ex: Exception => ex.getCause.getMessage
    }
  }

  def delete(id: Long): Future[Int] = {
    dbConfig.db.run(ad.filter(_.id === id).delete)
  }

  def deleteUser(id: String): Future[Int] = {
    dbConfig.db.run(ad.filter(_.stuID === id).delete)
  }

  def get(id: Long): Future[Option[DBAdviser]] = {
    dbConfig.db.run(ad.filter(_.id === id).result.headOption)
  }

  def getRelation(stu: String): Future[Option[DBAdviser]] = {
    dbConfig.db.run(ad.filter(_.stuID === stu).result.headOption)
  }

  def listAll: Future[Seq[DBAdviser]] = {
    dbConfig.db.run(ad.result)
  }
  def listStu (tea: String): Future[Seq[DBAdviser]] = {
    dbConfig.db.run(ad.filter(_.teaID === tea).result)
  }

}
