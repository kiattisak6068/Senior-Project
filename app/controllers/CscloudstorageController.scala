package controllers

import java.util.UUID
import javax.inject.Inject
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AvatarService
import com.mohiva.play.silhouette.api.util.PasswordHasher
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import com.mohiva.play.silhouette.impl.providers._
import forms._
import models._
import models.services.UserService
import play.api.i18n.{ MessagesApi, Messages }
import play.api.libs.concurrent.Execution.Implicits._
import play.api.mvc.Action
import play.api.mvc._
import scala.concurrent.Future
import sys.process._
import java.io.File
import play.api.libs.json._
import org.apache.commons.io.FilenameUtils

/**
 * The basic application controller.
 *
 * @param messagesApi The Play messages API.
 * @param env The Silhouette environment.
 * @param socialProviderRegistry The social provider registry.
 */
 class CscloudstorageController @Inject() (
   val messagesApi: MessagesApi,
   val env: Environment[User, CookieAuthenticator],
   userService: UserService,
   authInfoRepository: AuthInfoRepository,
   avatarService: AvatarService,
   passwordHasher: PasswordHasher)
   extends Silhouette[User, CookieAuthenticator] {

    def deleteUser(id: String) = Action.async { implicit request =>
      val del = MUser.find(id)
      val login = del.map { a =>
        a.map { b =>
          val ll = LoginInfo(
            b.providerID,
            b.providerKey
          )
          val des = for {
            auth <- authInfoRepository.remove(ll)
            role <- Userroles.delete(id)
            user <- ListUser.delete(id)
          }yield (auth,role,user)

        }
      }
          login.map { res =>
            Redirect(routes.ApplicationController.getlist())
          }

      }

    def getdata(x: Option[String]) = x match {
     case Some(s) => s
     case None => ""
   }

  def gitstatus = Action { request =>
    val r = "git status"
    val a = r.!
    Ok(f"I'm run :$a")
  }

  def upload = UserAwareAction.async { implicit request =>
  request.identity match {
    case Some(user) =>
    request.body.asMultipartFormData.map {a =>
      val datatitle = a.dataParts.get("title").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val datadetail = a.dataParts.get("detail").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val datadetail2 = a.dataParts.get("detail2").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val datadetail3 = a.dataParts.get("detail3").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val datadetail4 = a.dataParts.get("detail4").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val datadetail5 = a.dataParts.get("detail5").map { a =>
         for{
           b <- a.mkString("")
          }yield b
      }
      val dataimg = a.file("img").map { a=>
        val filename = a.filename
        val extension = FilenameUtils.getExtension(filename)
        val newFileName = s"${UUID.randomUUID}.$extension"
        a.ref.moveTo(new File(s"public/images/imgCsCloud/$newFileName"))
        for{
          b <- newFileName
        }yield b
      }

      val get = for{
        t <- Advisers.getRelation(user.userID.toString)
        teaName <- ListUser.getUser(getdata(t.map{ a=> a.teaID}))
      }yield teaName
      val saveDatatoDB = get.map {data =>
        data.map {teaID =>
          val datatodb = DBDetail (
            userID = user.userID.toString,
            topic = getdata(datatitle),
            detail = getdata(datadetail),
            objective = getdata(datadetail2),
            scope = getdata(datadetail3),
            technology = getdata(datadetail4),
            benefits = getdata(datadetail5),
            img = getdata(dataimg),
            tea = getdata(teaID.fullName)
          )
          val saveData = for{
              a <- ObjDetails.add(datatodb)
          }yield a
        }
      }

      Future.successful(Redirect("/up"))

    }.getOrElse {
      Future.successful(Redirect("/up"))
    }
    case None => Future.successful(Redirect("/"))
    }
  }



  def showDetial(id: String) = Action.async { implicit request =>

        val data = for{
          dataDetail <- ObjDetails.getdata(id)
          reUser <- Advisers.getRelation(id)
          s <- ListUser.getUser(getdata(reUser.map { s => s.stuID}))
          t <- ListUser.getUser(getdata(reUser.map { t => t.teaID}))
        }yield (dataDetail,s,t)

        data.map { case (dataDetail,stu,tea) =>

            Ok(views.html.showDetial(dataDetail,stu,tea,Commentform.form))
        }

  }

  var projectID = "";
  def viewDetial(id: String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        projectID = id
        val data = for{
          dataDetail <- ObjDetails.getdata(id)
          reUser <- Advisers.getRelation(id)
          s <- ListUser.getUser(getdata(reUser.map { s => s.stuID}))
          t <- ListUser.getUser(getdata(reUser.map { t => t.teaID}))
          r <- Userroles.get(user.userID.toString)
          c <- ObjComment.listAll
          listUser <- ListUser.listAll
        }yield (r,dataDetail,s,t,c,listUser)
        data.map { case (role,dataDetail,stu,tea,comment,listUser) =>
            Ok(views.html.viewDetial(Commentform.form,user,role,dataDetail,stu,tea,comment,listUser,Commentform.form))
        }
      case None => Future.successful(Redirect("/"))
    }
  }

  def commentProject = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
      Commentform.form.bindFromRequest.fold(
        form => Future.successful(Redirect(s"/showdetail/${projectID}")),
        data => {
              val com = DBComment (
                id  = Some(0),
                detail = data.comment,
                userID = user.userID.toString,
                projectID = projectID
              )
              val save = for{
                add <- ObjComment.add(com)
              }yield add

              Future.successful(Redirect(s"/showdetail/${projectID}"))
        }
      )
      case None => Future.successful(Redirect("/"))
    }
  }

  def search = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
      Commentform.form.bindFromRequest.fold(
        form => Future.successful(Redirect("/")),
        data => {
          val dataDB = for{
            role <- Userroles.get(user.userID.toString)
            detail <- ObjDetails.search(data.comment)
          }yield (role,detail)

          dataDB.map{ case (role,detail) =>
            Ok(views.html.homecs(user,role,detail,Commentform.form))
          }

        }
      )

      case None =>
          Commentform.form.bindFromRequest.fold(
            form => Future.successful(Redirect("/")),
            data => {
              ObjDetails.search(data.comment).map {case (detail) =>
                Ok(views.html.listfile(detail,Commentform.form))
                }
              }
          )
        //Future.successful(Ok(views.html.guesthome(UserConstants.guest)))
    }
  }

  def deleteComment(id : Long,str :String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        val delete = ObjComment.delete(id)
        Future.successful(Redirect(s"/showdetail/${str}"))
      case None =>Future.successful(Redirect("/"))
    }
  }

  def editComment(id : Long,str :String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        
        Future.successful(Redirect(s"/showdetail/${str}"))
      case None =>Future.successful(Redirect("/"))
    }
  }


}
