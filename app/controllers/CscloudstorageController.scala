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
import org.apache.commons.io.FilenameUtils;

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

  //   def upload = Action { request =>
  //   request.body.asMultipartFormData.map {a =>
  //     val datatitle = a.dataParts.get("title").map { a =>
  //        for{
  //          b <- a.mkString("")
  //         }yield b
  //     }
  //
  //     val datadetail = a.dataParts.get("detail").map { a =>
  //        for{
  //          b <- a.mkString("")
  //         }yield b
  //     }
  //
  //     val dataimg = a.file("img").map { a=>
  //       val filename = a.filename
  //       a.ref.moveTo(new File(s"public/images/imgCsCloud/$filename"))
  //       for{
  //         b <- a.filename
  //       }yield b
  //     }
  //
  //     val title = getdata(datatitle)
  //     val detail = getdata(datadetail)
  //     val img = getdata(dataimg)
  //
  //     Redirect("/up")
  //   }.getOrElse {
  //     Redirect("/up")
  //   }
  // }
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

      val dataimg = a.file("img").map { a=>
        val filename = a.filename
        val extension = FilenameUtils.getExtension(filename)
        val newFileName = s"${UUID.randomUUID}.$extension"
        a.ref.moveTo(new File(s"public/images/imgCsCloud/$newFileName"))
        for{
          b <- newFileName
        }yield b
      }

      val title = getdata(datatitle)
      val detail = getdata(datadetail)
      val img = getdata(dataimg)

      val datatodb = DBDetail (
        userID = user.userID.toString,
        topic = title,
        detail = detail,
        img = img
      )

      val saveData = for{
          a <- ObjDetails.add(datatodb)
      }yield a

      Future.successful(Ok(""+dataimg))
    }.getOrElse {
      Future.successful(Redirect("/up"))
    }

    case None => Future.successful(Redirect("/"))
  }
}

}
