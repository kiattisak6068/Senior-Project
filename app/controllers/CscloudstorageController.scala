package controllers

import javax.inject.Inject
import sys.process._
import com.mohiva.play.silhouette.api.{ Environment, LogoutEvent, Silhouette }
import com.mohiva.play.silhouette.api._
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AvatarService
import com.mohiva.play.silhouette.api.util.PasswordHasher
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import com.mohiva.play.silhouette.impl.providers._
import forms._
import models.User
import models.Userroles
import models.services.UserService
import play.api.mvc._
import play.api.i18n.MessagesApi
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import models.MUser
import models.ListUser
import models.Adviser
import models.Advisers

import java.io.File

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

    def upload = Action(parse.multipartFormData) { request =>
        request.body.file("img").map { picture =>
          val filename = picture.filename
          val contentType = picture.contentType
          picture.ref.moveTo(new File(s"public/images/$filename"))
          Ok("File uploaded")
        }.getOrElse {
          Ok("File F")
          }
  }

  def gitstatus = Action { request =>
    val r = "git status"
    val a = r.!
    Ok(f"I'm run :$a")
  }


}
