package controllers

import javax.inject.Inject

import sys.process._
import com.mohiva.play.silhouette.api.{ Environment, LogoutEvent, Silhouette }
import com.mohiva.play.silhouette.impl.authenticators.CookieAuthenticator
import com.mohiva.play.silhouette.impl.providers.SocialProviderRegistry
import forms._
import models.User
import models.ListUser
import models.DBUserRole
import models.Modellist
import models.UserConstants
import models.Userroles
import models.Advisers
import models.Adviser
import models.DBUser
import models.services.UserService
import play.api.mvc._
import play.api.i18n.MessagesApi
import scala.concurrent.Future
import models.daos.UserDAOImpl
import scala.concurrent.ExecutionContext.Implicits.global

/**
 * The basic application controller.
 *
 * @param messagesApi The Play messages API.
 * @param env The Silhouette environment.
 * @param socialProviderRegistry The social provider registry.
 */
class ApplicationController @Inject() (
  val messagesApi: MessagesApi,
  val env: Environment[User, CookieAuthenticator],
  socialProviderRegistry: SocialProviderRegistry)
  extends Silhouette[User, CookieAuthenticator] {


    def run = Action { request =>
      val r = Seq("git","init","--bare","test pond").lineStream
      Ok(f"I'm run :$r")
    }

  /**
   *  public page
   */
  def index = UserAwareAction.async { implicit request =>
  request.identity match {
    case Some(user) =>
      Userroles.get(user.userID.toString).map{ role =>
        Ok(views.html.homecs(user,role))
      }
    case None => Future.successful(Ok(views.html.guesthome(UserConstants.guest)))
  }
}


  /**
   * Handles the index action.
   *
   * @return The result to display.
   */
  // def securedPage = SecuredAction.async { implicit request =>
  //   Future.successful(Ok(views.html.homecs(request.identity)))
  // }
  /**
   * Handles the Sign In action.
   *
   * @return The result to display.
   */
  def signIn = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) => Future.successful(Redirect(routes.ApplicationController.index()))
      case None => Future.successful(Ok(views.html.signIn(SignInForm.form, socialProviderRegistry)))
    }
  }

  /**
   * Handles the Sign Up action.
   *
   * @return The result to display.
   */
  def signUp = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) => Future.successful(Redirect(routes.ApplicationController.index()))
      case None => Future.successful(Ok(views.html.signUp(SignUpForm.form)))
    }
  }


  def signUpcs = UserAwareAction.async {implicit request =>
    Future.successful(Ok(views.html.signUpcs(Roleform.form)))
  }

  def adduser = UserAwareAction.async {implicit request =>
    request.identity match {
      case Some(user) =>
        Userroles.get(user.userID.toString).map{ role =>
          Ok(views.html.adduser(user,role,Userform.form))
        }
      case None => Future.successful(Redirect(routes.ApplicationController.index()))
    }
  }

  /**
   * Handles the Sign Out action.
   *
   * @return The result to display.
   */
  def signOut = SecuredAction.async { implicit request =>
    val result = Redirect(routes.ApplicationController.index())
    env.eventBus.publish(LogoutEvent(request.identity, request, request2Messages))

    env.authenticatorService.discard(request.authenticator, result)
  }

  def getlist = UserAwareAction.async {implicit request =>
    request.identity match {
      case Some(user) =>
        val c = for{
          a <- Userroles.get(user.userID.toString)
          b <- ListUser.listUserCs
        }yield (a,b)

        c.map { case (role,users) =>
            Ok(views.html.listuser(user,users,role))
        }

      case None => Future.successful(Redirect(routes.ApplicationController.index()))
    }
  }

  def uploadfile = UserAwareAction.async { implicit request =>
    Future.successful(Ok(views.html.upload(Upform.form)))
  }

  var str = "";
  var dataID = "";
  var i = "";
  def relation = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
        val c = for{
          a <- Userroles.get(user.userID.toString)
          b <- ListUser.listUserStudent
          c <- ListUser.listUserTeacher
          d <- Advisers.listAll
        }yield (a,b,c,d)

        c.map { case (role,stu,tea,rela) =>
            Ok(views.html.relationship(str,user,stu,role,tea,rela))
        }

      case None => Future.successful(Redirect(routes.ApplicationController.index()))
    }
  }

  def dataUser(id : String) = Action {
    dataID = id
    str = "show"

    var n = for{
      b <- ListUser.getUser(dataID)
    }yield b

    for (k <- n){
      i = show(k.get.fullName)
    }

    Redirect("/relation")
  }

  def clear = Action {
    str = ""
    i = ""
    Redirect("/relation")
  }

  def addrelation(teaID : String) = Action.async { implicit request =>
      val a = Adviser(
        stuID = dataID,
        teaID = teaID
      )
      var n = for{
        c <- Advisers.add(a)
      }yield c

      str = ""
      dataID = ""
      var r = Seq("git","init","--bare",i).lineStream
      Future.successful(Redirect("/relation"))

  }

  def show(x: Option[String]) = x match {
   case Some(s) => s
   case None => ""
 }

  def gitupload = UserAwareAction.async { implicit request =>
  request.identity match {
    case Some(user) =>
      Userroles.get(user.userID.toString).map{ role =>
        Ok(views.html.gitupload(user,role))
      }
    case None => Future.successful(Ok(views.html.guesthome(UserConstants.guest)))
  }
}


}
