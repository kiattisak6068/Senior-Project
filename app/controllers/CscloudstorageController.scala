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
import scala.io.Source
import scala.io._
import java.io._
import java.util.Date;
import java.text.SimpleDateFormat;
import java.util.Calendar;


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
          ListUser.delete(id)
          Userroles.delete(id)
          Future.successful(Redirect("/pagelist"))
      }

      def deleteRelation(id: String) = Action.async { implicit request =>
            Advisers.deleteUser(id)
            ObjDetails.delete(id)
            val path = s"public/members/${id}/"
            val fileTemp = new File(path)
            if (fileTemp.exists) {
              fileTemp.delete()
            }
            Future.successful(Redirect("/relation"))
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
     val uuid = UUID.randomUUID

    request.body.asMultipartFormData.map {a =>
      for (
        title <- a.dataParts.get("title");
        detail <- a.dataParts.get("detail");
        objective <- a.dataParts.get("detail2");
        scope <- a.dataParts.get("detail3");
        technology <- a.dataParts.get("detail4");
        benefits <-a.dataParts.get("detail5");
        pictureFile <- a.file("img")
      ) yield {
     //Logger.warn(s"pic = ${pic}, blend = ${blend}, html = ${html}")
      val pictureExtension = reflect.io.File(pictureFile.filename).extension
      val picture = s"$uuid.$pictureExtension"

      val get = for{
        t <- Advisers.getRelation(user.userID.toString)
        teaName <- ListUser.getUser(getdata(t.map{ a=> a.teaID}))
      }yield teaName

      val saveDatatoDB = get.map {data =>
        data.map {teaID =>
          val datatodb = DBDetail (
            userID = user.userID.toString,
            topic = title(0),
            detail = detail(0),
            objective = objective(0),
            scope = scope(0),
            technology = technology(0),
            benefits = benefits(0),
            img = picture,
            tea = getdata(teaID.fullName)
          )
          ObjDetails.add(datatodb)
        }
      }
     // move files
      pictureFile.ref.moveTo(new File(s"public/members/${user.userID}/รูปภาพ/$picture"))
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
                Ok(views.html.guesthome(UserConstants.guest,detail,Commentform.form))
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

  var userid = "";
  def listfiles(id : String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
      userid = id;
      val data = for{
        role <- Userroles.get(user.userID.toString)
        com <- ObjDetailUp.listAll(user.userID.toString)
      }yield (role,com)
      data.map{ case (role,com) =>
        val r = new java.io.File(s"public/members/${id}").listFiles
        Ok(views.html.listfile(r,Commentform.form,user,role,com))
      }
      case None =>Future.successful(Redirect("/"))
    }
  }

  def uploadFile = UserAwareAction.async { implicit request =>
  request.identity match {
    case Some(user) =>
    request.body.asMultipartFormData.map {a =>
      for (
        pointer <- a.dataParts.get("pointer");
        pictureFile <- a.file("uploadBtn");
        d <- a.dataParts.get("comment")
      ) yield {
     val pictureExtension = reflect.io.File(pictureFile.filename).extension
     val picture = s"${pointer(0)}.$pictureExtension"
     val path = s"public/members/${userid}/${pointer(0)}/$picture"
     val c = ObjDetailUp.get(user.userID.toString,pointer(0))

     val fileTemp = new File(path)
     if (fileTemp.exists) {
       fileTemp.delete()
       val update = c.map{com =>
         com.map{comment =>
           val detail = DetailUpload(
             id = comment.id,
             detail = d(0),
             userID = user.userID.toString,
             lesson = pointer(0),
             time = new SimpleDateFormat("dd/MM/yyyy").format(new Date())
           )
           ObjDetailUp.update(detail)
         }
       }
     }else{
       val detail = DetailUpload(
         id = Some(0),
         detail = d(0),
         userID = user.userID.toString,
         lesson = pointer(0),
         time = new SimpleDateFormat("dd/mm/yy").format(new Date())
       )
       ObjDetailUp.add(detail)
     }
     // move files
      pictureFile.ref.moveTo(new File(path))

    }
      Future.successful(Redirect(s"/listfile/${userid}"))
    }.getOrElse {
      Future.successful(Redirect(s"/listfile/${userid}"))
    }
    case None => Future.successful(Redirect("/"))
    }
  }

  var fol = "";
  def listfilesInfolder(folder : String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>
      fol = folder;

      val data = for{
        role <- Userroles.get(user.userID.toString)
        com <- ObjDetailUp.get(user.userID.toString,folder)
        comPro <- ObjCommentProject.listAll(user.userID.toString,folder)
        tea <- ListUser.listUserTeacher
      }yield (role,com,comPro,tea)
      data.map{ case (role,com,comPro,tea) =>
        val r = new java.io.File(s"public/members/${userid}/${folder}").listFiles
        Ok(views.html.listfileInfolder(r,Commentform.form,user,role,com,comPro,tea))
      }
      case None =>Future.successful(Redirect("/"))
    }
  }

  def listfilesInfile(file : String) = UserAwareAction.async { implicit request =>
    request.identity match {
      case Some(user) =>Future.successful(Redirect("/"))
      case None =>Future.successful(Redirect("/"))
    }
  }


  def listStu = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        val data = for{
          role <- Userroles.get(user.userID.toString)
          rela <- Advisers.listStu(user.userID.toString)
          stu <- ListUser.listUserStudent
        }yield (role,rela,stu)
        data.map {case (role,relation,stu) =>
          Ok(views.html.listStu(Commentform.form,user,role,relation,stu))
        }
        case None =>Future.successful(Redirect("/"))
      }
    }

    var stuid = "";
    def listfilesInfolderStu(id : String) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        stuid = id;
        val data = for{
          role <- Userroles.get(user.userID.toString)
          stu <- ListUser.getUser(id)
          com <- ObjDetailUp.listAll(id)
        }yield (role,stu,com)
        data.map{ case (role,stu,com) =>
          val r = new java.io.File(s"public/members/${id}").listFiles
          Ok(views.html.folder(r,Commentform.form,user,role,stu,com))
        }
        case None =>Future.successful(Redirect("/"))
      }
    }

    var folStu = "";
    def listfilesInfileStu(folder : String) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        folStu = folder;
        val data = for{
          role <- Userroles.get(user.userID.toString)
          stu <- ListUser.getUser(stuid)
          com <- ObjDetailUp.get(stuid,folder)
          comPro <- ObjCommentProject.listAll(stuid,folder)
        }yield (role,stu,com,comPro)
        data.map{ case (role,stu,com,comPro) =>
          val r = new java.io.File(s"public/members/${stuid}/${folder}").listFiles
          Ok(views.html.fileStu(r,Commentform.form,user,role,stu,com,comPro))
        }
        case None =>Future.successful(Redirect("/"))
      }
    }


    def dowloadfile(fileToDownload : String) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        Future.successful(Ok.sendFile(
          content = new java.io.File(s"public/members/${userid}/${fol}/${fileToDownload}"),
          fileName = _ => s"${fileToDownload}"
        ))
        case None =>Future.successful(Redirect("/"))
      }
    }

    def dowloadfileStu(fileToDownload : String) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        Future.successful(Ok.sendFile(
          content = new java.io.File(s"public/members/${stuid}/${folStu}/${fileToDownload}"),
          fileName = _ => s"${fileToDownload}"
        ))
        case None =>Future.successful(Redirect("/"))
      }
    }

    def stuComment = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        Commentform.form.bindFromRequest.fold(
          form => Future.successful(Redirect("/")),
          data => {
                val com = DBCommentProject (
                  id  = Some(0),
                  detail = data.comment,
                  userID = user.userID.toString,
                  projectID = userid,
                  lesson = fol
                )
                ObjCommentProject.add(com)

                Future.successful(Redirect(routes.CscloudstorageController.listfilesInfolder(fol)))
          }
        )
        case None => Future.successful(Redirect("/"))
      }
    }

    def teaComment = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
        Commentform.form.bindFromRequest.fold(
          form => Future.successful(Redirect(routes.CscloudstorageController.listfilesInfileStu(folStu))),
          data => {
                val com = DBCommentProject (
                  id  = Some(0),
                  detail = data.comment,
                  userID = user.userID.toString,
                  projectID = stuid,
                  lesson = folStu
                )
                val save = for{
                  add <- ObjCommentProject.add(com)
                }yield add

                Future.successful(Redirect(routes.CscloudstorageController.listfilesInfileStu(folStu)))
          }
        )
        case None => Future.successful(Redirect("/"))
      }
    }

    def deletestuComment(id : Long) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
          ObjCommentProject.delete(id)
          Future.successful(Redirect(routes.CscloudstorageController.listfilesInfolder(fol)))
        case None => Future.successful(Redirect("/"))
      }
    }

    def deleteteaComment(id : Long) = UserAwareAction.async { implicit request =>
      request.identity match {
        case Some(user) =>
          ObjCommentProject.delete(id)
          Future.successful(Redirect(routes.CscloudstorageController.listfilesInfileStu(folStu)))
        case None => Future.successful(Redirect("/"))
      }
    }


}
