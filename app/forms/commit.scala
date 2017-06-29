package forms

import play.api.data.Form
import play.api.data.Forms._

/**
 * The form which handles the sign up process.
 */
object Commentform {

  val form = Form(
    mapping(
      "comment" -> text
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    comment : String)
}
