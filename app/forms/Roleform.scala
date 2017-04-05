package forms

import play.api.data.Form
import play.api.data.Forms._

/**
 * The form which handles the sign up process.
 */
object Roleform {

  val form = Form(
    mapping(
      "role" -> nonEmptyText
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    role: String)
}
