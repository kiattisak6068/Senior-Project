package forms

import play.api.data.Form
import play.api.data.Forms._

/**
 * The form which handles the sign up process.
 */
object Upform {

  val form = Form(
    mapping(
      "img" -> nonEmptyText
    )(Data.apply)(Data.unapply)
  )

  case class Data(
    img: String)
}
