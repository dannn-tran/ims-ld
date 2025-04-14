package imsld.dashboard

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import imsld.model.ItemPartial
import io.circe.parser.decode
import io.circe.generic.auto.*
import imsld.model.PagedResponse
import cats.syntax.all.*

val BACKEND_ENDPOINT = "http://localhost:8080"

@main
def Dashboard(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )

object Main:
  val model = new Model
  import model.*

  def appElement(): Element =
    div(
      ItemTable()
    )

final class Model
