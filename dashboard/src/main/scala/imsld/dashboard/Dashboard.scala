package imsld.dashboard

import com.raquo.laminar.api.L.*
import org.scalajs.dom

val BACKEND_ENDPOINT = "http://localhost:8080"

@main
def Dashboard(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )

object Main:

  def appElement(): Element =
    div(
      // ItemViewTable(),
      ItemAddTable()
    )
