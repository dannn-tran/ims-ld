package imsld.dashboard

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import imsld.dashboard.pages.{
  HomePage,
  ItemAddBulkPage,
  ItemViewAllPage,
  NotFoundPage
}
import imsld.dashboard.views.*

object JsApp {
  private val views: Signal[HtmlElement] =
    JsRouter.currentPageSignal.splitMatchOne
      .handleValue(HomePage)(HomeView())
      .handleValue(ItemViewAllPage)(ItemViewAllView())
      .handleValue(ItemAddBulkPage)(ItemAddBulkView())
      .handleValue(NotFoundPage)(NotFoundView())
      .toSignal

  def main(args: Array[String]): Unit = {
    lazy val container = dom.document.getElementById("body")

    lazy val appElement = {
      div(
        Navbar(),
        child <-- views,
        idAttr := "app"
      )
    }

    render(container, appElement)
  }
}
