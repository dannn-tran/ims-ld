package imsld.dashboard

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import imsld.dashboard.pages.{
  HomePage,
  ItemAddBulkPage,
  ItemByIdPage,
  ItemViewAllPage,
  NotFoundPage
}
import imsld.dashboard.views.*
import imsld.dashboard.pages.StorageViewAllPage

object JsApp {
  private val views: Signal[HtmlElement] =
    JsRouter.currentPageSignal.splitMatchOne
      .handleValue(HomePage)(HomeView())
      .handleValue(NotFoundPage)(NotFoundView())
      .handleValue(ItemViewAllPage)(ItemViewAllView())
      .handleType[ItemByIdPage] { (_, signal) => ItemByIdView(signal) }
      .handleValue(ItemAddBulkPage)(ItemAddBulkView())
      .handleValue(StorageViewAllPage)(StorageViewAllView())
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
