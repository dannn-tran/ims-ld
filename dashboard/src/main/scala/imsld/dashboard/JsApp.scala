package imsld.dashboard

import cats.syntax.all.*
import com.raquo.laminar.api.L.*
import org.scalajs.dom

import imsld.dashboard.pages.{
  ItemAddBulkPage,
  ItemViewAllPage,
  NotFoundPage,
  PageWithTitle
}
import imsld.dashboard.views.*

object JsApp {
  private val views: Signal[HtmlElement] =
    JsRouter.currentPageSignal.splitMatchOne
      .handleValue(ItemViewAllPage)(ItemViewAllView())
      .handleValue(ItemAddBulkPage)(ItemAddBulkView())
      .handleValue(NotFoundPage)(NotFoundView())
      .toSignal

  def main(args: Array[String]): Unit = {
    lazy val container = dom.document.getElementById("app")

    lazy val appElement = {
      div(
        child.maybe <-- JsRouter.currentPageSignal.splitMatchOne
          .handleCase { case p: PageWithTitle => p.title } { (title, _) =>
            h1(title).some
          }
          .handleCase { case _ => () } { (_, _) => None }
          .toSignal,
        child <-- views
      )
    }

    render(container, appElement)
  }
}
