package imsld.dashboard

import com.raquo.laminar.api.L.*

import imsld.dashboard.pages.{
  HomePage,
  ItemAddBulkPage,
  ItemViewAllPage,
  Page,
  StorageViewAllPage
}

object Navbar:
  private val data: List[(String, Page) | (String, List[(String, Page)])] =
    List(
      ("Home", HomePage),
      (
        "Items",
        List(("View all", ItemViewAllPage), ("Create bulk", ItemAddBulkPage))
      ),
      ("Storage locations", List(("View all", StorageViewAllPage)))
    )

  def apply() =
    navTag(
      idAttr := "navbar",
      data.map {
        case (label: String, pg: Page) =>
          div(
            a(
              label,
              onClick.preventDefault --> { _ => JsRouter.pushState(pg) }
            )
          )
        case (header: String, items: List[(String, Page)]) =>
          div(
            div(header),
            div(
              cls := "navbar-grp-items",
              items.map { (label, pg) =>
                div(
                  span("> "),
                  a(
                    label,
                    onClick.preventDefault --> { _ => JsRouter.pushState(pg) }
                  )
                )
              }
            )
          )
      }
    )
