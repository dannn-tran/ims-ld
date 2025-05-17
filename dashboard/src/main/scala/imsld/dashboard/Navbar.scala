package imsld.dashboard

import com.raquo.laminar.api.L.*

import imsld.dashboard.pages.{HomePage, ItemAddBulkPage, ItemViewAllPage}

object Navbar:

  def apply() =
    navTag(
      idAttr := "navbar",
      div(
        a(
          "Home",
          onClick.preventDefault --> { _ => JsRouter.pushState(HomePage) }
        )
      ),
      div(
        div("Items"),
        div(
          cls := "navbar-grp-items",
          div(
            span("> "),
            a(
              "View all",
              onClick.preventDefault --> { _ =>
                JsRouter.pushState(ItemViewAllPage)
              }
            )
          ),
          div(
            span("> "),
            a(
              "Create bulk",
              onClick.preventDefault --> { _ =>
                JsRouter.pushState(ItemAddBulkPage)
              }
            )
          )
        )
      )
    )
