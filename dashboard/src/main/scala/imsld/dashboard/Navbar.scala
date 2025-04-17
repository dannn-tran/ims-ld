package imsld.dashboard

import com.raquo.laminar.api.L.*

import imsld.dashboard.pages.{HomePage, ItemAddBulkPage, ItemViewAllPage}

object Navbar:

  def apply() =
    navTag(
      idAttr := "navbar",
      div(
        a("Home", JsRouter.navigateTo(HomePage))
      ),
      div(
        cls := "navbar-navgroup",
        div("Items"),
        div(
          cls := "navbar-navgroup-items",
          a("> View all", JsRouter.navigateTo(ItemViewAllPage)),
          a("> Create bulk", JsRouter.navigateTo(ItemAddBulkPage))
        )
      )
    )
