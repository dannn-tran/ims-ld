package imsld.dashboard.views

import cats.syntax.all.*
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode

import imsld.dashboard.HttpResponse.Ok
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.{HttpResponse, JsRouter}
import imsld.model.{ItemPartial, PagedResponse}

object ItemViewAllView extends BaseViewAllView[ItemPartial]:
  protected val columnHeaders: List[String] = List(
    "id",
    "slug",
    "label",
    "acquire date",
    "acquire price",
    "details",
    "storage",
    ""
  )

  protected def fetchAll(): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder(HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map[ResponseT](
              decode[PagedResponse[ItemPartial]] `andThen` Ok.apply
            )
        case resp => HttpResponse.mkUnexpectedResponse(resp)
      })
      .get(s"$BACKEND_ENDPOINT/items?detail=partial")
      .recoverToEither

  protected def renderRow(item: ItemPartial) = tr(
    td(item.id),
    td(item.slug),
    td(item.label),
    td(item.acquireDate.map(_.toString())),
    td(item.acquirePrice.map { p =>
      s"${p.value} ${p.currency}"
    }),
    td(item.details),
    td(item.storage.map(_.label)),
    td(
      a(
        "View item",
        onClick.preventDefault --> { _ =>
          JsRouter.pushState(ItemByIdPage(item.id))
        }
      )
    )
  )
