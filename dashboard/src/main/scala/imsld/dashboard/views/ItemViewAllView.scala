package imsld.dashboard.views

import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.HttpResponse.UnexpectedResponse
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.{HttpResponse, JsRouter}
import imsld.model.{ItemPartial, PagedResponse}
import imsld.dashboard.HttpResponse.{ServerError, Ok}
import org.scalajs.dom.HTMLElement

object ItemViewAllView:
  private type ResponseT =
    HttpResponse.Ok[Either[Throwable, PagedResponse[ItemPartial]]] |
      HttpResponse.ServerError | HttpResponse.UnexpectedResponse

  private val COLUMN_HEADERS: List[String] = List(
    "id",
    "slug",
    "label",
    "acquire date",
    "acquire price",
    "details",
    "storage",
    ""
  )

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    val fetchAllS = fetchItems().startWith(None)

    div(
      h1("All Items"),
      child <-- fetchAllS.splitMatchOne
        .handleValue(None)(p("Fetching items..."))
        .handleCase { case Left(err) => err } { (_, signal) =>
          p(text <-- signal.map { err => s"Error: $err" }, cls := "error")
        }
        .handleCase { case Right(err: ServerError) => err.statusCode } {
          (_, signal) =>
            p(text <-- signal.map { statusCode =>
              s"Server error ($statusCode)"
            })
        }
        .handleCase { case Right(resp: UnexpectedResponse) => resp } {
          (_, signal) =>
            p(
              text <-- signal.map { resp =>
                s"Unexpected response (${resp.statusCode}): ${resp.body}"
              }
            )
        }
        .handleCase { case Right(Ok(Left(err))) => err } { (_, signal) =>
          p(
            text <-- signal.map { err =>
              s"Unable to parse response body. Error: $err"
            }
          )
        }
        .handleCase[Either[Throwable, ResponseT] | None.type, PagedResponse[
          ItemPartial
        ], ReactiveHtmlElement[HTMLElement]] { case Right(Ok(Right(resp))) =>
          resp
        } { (_, signal) =>
          div(
            div(
              children <-- signal.map { resp =>
                val pageCount = ceilDiv(resp.paging.total, resp.paging.limit)
                val curPage = resp.paging.offset / resp.paging.limit + 1
                (1 until (pageCount + 1)).map { i =>
                  button(
                    typ := "button",
                    i,
                    cls("btn-pg"),
                    cls("btn-pg-cur") := i == curPage
                  )
                }
              }
            ),
            table(
              thead(
                tr(
                  COLUMN_HEADERS.map { h => th(h) }
                )
              ),
              tbody(
                children <-- signal.map(_.data.map(renderRow))
              )
            )
          )
        }
        .toSignal
    )

  private def fetchItems(): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder(HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map[ResponseT](
              decode[PagedResponse[ItemPartial]] `andThen` HttpResponse.Ok.apply
            )
        case resp => HttpResponse.mkUnexpectedResponse(resp)
      })
      .get(s"$BACKEND_ENDPOINT/items?detail=partial")
      .recoverToEither

  private def renderRow(item: ItemPartial) = tr(
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

  private def ceilDiv(a: Int, b: Int): Int = a / b + (if (a % b == 0) 0 else 1)
