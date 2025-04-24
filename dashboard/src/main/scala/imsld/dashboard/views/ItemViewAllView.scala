package imsld.dashboard.views

import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.HttpResponse.UnexpectedResponse
import imsld.dashboard.implicits.HeadersImplicit
import imsld.dashboard.{BACKEND_ENDPOINT, HttpResponse}
import imsld.model.{ItemPartial, PagedResponse}

object ItemViewAllView:
  private type ResponseT =
    HttpResponse.Ok[Throwable, PagedResponse[ItemPartial]] |
      HttpResponse.ServerError | HttpResponse.UnexpectedResponse

  private val COLUMN_HEADERS: List[String] = List(
    "id",
    "slug",
    "label",
    "acquire date",
    "acquire price",
    "details",
    "storage"
  )

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    val fetchItemsS = fetchItems()
    val fetchItemsSuccessS = fetchItemsS
      .map {
        case Right(HttpResponse.Ok(Right(resp))) => resp.some
        case _                                   => None
      }
      .startWith(None)
    val fetchItemErrS = fetchItemsS
      .map {
        case Left(err) => s"Error: $err".some
        case Right(err: HttpResponse.ServerError) =>
          s"Server error (${err.statusCode})".some
        case Right(resp: HttpResponse.UnexpectedResponse) =>
          s"Unexpected response (${resp.statusCode}): ${resp.body}".some
        case _ => None
      }
      .startWith(None)

    div(
      h1("All Items"),
      child.maybe <-- fetchItemErrS.splitOption { (_, signal) =>
        p(text <-- signal, cls := "error")
      },
      child.maybe <-- fetchItemsSuccessS.splitOption { (resp, _) =>
        val pageCount = ceilDiv(resp.paging.total, resp.paging.limit)
        val curPage = resp.paging.offset / resp.paging.limit + 1
        div(
          div(
            (1 until (pageCount + 1)).map { i =>
              button(
                typ := "button",
                i,
                cls("btn-pg"),
                cls("btn-pg-cur") := i == curPage
              )
            }
          ),
          table(
            thead(
              tr(
                COLUMN_HEADERS.map { h => th(h) }
              )
            ),
            tbody(
              resp.data.map(renderRow)
            )
          )
        )
      }
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
        case resp =>
          EventStream
            .fromJsPromise(resp.text())
            .map { body =>
              HttpResponse
                .UnexpectedResponse(
                  resp.headers.toMap,
                  resp.status,
                  body
                )
            }
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
    td(item.storage.map(_.label))
  )

  private def ceilDiv(a: Int, b: Int): Int = a / b + (if (a % b == 0) 0 else 1)
