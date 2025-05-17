package imsld.dashboard.views

import com.raquo.airstream.core.EventStream
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.{HTMLDivElement, HTMLElement, HTMLTableRowElement}

import imsld.dashboard.HttpResponse.{Ok, ServerError, UnexpectedResponse}
import imsld.model.PagedResponse

abstract class BaseViewAllView[T]:
  protected type ResponseT = Ok[Either[Throwable, PagedResponse[T]]] |
    ServerError | UnexpectedResponse

  protected def fetchAll(): EventStream[Either[Throwable, ResponseT]]
  protected val columnHeaders: List[String]
  protected def renderRow(obj: T): ReactiveHtmlElement[HTMLTableRowElement]

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    val fetchAllS = fetchAll().startWith(None)

    div(
      h1("All Items"),
      child <-- fetchAllS.splitMatchOne
        .handleValue(None)(p("Fetching items..."))
        .handleCase { case Left(err) => err } { (_, signal) =>
          p(text <-- signal.map { err => s"Error: $err" }, cls := "error")
        }
        .handleCase { case Right(err: ServerError) => err.statusCode } {
          (_, signal) =>
            p(
              text <-- signal.map { statusCode =>
                s"Server error ($statusCode)"
              },
              cls := "error"
            )
        }
        .handleCase { case Right(resp: UnexpectedResponse) => resp } {
          (_, signal) =>
            p(
              text <-- signal.map { resp =>
                s"Unexpected response (${resp.statusCode}): ${resp.body}"
              },
              cls := "error"
            )
        }
        .handleCase { case Right(Ok(Left(err))) => err } { (_, signal) =>
          p(
            text <-- signal.map { err =>
              s"Unable to parse response body. Error: $err"
            },
            cls := "error"
          )
        }
        .handleCase[
          Either[Throwable, ResponseT] | None.type,
          PagedResponse[T],
          ReactiveHtmlElement[HTMLElement]
        ] { case Right(Ok(Right(resp))) =>
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
                  columnHeaders.map { h => th(h) }
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
  private def ceilDiv(a: Int, b: Int): Int = a / b + (if (a % b == 0) 0 else 1)
