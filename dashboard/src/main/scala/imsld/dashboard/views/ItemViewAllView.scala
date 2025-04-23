package imsld.dashboard.views

import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.BACKEND_ENDPOINT
import imsld.model.{ItemPartial, PagedResponse}

object ItemViewAllView:
  private val COLUMN_HEADERS: List[String] = List(
    "id",
    "slug",
    "label",
    "acquire date",
    "acquire price",
    "details",
    "storage"
  )

  private val itemsVar
      : Var[Option[Either[Throwable, PagedResponse[ItemPartial]]]] =
    Var(
      None
    )

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      h1("All Items"),
      FetchStream
        .get(s"$BACKEND_ENDPOINT/items?detail=partial")
        .recoverToEither
        .map(
          _.fold(err => Left(err), decode[PagedResponse[ItemPartial]])
        ) --> itemsVar.writer
        .contramap[Either[Throwable, PagedResponse[ItemPartial]]](_.some),
      child <-- itemsVar.signal.splitMatchOne
        .handleCase { case Some(Left(err)) => err } { (_, errSignal) =>
          div(text <-- errSignal.map(_.toString()))
        }
        .handleCase[
          Option[Either[Throwable, PagedResponse[ItemPartial]]],
          PagedResponse[ItemPartial],
          ReactiveHtmlElement[HTMLDivElement]
        ] { case Some(Right(data)) =>
          data
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
                children <-- signal.map(_.data).map { items =>
                  items.map(renderRow)
                }
              )
            )
          )
        }
        .toSignal
    )
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
