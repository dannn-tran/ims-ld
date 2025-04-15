package imsld.dashboard

import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLDivElement

import imsld.model.{ItemPartial, PagedResponse}

object ItemViewTable:
  private val COLUMN_HEADERS: List[String] = List(
    "id",
    "slug",
    "label",
    "acquire date",
    "acquire price",
    "acquire source"
  )

  private val itemsVar
      : Var[Option[Either[Throwable, PagedResponse[ItemPartial]]]] =
    Var(
      None
    )

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      FetchStream
        .get(s"$BACKEND_ENDPOINT/items")
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
              div(
                children <-- signal.map(_.paging.totalPages).map { n =>
                  (1 until (n + 1)).map { i => button(typ := "button", n) }
                }
              ),
              button(typ := "button", "New")
            ),
            table(
              thead(
                tr(
                  COLUMN_HEADERS.map { h => th(h) }
                )
              ),
              tbody(
                children <-- signal.map(_.data).map { items =>
                  items.map { item =>
                    tr(
                      td(item.id),
                      td(item.slug),
                      td(item.label),
                      td(item.acquireDate.map(_.toString())),
                      td(item.acquirePrice.map(_.toString())),
                      td(item.acquireSource)
                    )
                  }
                }
              )
            )
          )
        }
        .toSignal
    )
