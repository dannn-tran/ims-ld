package imsld.dashboard

import imsld.model.PagedResponse
import imsld.model.ItemPartial
import com.raquo.airstream.core.Signal
import com.raquo.airstream.core.Observer
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLDivElement
import com.raquo.laminar.api.L.{*, given}
import io.circe.parser.decode
import io.circe.generic.auto.*
import cats.syntax.all.*

object ItemTable:
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
      FetchStream.get(s"$BACKEND_ENDPOINT/items").recoverToEither --> itemsVar
        .updater[Either[Throwable, String]] { (_, resp) =>
          resp match
            case Left(err)   => Left(err).some
            case Right(resp) => decode[PagedResponse[ItemPartial]](resp).some
        },
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
              children <-- signal.map(_.paging.totalPages).map { n =>
                (1 until n).map { i => button(typ := "button", n) }
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
