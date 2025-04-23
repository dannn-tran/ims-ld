package imsld.dashboard.views

import com.raquo.laminar.api.L.*
import imsld.dashboard.BACKEND_ENDPOINT
import imsld.model.PagedResponse
import imsld.model.StoragePartial

import io.circe.parser.decode
import cats.syntax.all.*
import io.circe.generic.auto.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import org.scalajs.dom.HTMLDivElement
import imsld.model.StorageSlim

object StorageViewAllView {
  private val storagesVar
      : Var[Option[Either[Throwable, PagedResponse[StorageSlim]]]] = Var(
    None
  )

  def apply() =
    div(
      h1("All Storages"),
      FetchStream
        .get(s"$BACKEND_ENDPOINT/storages?detail=slim")
        .recoverToEither
        .map(
          _.fold(err => Left(err), decode[PagedResponse[StorageSlim]])
        ) --> storagesVar.writer
        .contramap[Either[Throwable, PagedResponse[StorageSlim]]](_.some),
      child <-- storagesVar.signal.splitMatchOne
        .handleCase { case Some(Left(err)) => err } { (_, errSignal) =>
          div(text <-- errSignal.map(_.toString()))
        }
        .handleCase[
          Option[Either[Throwable, PagedResponse[StoragePartial]]],
          PagedResponse[StoragePartial],
          ReactiveHtmlElement[HTMLDivElement]
        ] { case Some(Right(data)) => data } { (_, signal) =>
          div(
            children <-- signal.map(_.data.map { s =>
              div(
                h2(s.label),
                s.description.map { d => p(d) },
                p(s"Item count: ${s.itemCount}")
              )
            })
          )
        }
        .toSignal
    )
}
