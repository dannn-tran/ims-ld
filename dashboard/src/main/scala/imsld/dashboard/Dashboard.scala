package imsld.dashboard

import com.raquo.laminar.api.L.{*, given}
import org.scalajs.dom
import imsld.model.ItemPartial
import io.circe.parser.decode
import io.circe.generic.auto.*
import imsld.model.PagedResponse
import cats.syntax.all.*

val BACKEND_ENDPOINT = "http://localhost:8080"

@main
def Dashboard(): Unit =
  renderOnDomContentLoaded(
    dom.document.getElementById("app"),
    Main.appElement()
  )

object Main:
  val model = new Model
  import model.*

  def appElement(): Element =
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
        .handleCase { case Some(Right(data)) => data } { (_, signal) =>
          div(text <-- signal.map(_.toString()))
        }
        .toSignal
    )

final class Model:
  val itemsVar: Var[Option[Either[Throwable, PagedResponse[ItemPartial]]]] =
    Var(
      None
    )
