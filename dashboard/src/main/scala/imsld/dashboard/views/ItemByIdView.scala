package imsld.dashboard.views
import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.HttpResponse.{ServerError, UnexpectedResponse}
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.{BACKEND_ENDPOINT, HttpResponse}
import imsld.model.Item

object ItemByIdView {
  private type ResponseT = HttpResponse.Ok[Throwable, Item] |
    HttpResponse.NotFound.type | HttpResponse.ServerError |
    HttpResponse.UnexpectedResponse

  private def fetchItem(id: Int): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder[ResponseT](HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 400 =>
          EventStream.fromValue[ResponseT](HttpResponse.NotFound)
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map[ResponseT](decode[Item] `andThen` HttpResponse.Ok.apply)
        case resp =>
          EventStream.fromValue[ResponseT](
            HttpResponse.UnexpectedResponse(resp)
          )
      })
      .get(s"$BACKEND_ENDPOINT/$id")
      .recoverToEither

  def apply(pageS: Signal[ItemByIdPage]): ReactiveHtmlElement[HTMLDivElement] =
    val fetchItemStream = pageS.flatMapSwitch { p => fetchItem(p.id) }
    val itemS: Signal[Option[Item]] = fetchItemStream
      .collect[Option[Item]] {
        case Right(HttpResponse.Ok(Right[Throwable, Item](item))) =>
          item.some
        case _ => None
      }
      .startWith(None)
    val errS: Signal[Option[String]] = fetchItemStream
      .collect {
        case Right(HttpResponse.Ok(Right(_))) => None
        case Right(HttpResponse.Ok(Left(err))) =>
          s"Failed to parse 200 response body. Error: $err".some
        case Right(HttpResponse.NotFound) =>
          s"Not Found (${HttpResponse.NotFound.statusCode})".some
        case Right(resp: ServerError) =>
          s"Server error (${resp.statusCode})".some
        case Right(resp: UnexpectedResponse) =>
          s"Unexpected response: $resp".some
        case Left(err) => s"Error: $err".some
      }
      .startWith(None)

    div(
      h1(text <-- itemS.map(_.flatMap(_.label)).combineWith(pageS).map {
        case (None, p)        => s"Item ${p.id}"
        case (Some(label), _) => label
      }),
      child.maybe <-- errS.splitOption { (_, signal) =>
        p(text <-- signal, cls := "error")
      },
      children <-- itemS.splitOption(
        (_, signal) =>
          List(
            p(text <-- signal.map { item => s"ID: ${item.id}" }),
            p(text <-- signal.map { item =>
              s"Slug: ${item.slug.getOrElse("")}"
            })
          ),
        List.empty
      )
    )
}
