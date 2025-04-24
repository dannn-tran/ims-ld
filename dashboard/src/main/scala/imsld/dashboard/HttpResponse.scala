package imsld.dashboard

import com.raquo.airstream.core.EventStream
import org.scalajs.dom

import imsld.dashboard.implicits.HeadersImplicit

abstract class HttpResponse(val statusCode: Int)

object HttpResponse:
  final case class Ok[L, R](body: Either[L, R]) extends HttpResponse(200)
  case object Created extends HttpResponse(201)
  case object Accepted extends HttpResponse(202)

  final case class MovedPermanently(url: String) extends HttpResponse(301)
  final case class PermanentlyRedirect(url: String) extends HttpResponse(308)

  final case class BadRequest(body: String) extends HttpResponse(400)
  case object Unauthorized extends HttpResponse(401) // unauthenticated
  case object Forbidden extends HttpResponse(403)
  case object NotFound extends HttpResponse(404)

  case object InternalServerError extends HttpResponse(500)
  case object NotImplemented extends HttpResponse(501)
  case object BadGateway extends HttpResponse(502)
  case object ServiceUnavailable extends HttpResponse(503)
  case object GatewayTimeout extends HttpResponse(504)
  type ServerError = InternalServerError.type | NotImplemented.type |
    BadGateway.type | ServiceUnavailable.type | GatewayTimeout.type

  final case class UnexpectedResponse(
      headers: Map[String, List[String]],
      override val statusCode: Int,
      body: String
  ) extends HttpResponse(statusCode)

  val handleServerErrorResponse: PartialFunction[dom.Response, EventStream[
    ServerError | UnexpectedResponse
  ]] = {
    case resp if resp.status >= 500 =>
      val or: ServerError | EventStream[UnexpectedResponse] = resp.status match
        case 500 => InternalServerError
        case 501 => NotImplemented
        case 502 => BadGateway
        case 503 => ServiceUnavailable
        case 504 => GatewayTimeout
        case _ =>
          EventStream
            .fromJsPromise(resp.text())
            .map(
              UnexpectedResponse(
                resp.headers.toMap,
                resp.status,
                _
              )
            )
      or match
        case err: ServerError                    => EventStream.fromValue(err)
        case es: EventStream[UnexpectedResponse] => es
  }
