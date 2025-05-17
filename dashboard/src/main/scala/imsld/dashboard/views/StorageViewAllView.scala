package imsld.dashboard.views

import cats.syntax.all.*
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.HTMLTableRowElement

import imsld.dashboard.HttpResponse
import imsld.dashboard.HttpResponse.Ok
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.model.{PagedResponse, StoragePartial}

object StorageViewAllView extends BaseViewAllView[StoragePartial]:
  override protected val columnHeaders: List[String] = List(
    "id",
    "slug",
    "label",
    "item count",
    "description"
  )

  override protected def renderRow(
      obj: StoragePartial
  ): ReactiveHtmlElement[HTMLTableRowElement] = tr(
    td(obj.id),
    td(obj.slug),
    td(obj.label),
    td(obj.itemCount),
    td(obj.description)
  )

  protected def fetchAll(): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder(HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map[ResponseT](
              decode[PagedResponse[StoragePartial]] `andThen` Ok.apply
            )
        case resp => HttpResponse.mkUnexpectedResponse(resp)
      })
      .get(s"$BACKEND_ENDPOINT/storages?detail=partial")
      .recoverToEither
