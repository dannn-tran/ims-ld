package imsld.dashboard.views
import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.status.Resolved
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.{HTMLDivElement, HTMLElement}

import cats.data.Validated.{Valid, Invalid}
import imsld.dashboard.HttpResponse
import imsld.dashboard.HttpResponse.{ServerError, UnexpectedResponse}
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.utils.ItemDtoFlat
import imsld.model.Item
import imsld.model.StorageSlim
import imsld.model.ItemPut
import io.circe.Encoder
import io.circe.syntax.*
import com.raquo.airstream.status.Pending
import imsld.dashboard.HttpResponse.NotFound

package object ItemByIdView {
  given Encoder[ItemPut] = Encoder.derived

  private type ResponseT = HttpResponse.Ok[Either[Throwable, Item]] |
    HttpResponse.NotFound.type | HttpResponse.ServerError |
    HttpResponse.UnexpectedResponse

  def apply(
      pageS: Signal[ItemByIdPage]
  ): ReactiveHtmlElement[HTMLDivElement] =
    val fetchedItemV: Var[Option[Item]] = Var(None)

    val fetchItemS = pageS.map(_.id).flatMapWithStatus(fetchItem)
    val fetchItemStatusS: Signal[Option[(Boolean, String)]] = fetchItemS
      .map {
        case Pending(_) => (true, "Fetching data...").some
        case Resolved(_, Left(err), _) =>
          (false, s"Failed to fetch. Error: $err").some
        case Resolved(_, Right(resp: UnexpectedResponse), _) =>
          (
            false,
            s"Unexpected response (${resp.statusCode}): ${resp.body}"
          ).some
        case Resolved(_, Right(err: ServerError), _) =>
          (false, s"Server error (${err.statusCode})").some
        case Resolved(_, Right(HttpResponse.NotFound), _) =>
          (false, s"Not Found (${HttpResponse.NotFound.statusCode})").some
        case Resolved(_, Right(HttpResponse.Ok(Left(err))), _) =>
          (false, s"Failed to parse 200 response body. Error: $err").some
        case _ => None
      }
    val fetchItemSuccessS: Signal[Option[Item]] = fetchItemS
      .map {
        case Resolved(
              _,
              Right(HttpResponse.Ok(Right[Throwable, Item](item))),
              _
            ) =>
          item.some
        case _ => None
      }

    val editedItemV: Var[Option[(Int, ItemDtoFlat)]] = Var(None)
    val editedItemValidatedV
        : Var[Either[List[String], Option[(Int, ItemPut)]]] =
      Var(Right(None))

    val submitBus: EventBus[Unit] = new EventBus
    val submitS: EventStream[Status[
      (Int, ItemPut),
      Either[Throwable, ResponseT]
    ]] =
      submitBus.events
        .mapTo(editedItemValidatedV.now())
        .collect { case Right(Some(item)) =>
          item
        }
        .flatMapWithStatus { (id, dto) => submitEdit(id, dto) }

    val submitStatusS: EventStream[(Boolean, String)] =
      submitS.map {
        case Pending(_) => (true, "Submitting...")
        case Resolved(_, Right(HttpResponse.Ok(Right(item))), _) =>
          (true, "Item successfully updated.")
        case Resolved(_, Right(HttpResponse.Ok(Left(err))), _) =>
          (
            false,
            s"Item successfully updated but unable to parse server resposne. Error: $err"
          )
        case Resolved(_, Right(NotFound), _) =>
          (false, "Item not found")
        case Resolved(_, Right(err: ServerError), _) =>
          (false, s"Server error (${err.statusCode})")
        case Resolved(_, Right(resp: UnexpectedResponse), _) =>
          (false, s"Unexpected response (${resp.statusCode}): ${resp.body}")
        case Resolved(_, Left(err), _) =>
          (false, s"Failed to fetch. Error $err")
      }
    val submitSuccessS: EventStream[Item] = submitS
      .collect {
        case Resolved((id, _), Right(HttpResponse.Ok(Right(item))), _) =>
          item
      }
    val submitPending: Signal[Boolean] = submitS
      .map {
        case Pending(_) => true
        case _          => false
      }
      .startWith(false)

    div(
      fetchItemSuccessS --> fetchedItemV.writer,
      submitSuccessS.map(Option.apply) --> fetchedItemV.writer,
      submitSuccessS.mapTo(None) --> editedItemV.writer,
      editedItemV.signal.map { item =>
        item.map { (id, item) => (id, item.validate) } match {
          case None                       => Right(None)
          case Some((_, Invalid(errors))) => Left(errors.toList)
          case Some((id, Valid(item)))    => Right((id, item).some)
        }
      } --> editedItemValidatedV.writer,
      h1(
        text <-- pageS.combineWith(fetchedItemV).map { (page, item) =>
          item.flatMap(_.label) match {
            case Some(label) => label
            case None        => s"Item ${page.id}"
          }
        }
      ),
      child.maybe <-- fetchItemStatusS.splitOption { (_, signal) =>
        p(
          text <-- signal.map { (_, txt) => txt },
          cls("error") <-- signal.map { (ok, _) => !ok }
        )
      },
      child <-- submitStatusS.map { (ok, txt) =>
        p(
          txt,
          cls("error") := !ok
        )
      },
      child.maybe <-- fetchedItemV.signal
        .combineWith(editedItemV.signal)
        .splitMatchOne
        .handleCase[
          (Option[Item], Option[(Int, ItemDtoFlat)]),
          Item,
          Option[ReactiveHtmlElement[HTMLElement]]
        ] { case (Some(item), None) => item } { (_, signal) =>
          ItemDisplay(signal, editedItemV.writer).some
        }
        .handleCase[
          (Option[Item], Option[(Int, ItemDtoFlat)]),
          (Int, ItemDtoFlat),
          Option[ReactiveHtmlElement[HTMLElement]]
        ] { case (_, Some(item)) => item } { (_, signal) =>
          ItemEditForm(
            signal,
            editedItemV,
            fetchedItemV.signal,
            submitBus,
            editedItemValidatedV.signal.map(_.isLeft),
            submitPending
          ).some
        }
        .handleCase { case (_, _) => () } { (_, _) => None }
        .toSignal
    )

  private def fetchItem(id: Int): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder[ResponseT](HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 400 =>
          EventStream.fromValue(HttpResponse.NotFound)
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map(decode[Item] `andThen` HttpResponse.Ok.apply)
        case resp => HttpResponse.mkUnexpectedResponse(resp)
      })
      .get(s"$BACKEND_ENDPOINT/items/$id")
      .recoverToEither
  private def submitEdit(
      id: Int,
      dto: ItemPut
  ): EventStream[Either[Throwable, ResponseT]] =
    FetchStream
      .withDecoder[ResponseT](HttpResponse.handleServerErrorResponse orElse {
        case resp if resp.status == 400 =>
          EventStream.fromValue(HttpResponse.NotFound)
        case resp if resp.status == 200 =>
          EventStream
            .fromJsPromise(resp.text())
            .map(decode[Item] `andThen` HttpResponse.Ok.apply)
        case resp => HttpResponse.mkUnexpectedResponse(resp)
      })
      .put(
        s"$BACKEND_ENDPOINT/items/$id",
        options =>
          options.body(
            dto.asJson.noSpaces
          )
      )
      .recoverToEither
}
