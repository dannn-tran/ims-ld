package imsld.dashboard.views
import cats.syntax.all.*
import com.raquo.airstream.core.Signal
import com.raquo.airstream.status.Resolved
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.generic.auto.*
import io.circe.parser.decode
import org.scalajs.dom.{HTMLDivElement, HTMLElement}

import imsld.dashboard.HttpResponse
import imsld.dashboard.HttpResponse.{ServerError, UnexpectedResponse}
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.implicits.HeadersImplicit
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.utils.ItemDtoFlat
import imsld.dashboard.utils.ItemDtoFlat.{
  acquirePriceCurrencyInput,
  acquirePriceValueInput,
  ccyDataList
}
import imsld.model.Item

object ItemByIdView {
  private type ResponseT = HttpResponse.Ok[Throwable, Item] |
    HttpResponse.NotFound.type | HttpResponse.ServerError |
    HttpResponse.UnexpectedResponse

  private val fetchedItemV: Var[Option[Item]] = Var(None)
  private val editedItemV: Var[Option[ItemDtoFlat]] = Var(None)
  private val errorV: Var[Option[String]] = Var(None)

  def apply(pageS: Signal[ItemByIdPage]): ReactiveHtmlElement[HTMLDivElement] =
    val fetchItemS: Signal[Status[ItemByIdPage, Either[Throwable, ResponseT]]] =
      pageS.flatMapWithStatus { p => fetchItem(p.id) }

    val fetchItemPendingS: Signal[Option[String]] = fetchItemS.splitStatus(
      (_, _) => None,
      (_, _) => "Fetching data...".some
    )

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

    val fetchItemErrorS: Signal[Option[String]] = fetchItemS.map {
      case Resolved(_, Left(err), _) => s"Error: $err".some
      case Resolved(_, Right(resp: UnexpectedResponse), _) =>
        s"Unexpected response (${resp.statusCode}): ${resp.body}".some
      case Resolved(_, Right(err: ServerError), _) =>
        s"Server error (${err.statusCode})".some
      case Resolved(_, Right(HttpResponse.NotFound), _) =>
        s"Not Found (${HttpResponse.NotFound.statusCode})".some
      case Resolved(_, Right(HttpResponse.Ok(Left(err))), _) =>
        s"Failed to parse 200 response body. Error: $err".some
      case _ => None
    }

    div(
      h1(
        text <-- fetchedItemV.signal.combineWith(pageS).map { (item, page) =>
          item.flatMap(_.label) match {
            case Some(label) => label
            case None        => s"Item ${page.id}"
          }
        }
      ),
      child.maybe <-- fetchItemPendingS.splitOption { (_, signal) =>
        p(text <-- signal)
      },
      fetchItemErrorS --> errorV.writer,
      fetchItemSuccessS --> fetchedItemV.writer,
      child.maybe <-- errorV.splitOption { (_, signal) =>
        p(text <-- signal, cls := "error")
      },
      div(
        button(
          typ := "button",
          text <-- editedItemV.signal.map {
            case Some(_) => "Discard changes"
            case None    => "Edit"
          },
          onClick
            .mapTo[() => Option[Item]](fetchedItemV.now) --> editedItemV
            .updater[() => Option[Item]] {
              case (Some(_), _) => None
              case (None, item) =>
                item.apply().map { item =>
                  ItemDtoFlat(
                    slug = item.slug,
                    label = item.label,
                    acquireDate = item.acquireDate,
                    acquirePriceCurrency = item.acquirePrice.map(_.currency),
                    acquirePriceValue = item.acquirePrice.map(_.value),
                    details = item.details,
                    storageId = item.storage.map(_.id)
                  )
                }
            }
        )
      ),
      child.maybe <-- fetchedItemV.signal.splitOption { (_, signal) =>
        p(text <-- signal.map { item => s"ID: ${item.id}" })
      },
      children <-- fetchedItemV.signal
        .combineWith(editedItemV.signal)
        .splitMatchOne
        .handleCase[
          (Option[Item], Option[ItemDtoFlat]),
          Item,
          List[ReactiveHtmlElement[HTMLElement]]
        ] { case (Some(item), None) => item } { (_, signal) =>
          List(
            p(text <-- signal.map { item =>
              s"Slug: ${item.slug.getOrElse("")}"
            }),
            p(text <-- signal.map { item =>
              s"Acquire date : ${item.acquireDate.getOrElse("")}"
            }),
            p(text <-- signal.map { item =>
              s"Acquire price: ${item.acquirePrice
                  .fold("") { p => s"${p.value} ${p.currency}" }}"
            }),
            p(text <-- signal.map { item =>
              s"Acquire source: ${item.acquireSource.getOrElse("")}"
            }),
            p(text <-- signal.map { item =>
              s"Details: ${item.details.getOrElse("")}"
            }),
            p(text <-- signal.map { item =>
              s"Storage: ${item.storage.flatMap(_.label)}"
            })
          )
        }
        .handleCase[(Option[Item], Option[ItemDtoFlat]), ItemDtoFlat, List[
          ReactiveHtmlElement[HTMLElement]
        ]] { case (_, Some(item)) => item } { (_, signal) =>
          val editSlugDiv = div(
            label(forId := "slug-input", "Slug"),
            input(
              idAttr := "slug-input",
              controlled(
                value <-- signal.map(_.slug.getOrElse("")),
                onInput.mapToValue --> editedItemV.updater[String] {
                  (opt, slug) =>
                    opt.map(_.copy(slug = slug.some))
                }
              )
            )
          )
          val editAcquireDateDiv = div(
            label(forId := "acquireDate-input", "Acquire date"),
            input(
              idAttr := "acquireDate-input",
              controlled(
                value <-- signal.map(_.acquireDate.getOrElse("")),
                onInput.mapToValue --> editedItemV.updater[String] {
                  (opt, acquireDate) =>
                    opt.map(_.copy(acquireDate = acquireDate.some))
                }
              )
            )
          )
          val editAcquirePriceDiv =
            div(
              acquirePriceCurrencyInput(
                itemS = signal,
                updater = editedItemV.updater { (item, ccy) =>
                  item.map(_.copy(acquirePriceCurrency = ccy))
                }
              ),
              ccyDataList,
              acquirePriceValueInput(
                itemS = signal,
                updater = editedItemV.updater { (item, v) =>
                  item.map(_.copy(acquirePriceValue = v))
                }
              )
            )

          List(
            editSlugDiv,
            editAcquireDateDiv,
            editAcquirePriceDiv
          )
        }
        .toSignal
    )
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
          EventStream
            .fromJsPromise(resp.text())
            .map { body =>
              HttpResponse
                .UnexpectedResponse(
                  resp.headers.toMap,
                  resp.status,
                  body
                )
            }
      })
      .get(s"$BACKEND_ENDPOINT/items/$id")
      .recoverToEither
}
