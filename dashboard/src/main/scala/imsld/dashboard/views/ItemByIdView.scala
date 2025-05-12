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
import imsld.dashboard.implicits.HeadersImplicit
import imsld.dashboard.pages.ItemByIdPage
import imsld.dashboard.utils.ItemDtoFlat
import imsld.model.Item
import imsld.dashboard.constants.CCY_OPTIONS
import scala.util.Try
import imsld.dashboard.utils.BigDecimalFormatter
import imsld.model.StorageSlim
import imsld.dashboard.utils.FetchStorages
import imsld.model.ItemNew
import io.circe.Encoder
import io.circe.syntax.*

object ItemByIdView {
  given Encoder[ItemNew] = Encoder.derived

  private type ResponseT = HttpResponse.Ok[Throwable, Item] |
    HttpResponse.NotFound.type | HttpResponse.ServerError |
    HttpResponse.UnexpectedResponse

  private val fetchedItemV: Var[Option[Item]] = Var(None)
  private val editedItemV: Var[Option[(Int, ItemDtoFlat)]] = Var(None)
  private val editedItemValidatedV
      : Var[Either[List[String], Option[(Int, ItemNew)]]] =
    Var(Right(None))

  private val submitBus: EventBus[Unit] = new EventBus
  private val respS =
    submitBus.events
      .mapTo(editedItemValidatedV.now())
      .collect { case Right(Some(item)) =>
        item
      }
      .flatMapWithStatus { (id, dto) =>
        FetchStream.put(
          s"$BACKEND_ENDPOINT/items/$id",
          options =>
            options.body(
              dto.asJson.noSpaces
            )
        )
      }

  def apply(pageS: Signal[ItemByIdPage]): ReactiveHtmlElement[HTMLDivElement] =
    val fetchItemS: Signal[Status[ItemByIdPage, Either[Throwable, ResponseT]]] =
      pageS.flatMapWithStatus { p => fetchItem(p.id) }
    val fetchItemStatusS: Signal[Option[(Boolean, String)]] = fetchItemS.map {
      case Pending(_) => (true, "Fetching data...").some
      case Resolved(_, Left(err), _) =>
        (false, s"Failed to fetch. Error: $err").some
      case Resolved(_, Right(resp: UnexpectedResponse), _) =>
        (false, s"Unexpected response (${resp.statusCode}): ${resp.body}").some
      case Resolved(_, Right(err: ServerError), _) =>
        (false, s"Server error (${err.statusCode})").some
      case Resolved(_, Right(HttpResponse.NotFound), _) =>
        (false, s"Not Found (${HttpResponse.NotFound.statusCode})").some
      case Resolved(_, Right(HttpResponse.Ok(Left(err))), _) =>
        (false, s"Failed to parse 200 response body. Error: $err").some
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
      editedItemV.signal.map { item =>
        item.map { (id, item) => (id, item.validate) } match {
          case None                       => Right(None)
          case Some((_, Invalid(errors))) => Left(errors.toList)
          case Some((id, Valid(item)))    => Right((id, item).some)
        }
      } --> editedItemValidatedV.writer,
      child.maybe <-- fetchItemStatusS.splitOption { (_, signal) =>
        p(
          text <-- signal.map { (_, txt) => txt },
          cls("error") <-- signal.map { (ok, _) => !ok }
        )
      },
      fetchItemS
        .map {
          case Resolved(
                _,
                Right(HttpResponse.Ok(Right[Throwable, Item](item))),
                _
              ) =>
            item.some
          case _ => None
        } --> fetchedItemV.writer,
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
                  (
                    item.id,
                    ItemDtoFlat(
                      slug = item.slug,
                      label = item.label,
                      acquireDate = item.acquireDate,
                      acquirePriceCurrency = item.acquirePrice.map(_.currency),
                      acquirePriceValue =
                        item.acquirePrice.map(_.value).asRight,
                      details = item.details,
                      storageId = item.storage.map(_.id).asRight
                    )
                  )
                }
            }
        ),
        button(
          typ := "submit",
          onClick.preventDefault.mapToUnit --> submitBus.writer,
          disabled <-- editedItemValidatedV.signal.map(_.isLeft)
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
          val editSlugDiv =
            val inputId = "slug-input"
            div(
              label(forId := inputId, "Slug"),
              input(
                idAttr := inputId,
                controlled(
                  value <-- signal.map(_.slug.getOrElse("")),
                  onInput.mapToValue --> editedItemV.updater[String] {
                    (opt, slug) =>
                      opt.map { (id, item) =>
                        (id, item.copy(slug = slug.some))
                      }
                  }
                )
              )
            )
          val editAcquireDateDiv =
            val inputId = "acquireDate-input"
            div(
              label(forId := inputId, "Acquire date"),
              input(
                idAttr := inputId,
                controlled(
                  value <-- signal.map(_.acquireDate.getOrElse("")),
                  onInput.mapToValue --> editedItemV.updater[String] {
                    (opt, acquireDate) =>
                      opt.map { (id, item) =>
                        (id, item.copy(acquireDate = acquireDate.some))
                      }
                  }
                )
              )
            )
          val editAcquirePriceDiv =
            val lastGoodPriceValue: Var[String] = Var("")
            div(
              input(
                cls := "acquirePriceCurrency-input",
                placeholder := "ccy",
                controlled(
                  value <-- signal.map(_.acquirePriceCurrency.getOrElse("")),
                  onInput.mapToValue.map(Option.apply) --> editedItemV
                    .updater[Option[String]] { (item, ccy) =>
                      item.map { (id, item) =>
                        (id, item.copy(acquirePriceCurrency = ccy))
                      }
                    }
                ),
                listId := "defaultCurrencies",
                size := 8
              ),
              dataList(
                idAttr := "defaultCurrencies",
                CCY_OPTIONS.map { ccy =>
                  option(value := ccy)
                }
              ),
              input(
                placeholder := "value",
                controlled(
                  value <-- lastGoodPriceValue.signal,
                  onInput.mapToValue --> lastGoodPriceValue.writer
                ),
                lastGoodPriceValue.signal.map { str =>
                  if (str.isBlank()) Right(None)
                  else
                    Try(BigDecimal(str.filter(_ != ','))).fold(
                      _ => Left(s"Invalid decimal $str"),
                      bd => Right(bd.some)
                    )
                } --> editedItemV.updater[Either[String, Option[BigDecimal]]] {
                  (item, v) =>
                    item.map { (id, item) =>
                      (id, item.copy(acquirePriceValue = v))
                    }
                },
                signal.map(_.acquirePriceValue) --> lastGoodPriceValue
                  .updater[Either[String, Option[BigDecimal]]] { (prev, v) =>
                    v match {
                      case Right(Some(bd)) => BigDecimalFormatter.format(bd)
                      case _               => prev
                    }
                  },
                size := 16,
                cls("error") <-- signal.map(_.acquirePriceValue.isLeft)
              )
            )
          val editAcquireSourceDiv =
            val textAreaId = "acquireSource-textARea"
            div(
              label(forId := textAreaId, "Acquire source"),
              textArea(
                idAttr := textAreaId,
                controlled(
                  value <-- signal.map(_.acquireSource.getOrElse("")),
                  onInput.mapToValue --> editedItemV.updater[String] {
                    (opt, acquireSource) =>
                      opt.map { (id, item) =>
                        (id, item.copy(acquireSource = acquireSource.some))
                      }
                  }
                )
              )
            )
          val editDetailsDiv =
            val textAreaId = "details-textArea"
            div(
              label(forId := textAreaId, "Details"),
              textArea(
                idAttr := textAreaId,
                controlled(
                  value <-- signal.map(_.details.getOrElse("")),
                  onInput.mapToValue --> editedItemV.updater[String] {
                    (opt, details) =>
                      opt.map { (id, item) =>
                        (id, item.copy(details = details.some))
                      }
                  }
                )
              )
            )
          val storageIdDiv =
            val storagesFetchStream = FetchStorages.stream
            val storageIdInputBus: EventBus[Either[String, Option[Int]]] =
              new EventBus

            val selectId = "storage-select"

            div(
              label(forId := selectId, "Storage"),
              select(
                idAttr := selectId,
                option(value := "", "Choose storage location..."),
                children <-- storagesFetchStream
                  .collect { case Right(resp) =>
                    resp.data
                  }
                  .map { lst =>
                    lst.map { s => option(value := s.id.toString(), s.label) }
                  },
                value <-- signal.map(_.storageId match {
                  case Right(Some(id)) => id.toString()
                  case _               => ""
                }),
                onInput.mapToValue.map[Either[String, Option[Int]]] { x =>
                  if (x.isEmpty()) Right(None)
                  else
                    x.toIntOption match
                      case None        => Left("Invalid storage_id: " + x)
                      case Some(value) => Right(value.some)
                } --> editedItemV.updater[Either[String, Option[Int]]] {
                  (opt, storageId) =>
                    opt.map { (id, item) =>
                      (id, item.copy(storageId = storageId))
                    }
                }
              ),
              child.maybe <-- storagesFetchStream
                .collect {
                  case Left(err) =>
                    s"Unable to fetch storages from server. Error: $err".some
                  case _ => None
                }
                .splitOption { (_, signal) =>
                  p(text <-- signal, cls := "error")
                },
              child.maybe <-- storageIdInputBus.events
                .collect {
                  case Left(err) =>
                    s"Error encountered while updating storage_id: $err".some
                  case _ => None
                }
                .splitOption { (_, signal) =>
                  p(text <-- signal, cls := "error")
                }
            )

          List(
            editSlugDiv,
            editAcquireDateDiv,
            editAcquirePriceDiv,
            editAcquireSourceDiv,
            editDetailsDiv,
            storageIdDiv
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
