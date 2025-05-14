package imsld.dashboard.views

import cats.data.Validated.Valid
import cats.data.{Validated, ValidatedNec}
import cats.syntax.all.*
import com.raquo.airstream.status.{Pending, Resolved}
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.Encoder
import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.utils.ItemDtoFlat
import imsld.model.{
  InsertedRowWithId,
  ItemPut,
  MonetaryAmount,
  PagedResponse,
  StorageSlim
}
import imsld.dashboard.constants.CCY_OPTIONS
import imsld.dashboard.utils.BigDecimalFormatter
import scala.util.Try
import imsld.dashboard.utils.FetchStorages
import cats.data.Validated.Invalid
import imsld.dashboard.HttpResponse

object ItemAddBulkView:
  given Encoder[ItemPut] = Encoder.derived

  private val COLUMN_HEADERS: List[String] = List(
    "slug",
    "label",
    "publish date",
    "acquire date",
    "acquire price",
    "acquire source",
    "details",
    "storage",
    ""
  )
  private val clearDataBus: EventBus[Unit] = new EventBus
  private val submitBus: EventBus[Unit] = new EventBus

  private val storagesFetchStream = FetchStorages.stream
  private val storagesS: Signal[List[StorageSlim]] = storagesFetchStream
    .collect { case Right(resp) => resp.data }
    .startWith(List.empty)

  private val itemsV: Var[List[ItemDtoFlat]] = Var(List(ItemDtoFlat()))
  private val itemsValidatedV
      : Var[ValidatedNec[(Int, List[String]), List[ItemPut]]] =
    Var(Valid(List.empty))

  private type ResponseT =
    HttpResponse.Ok[Either[Throwable, List[InsertedRowWithId]]] |
      HttpResponse.ServerError | HttpResponse.UnexpectedResponse
  private val respStream: EventStream[
    Status[List[ItemPut], Either[Throwable, ResponseT]]
  ] = submitBus.events
    .mapTo(itemsValidatedV.now())
    .collect { case Valid(dto) => dto }
    .flatMapWithStatus { dto =>
      FetchStream
        .withDecoder(HttpResponse.handleServerErrorResponse orElse {
          case resp if resp.status == 201 =>
            EventStream
              .fromJsPromise(resp.text())
              .map[ResponseT](
                decode[List[InsertedRowWithId]] `andThen` HttpResponse.Ok.apply
              )
          case resp => HttpResponse.mkUnexpectedResponse(resp)
        })
        .post(
          s"$BACKEND_ENDPOINT/items",
          options =>
            options.body(
              dto.asJson.noSpaces
            )
        )
        .recoverToEither
    }
  private val respStatusS: Signal[Option[(Boolean, String)]] =
    (respStream.map {
      case Pending(_) => (true, "Submitting...").some
      case Resolved(_, Right(HttpResponse.Ok(Right(lst))), _) =>
        (true, s"New items created successfully. IDs: ${lst.map(_.id)}").some
      case Resolved(_, Right(HttpResponse.Ok(Left(err))), _) =>
        (false, s"Fail to parse 200 response. Error: $err").some
      case Resolved(_, Right(err: HttpResponse.ServerError), _) =>
        (false, s"Server error: $err").some
      case Resolved(_, Right(err: HttpResponse.UnexpectedResponse), _) =>
        (false, s"Unexpected response: $err").some
      case Resolved(_, Left(err), _) =>
        (false, s"Failed to create new items. Error: $err").some
    } mergeWith clearDataBus.events.mapTo(None)).startWith(None)
  private val inflightSubmitSignal: Signal[Boolean] = respStream
    .collect {
      case Pending(_) => true
      case _          => false
    }
    .startWith(false)
  private val inflightSubmitOrResolvedRightSignal: Signal[Boolean] = respStream
    .collect {
      case Pending(_)               => true
      case Resolved(_, Right(_), _) => true
      case _                        => false
    }
    .startWith(false)

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      clearDataBus.events.mapTo(List.empty) --> itemsV.writer,
      itemsV.signal.map { items =>
        items.zipWithIndex
          .foldLeft[ValidatedNec[(Int, List[String]), List[ItemPut]]](
            Valid(List.empty)
          ) { case (acc, (item, i)) =>
            (
              acc,
              item.validate.leftMap { errors =>
                (i, errors.toList)
              }.toValidatedNec
            ) mapN { (lst, validItem) => lst.appended(validItem) }
          }
      } --> itemsValidatedV.writer,
      h1("Bulk item creation"),
      child.maybe <-- storagesFetchStream.splitEither(
        (_, signal) =>
          p(
            text <-- signal.map { err =>
              s"Fail to retrieve storages. Error: $err"
            },
            cls := "error"
          ).some,
        (_, _) => None
      ),
      children <-- itemsValidatedV.signal
        .map {
          case Valid(a)   => List.empty
          case Invalid(e) => e.toList
        }
        .split { (idx, _) => idx } { (idx, _, signal) =>
          p(text <-- signal.map { (_, errors) =>
            s"Invalid values for item $idx. Reasons: ${errors.mkString("; ")}"
          })
        },
      controlPanel,
      inputForm
    )

  private val controlPanel: ReactiveHtmlElement[HTMLDivElement] =
    div(
      button(
        typ := "button",
        onClick --> itemsV.updater { (prev, _) =>
          prev.appended(ItemDtoFlat())
        },
        "Add",
        disabled <-- inflightSubmitOrResolvedRightSignal
      ),
      button(
        typ := "submit",
        "Submit",
        onClick.preventDefault.mapToUnit --> submitBus.writer,
        disabled <-- (itemsValidatedV.signal.map(_.isInvalid)
          combineWith inflightSubmitOrResolvedRightSignal).map {
          case (false, false) => false
          case _              => true
        }
      ),
      child.maybe <-- respStatusS.splitOption { (_, signal) =>
        p(
          text <-- signal.map { (_, txt) => txt },
          cls("error") <-- signal.map { (ok, _) => !ok }
        )
      },
      button(
        typ := "button",
        "Clear",
        onClick.mapToUnit --> clearDataBus.writer,
        disabled <-- inflightSubmitSignal
      )
    )

  private val inputForm =
    form(
      cls := "input-form",
      table(
        thead(
          tr(
            COLUMN_HEADERS.map { h => th(h) }
          )
        ),
        tbody(
          children <-- itemsV.signal.splitByIndex { (idx, _, signal) =>
            TableBodyRow(idx, signal)
          }
        )
      )
    )

  private def TableBodyRow(idx: Int, signal: Signal[ItemDtoFlat]) =
    val lockAndItemSignal =
      inflightSubmitOrResolvedRightSignal.combineWith(signal)

    def mkTextInputCell(
        accessor: ItemDtoFlat => String,
        updater: Observer[String]
    ) = td(
      child.maybe <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
        _ => None,
        _ =>
          input(
            controlled(
              value <-- signal.map(accessor),
              onInput.mapToValue --> updater
            )
          ).some
      ),
      text <-- lockAndItemSignal.map {
        case (true, item) => accessor(item)
        case _            => ""
      }
    )

    def mkTextAreaCell(
        accessor: ItemDtoFlat => String,
        updater: Observer[String]
    ) = td(
      child.maybe <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
        _ => None,
        _ =>
          textArea(
            controlled(
              value <-- signal.map(accessor),
              onInput.mapToValue --> updater
            )
          ).some
      ),
      text <-- lockAndItemSignal.map {
        case (true, item) => accessor(item)
        case _            => ""
      }
    )

    val slugCell = mkTextInputCell(
      accessor = _.slug.getOrElse(""),
      updater = itemsV.updater { (lst, slug) =>
        lst.updated(idx, lst(idx).copy(slug = slug.some))
      }
    )

    val labelCell = mkTextAreaCell(
      accessor = _.label.getOrElse(""),
      updater = itemsV.updater { (lst, str) =>
        val processedStr = str.filter { c =>
          !"\r\n".contains(c)
        }
        lst.updated(
          idx,
          lst(idx)
            .copy(label = processedStr.some)
        )
      }
    )

    val publishDateCell = mkTextInputCell(
      accessor = _.publishDate.getOrElse(""),
      updater = itemsV.updater { (lst, str) =>
        lst.updated(
          idx,
          lst(idx).copy(publishDate = str.some)
        )
      }
    )

    val acquireDateCell = mkTextInputCell(
      accessor = _.acquireDate.getOrElse(""),
      updater = itemsV
        .updater[String] { (lst, str) =>
          lst.updated(
            idx,
            lst(idx)
              .copy(acquireDate = str.some)
          )
        }
    )

    def acquirePriceCell =
      td(
        children <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
          _ => List.empty,
          _ =>
            val lastGoodPriceValue: Var[String] = Var("")
            List(
              input(
                cls := "acquirePriceCurrency-input",
                placeholder := "ccy",
                controlled(
                  value <-- signal.map(_.acquirePriceCurrency.getOrElse("")),
                  onInput.mapToValue --> itemsV
                    .updater[String] { (lst, ccy) =>
                      lst.updated(
                        idx,
                        lst(idx).copy(acquirePriceCurrency = ccy.some)
                      )
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
                  if (str.isEmpty()) Right(None)
                  else
                    Try(BigDecimal(str.filter(_ != ','))).fold(
                      _ => Left(s"Invalid decimal: $str"),
                      bd => Right(bd.some)
                    )
                } --> itemsV.updater[Either[String, Option[BigDecimal]]] {
                  (lst, v) =>
                    lst.updated(
                      idx,
                      lst(idx).copy(acquirePriceValue = v)
                    )
                },
                signal.map(_.acquirePriceValue) --> lastGoodPriceValue
                  .updater[Either[String, Option[BigDecimal]]] { (prev, opt) =>
                    opt match {
                      case Right(Some(bd)) => BigDecimalFormatter.format(bd)
                      case _               => prev
                    }
                  },
                size := 16
              )
            )
        ),
        text <-- lockAndItemSignal.map {
          case (true, item) =>
            List(
              item.acquirePriceCurrency.getOrElse(""),
              item.acquirePriceValue.toString()
            ).filter(!_.isBlank()).mkString(" ")
          case _ => ""
        }
      )

    val acquireSourceCell = mkTextAreaCell(
      accessor = _.acquireSource.getOrElse(""),
      updater = itemsV
        .updater { (lst, str) =>
          lst.updated(
            idx,
            lst(idx).copy(acquireSource = if (str.isEmpty()) None else str.some)
          )
        }
    )

    val detailsCell = mkTextAreaCell(
      accessor = _.details.getOrElse(""),
      updater = itemsV
        .updater { (lst, str) =>
          lst.updated(
            idx,
            lst(idx).copy(details = if (str.isEmpty()) None else str.some)
          )
        }
    )

    val storageCell = td(
      text <-- lockAndItemSignal.withCurrentValueOf(storagesS).map {
        case (true, item, storages) =>
          item.storageId match {
            case Right(Some(id)) =>
              storages.find(_.id == id).flatMap(_.label).getOrElse("")
            case _ => ""
          }
        case _ => ""
      },
      child.maybe <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
        _ => None,
        _ =>
          select(
            option(value := "", "Choose storage location..."),
            children <-- storagesS.map { lst =>
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
            } --> itemsV.updater[Either[String, Option[Int]]] {
              (lst, storageId) =>
                lst.updated(idx, lst(idx).copy(storageId = storageId))
            }
          ).some
      )
    )

    val removeRowCell = td(
      button(
        typ := "button",
        onClick --> itemsV.updater { (lst, _) =>
          lst.zipWithIndex.collect { case (item, i) if i != idx => item }
        },
        "Remove",
        disabled <-- inflightSubmitOrResolvedRightSignal
      )
    )

    tr(
      slugCell,
      labelCell,
      publishDateCell,
      acquireDateCell,
      acquirePriceCell,
      acquireSourceCell,
      detailsCell,
      storageCell,
      removeRowCell
    )
