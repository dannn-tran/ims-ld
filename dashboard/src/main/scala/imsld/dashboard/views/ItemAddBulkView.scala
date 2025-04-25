package imsld.dashboard.views

import cats.data.Validated.{Invalid, Valid}
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

import imsld.dashboard.LogLevel
import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.dashboard.utils.ItemDtoFlat
import imsld.dashboard.utils.ItemDtoFlat.{
  acquirePriceCurrencyInput,
  acquirePriceValueInput,
  ccyDataList
}
import imsld.model.{
  InsertedRowWithId,
  ItemNew,
  MonetaryAmount,
  PagedResponse,
  StorageSlim
}

object ItemAddBulkView:
  given Encoder[ItemNew] = Encoder.derived

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

  private val itemsVar: Var[List[ItemDtoFlat]] = Var(List(ItemDtoFlat()))
  private val storagesFetchStream
      : EventStream[Either[Throwable, PagedResponse[StorageSlim]]] =
    FetchStream
      .get(s"$BACKEND_ENDPOINT/storages")
      .recoverToEither
      .map(
        _.fold(
          err => Left(err),
          decode[PagedResponse[StorageSlim]]
        )
      )
  private val storagesVar: Var[List[StorageSlim]] = Var(List.empty)

  private val submitBus: EventBus[ValidatedNec[String, List[ItemNew]]] =
    new EventBus
  private val respStream: EventStream[
    Status[List[ItemNew], Either[Throwable, List[InsertedRowWithId]]]
  ] = submitBus.events
    .collect { case Valid(dto) => dto }
    .flatMapWithStatus { dto =>
      FetchStream
        .post(
          s"$BACKEND_ENDPOINT/items",
          options =>
            options.body(
              dto.asJson.noSpaces
            )
        )
        .recoverToEither
        .map(_.fold(err => Left(err), decode[List[InsertedRowWithId]]))
    }
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
  private val logVar: Var[Option[(LogLevel, String)]] = Var(None)

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      h1("Bulk item creation"),
      storagesFetchStream.collect { case Right(resp) =>
        resp.data
      } --> storagesVar.writer,
      storagesFetchStream.collect {
        case Right(_)  => None
        case Left(err) => (LogLevel.Error, err.toString()).some
      } --> logVar.writer,
      controlPanel,
      child.maybe <-- logVar.signal.splitOption { (_, signal) =>
        div(
          text <-- signal.map { (_, str) => str },
          cls("error") <-- signal.map {
            case (LogLevel.Error, _) => true
            case _                   => false
          }
        )
      },
      inputForm
    )

  private val controlPanel: ReactiveHtmlElement[HTMLDivElement] =
    div(
      button(
        typ := "button",
        onClick --> itemsVar.updater { (prev, _) =>
          prev.appended(ItemDtoFlat())
        },
        "Add",
        disabled <-- inflightSubmitOrResolvedRightSignal
      ),
      button(
        typ := "submit",
        "Submit",
        onClick.preventDefault.mapTo(
          itemsVar.now().traverse(_.validate)
        ) --> submitBus.writer,
        submitBus.events.collect {
          case Invalid(err) =>
            (LogLevel.Error, err.toList.mkString("\n")).some
          case Valid(_) => None
        } --> logVar.writer,
        disabled <-- inflightSubmitOrResolvedRightSignal
      ),
      respStream.collect {
        case Pending(_) => (LogLevel.Info, "Submitting...")
        case Resolved(_, Right(resp), _) =>
          (
            LogLevel.Info,
            "New items successfully created. IDs: " + resp
              .map(_.id)
              .mkString(", ")
          )
        case Resolved(_, Left(err), _) =>
          (
            LogLevel.Error,
            "Failed to create new items. Error: " + err.toString()
          )
      } --> logVar.writer.contramap[(LogLevel, String)](_.some),
      button(
        typ := "button",
        "Clear",
        onClick.mapTo(List.empty) --> itemsVar.writer,
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
          children <-- itemsVar.signal.splitByIndex { (idx, _, signal) =>
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
      updater = itemsVar.updater { (lst, slug) =>
        lst.updated(idx, lst(idx).copy(slug = slug.some))
      }
    )

    val labelCell = mkTextAreaCell(
      accessor = _.label.getOrElse(""),
      updater = itemsVar.updater { (lst, str) =>
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
      updater = itemsVar.updater { (lst, str) =>
        lst.updated(
          idx,
          lst(idx).copy(publishDate = str.some)
        )
      }
    )

    val acquireDateCell = mkTextInputCell(
      accessor = _.acquireDate.getOrElse(""),
      updater = itemsVar
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
            List(
              acquirePriceCurrencyInput(
                itemS = signal,
                updater = itemsVar
                  .updater { (lst, ccy) =>
                    lst.updated(
                      idx,
                      lst(idx).copy(acquirePriceCurrency = ccy)
                    )
                  }
              ),
              ccyDataList,
              acquirePriceValueInput(
                itemS = signal,
                updater = itemsVar.updater { (lst, v) =>
                  lst.updated(
                    idx,
                    lst(idx).copy(acquirePriceValue = v)
                  )
                }
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
      updater = itemsVar
        .updater { (lst, str) =>
          lst.updated(
            idx,
            lst(idx).copy(acquireSource = if (str.isEmpty()) None else str.some)
          )
        }
    )

    val detailsCell = mkTextAreaCell(
      accessor = _.details.getOrElse(""),
      updater = itemsVar
        .updater { (lst, str) =>
          lst.updated(
            idx,
            lst(idx).copy(details = if (str.isEmpty()) None else str.some)
          )
        }
    )

    val storageIdInputBus: EventBus[Either[String, Option[Int]]] = new EventBus
    val storageCell = td(
      text <-- lockAndItemSignal.withCurrentValueOf(storagesVar).map {
        case (true, item, storages) =>
          item.storageId
            .flatMap { id => storages.find(_.id == id) }
            .flatMap(_.label)
            .getOrElse("")
        case _ => ""
      },
      child.maybe <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
        _ => None,
        _ =>
          select(
            option(value := "", "Choose storage location..."),
            children <-- storagesVar.signal.map { lst =>
              lst.map { s => option(value := s.id.toString(), s.label) }
            },
            value <-- signal.map(_.storageId.fold("")(_.toString())),
            onInput.mapToValue.map[Either[String, Option[Int]]] { x =>
              if (x.isEmpty()) Right(None)
              else
                x.toIntOption match
                  case None        => Left("Invalid storage_id: " + x)
                  case Some(value) => Right(value.some)
            } --> storageIdInputBus.writer,
            storageIdInputBus.events.collect {
              case Right(value) => value
              case Left(_)      => None
            } --> itemsVar.updater[Option[Int]] { (lst, storageId) =>
              lst.updated(idx, lst(idx).copy(storageId = storageId))
            },
            storageIdInputBus.events.collect { case Left(err) =>
              err
            } --> logVar.writer.contramap { err =>
              (
                LogLevel.Error,
                s"Error encountered while updating storage_id for row $idx: $err"
              ).some
            }
          ).some
      )
    )

    val removeRowCell = td(
      button(
        typ := "button",
        onClick --> itemsVar.updater { (lst, _) =>
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
