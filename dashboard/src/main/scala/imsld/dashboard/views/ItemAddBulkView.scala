package imsld.dashboard.views

import java.text.NumberFormat
import java.util.Locale

import scala.util.Try

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*
import com.raquo.airstream.status.{Pending, Resolved}
import com.raquo.laminar.api.L.*
import com.raquo.laminar.nodes.ReactiveHtmlElement
import io.circe.Encoder
import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom.HTMLDivElement

import imsld.dashboard.BACKEND_ENDPOINT
import imsld.model.{InsertedRowWithId, ItemNew, MonetaryAmount}

object ItemAddBulkView:
  given Encoder[ItemNew] = Encoder.derived

  private val itemsVar: Var[List[ItemDtoFlat]] = Var(List(ItemDtoFlat()))
  private val localErrVar: Var[Option[String]] = Var(None)

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

  def apply(): ReactiveHtmlElement[HTMLDivElement] =
    div(
      h1("Bulk item creation"),
      controlPanel,
      child.maybe <-- localErrVar.signal.splitOption { (_, signal) =>
        div(text <-- signal, cls := "error")
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
        onClick.preventDefault.mapTo(getDto) --> submitBus.writer,
        submitBus.events.collect { case Invalid(err) =>
          err
        } --> localErrVar.writer.contramap[NonEmptyChain[String]](
          _.toList.mkString("\n").some
        ),
        disabled <-- inflightSubmitOrResolvedRightSignal
      ),
      button(
        typ := "button",
        "Clear",
        onClick.mapTo(List.empty) --> itemsVar.writer,
        disabled <-- inflightSubmitSignal
      ),
      div(
        text <-- respStream.splitStatus(
          (resolve, _) =>
            resolve.output.fold(
              err => "Failed to create new items. Error: " + err.toString(),
              succ =>
                "New items successfully created. IDs: " + succ
                  .map(_.id)
                  .mkString(", ")
            ),
          (_, _) => "Submitting..."
        ),
        cls("error") <-- respStream.map {
          case Resolved(_, Left(_), _) => true
          case _                       => false
        }
      )
    )

  private def getDto: ValidatedNec[String, List[ItemNew]] =
    itemsVar.now().traverse(validate)

  private def validate(item: ItemDtoFlat): ValidatedNec[String, ItemNew] =
    for
      acquirePrice <- (item.acquirePriceCurrency, item.acquirePriceValue) match
        case (Some(ccy), Some(value)) => Valid(MonetaryAmount(ccy, value).some)
        case (_, None)                => Valid(None)
        case _ =>
          Invalid(
            NonEmptyChain(
              s"Currency of acquisition price must be present when value is present for $item"
            )
          )
      slug = item.slug.map(_.trim()).flatMap { s =>
        if (s.isEmpty()) None else s.some
      }
      label = item.label.map(_.trim()).flatMap { s =>
        if (s.isEmpty()) None else s.some
      }
      acquireSource = item.acquireSource.map(_.trim()).flatMap { s =>
        if (s.isEmpty()) None else s.some
      }
    yield ItemNew(
      slug,
      label,
      item.acquireDate,
      acquirePrice,
      acquireSource,
      None,
      None
    )

  private val inputForm =
    form(
      table(
        thead(
          tr(
            List(
              "slug",
              "label",
              "publish date",
              "acquire date",
              "acquire price",
              "acquire source",
              "details",
              ""
            ).map { h => th(h) }
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
            .copy(label =
              if (processedStr.isEmpty()) None
              else processedStr.some
            )
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
      val lastGoodPriceValue: Var[String] = Var("")
      td(
        children <-- inflightSubmitOrResolvedRightSignal.splitBoolean(
          _ => List.empty,
          _ =>
            List(
              input(
                cls := "ItemAddTable-acquirePriceCell-ccy",
                placeholder := "ccy",
                controlled(
                  value <-- signal.map(_.acquirePriceCurrency.getOrElse("")),
                  onInput.mapToValue --> itemsVar
                    .updater[String] { (lst, str) =>
                      lst.updated(
                        idx,
                        lst(idx).copy(acquirePriceCurrency =
                          if (str.isEmpty()) None else str.some
                        )
                      )
                    }
                ),
                listId := "defaultCurrencies",
                size := 8,
                cls("error") <-- signal.map { i =>
                  (i.acquirePriceCurrency, i.acquirePriceValue) match {
                    case (Some(ccy), Some(_)) if !ccy.isBlank() => true
                    case _                                      => false
                  }
                }
              ),
              dataList(
                idAttr := "defaultCurrencies",
                List("SGD", "USD", "EUR", "VND").map { ccy =>
                  option(value := ccy)
                }
              ),
              input(
                placeholder := "value",
                controlled(
                  value <-- lastGoodPriceValue.signal,
                  onInput.mapToValue --> lastGoodPriceValue.writer
                ),
                lastGoodPriceValue.signal --> itemsVar.updater[String] {
                  (lst, str) =>
                    lst.updated(
                      idx,
                      lst(idx).copy(acquirePriceValue =
                        Try(BigDecimal(str.filter(_ != ','))).toOption
                      )
                    )
                },
                signal.map(_.acquirePriceValue) --> lastGoodPriceValue
                  .updater[Option[BigDecimal]] { (prev, opt) =>
                    opt.fold(prev) { bd =>
                      val formatter = NumberFormat.getInstance(Locale.US)
                      formatter.setMaximumFractionDigits(bd.scale)
                      formatter.setMinimumFractionDigits(bd.scale)
                      formatter.format(bd)
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
      removeRowCell
    )

  private final case class ItemDtoFlat(
      slug: Option[String] = None,
      label: Option[String] = None,
      publishDate: Option[String] = None,
      acquireDate: Option[String] = None,
      acquirePriceCurrency: Option[String] = None,
      acquirePriceValue: Option[BigDecimal] = None,
      acquireSource: Option[String] = None,
      details: Option[String] = None
  )
