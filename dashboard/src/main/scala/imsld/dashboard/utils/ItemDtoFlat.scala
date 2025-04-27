package imsld.dashboard.utils
import java.text.NumberFormat
import java.util.Locale

import scala.util.Try

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*
import com.raquo.laminar.api.L.*

import imsld.model.{ItemNew, MonetaryAmount}

final case class ItemDtoFlat(
    slug: Option[String] = None,
    label: Option[String] = None,
    publishDate: Option[String] = None,
    acquireDate: Option[String] = None,
    acquirePriceCurrency: Option[String] = None,
    acquirePriceValue: Option[BigDecimal] = None,
    acquireSource: Option[String] = None,
    details: Option[String] = None,
    storageId: Option[Int] = None
):
  def validate: ValidatedNec[String, ItemNew] =
    for
      acquirePrice <- (acquirePriceCurrency, acquirePriceValue) match
        case (Some(ccy), Some(value)) => Valid(MonetaryAmount(ccy, value).some)
        case (_, None)                => Valid(None)
        case _ =>
          Invalid(
            NonEmptyChain(
              s"Currency of acquisition price must be present when value is present for $this"
            )
          )
      _slug = slug.flatMap(sanitiseText)
      _label = label.flatMap(sanitiseText)
      _acquireSource = acquireSource.flatMap(sanitiseText)
      _details = details.flatMap(sanitiseText)
    yield ItemNew(
      _slug,
      _label,
      acquireDate,
      acquirePrice,
      _acquireSource,
      storageId,
      _details
    )

object ItemDtoFlat:
  def acquirePriceValueInput(
      itemS: Signal[ItemDtoFlat],
      updater: Observer[Option[BigDecimal]]
  ) =
    val lastGoodPriceValue: Var[String] = Var("")
    input(
      placeholder := "value",
      controlled(
        value <-- lastGoodPriceValue.signal,
        onInput.mapToValue --> lastGoodPriceValue.writer
      ),
      lastGoodPriceValue.signal.map { str =>
        Try(BigDecimal(str.filter(_ != ','))).toOption
      } --> updater,
      itemS.map(_.acquirePriceValue) --> lastGoodPriceValue
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
