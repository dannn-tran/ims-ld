package imsld.dashboard.utils

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.all.*

import imsld.model.{ItemNew, MonetaryAmount}

final case class ItemDtoFlat(
    slug: Option[String] = None,
    label: Option[String] = None,
    publishDate: Option[String] = None,
    acquireDate: Option[String] = None,
    acquirePriceCurrency: Option[String] = None,
    acquirePriceValue: Either[String, Option[BigDecimal]] = Right(None),
    acquireSource: Option[String] = None,
    details: Option[String] = None,
    storageId: Either[String, Option[Int]] = Right(None)
):
  def validate: ValidatedNec[String, ItemNew] =
    (
      acquirePriceValue.toValidatedNec andThen { _acquirePriceValue =>
        (acquirePriceCurrency, _acquirePriceValue) match
          case (Some(ccy), Some(value)) =>
            Valid(MonetaryAmount(ccy, value).some)
          case (_, None) => Valid(None)
          case _ =>
            Invalid(
              NonEmptyChain(
                s"Acquisition price must have currency specified"
              )
            )
      },
      storageId.toValidatedNec
    ) mapN { (acquirePrice, _storageId) =>
      val _slug = slug.flatMap(sanitiseText)
      val _label = label.flatMap(sanitiseText)
      val _acquireSource = acquireSource.flatMap(sanitiseText)
      val _details = details.flatMap(sanitiseText)

      ItemNew(
        _slug,
        _label,
        acquireDate,
        acquirePrice,
        _acquireSource,
        _storageId,
        _details
      )
    }
