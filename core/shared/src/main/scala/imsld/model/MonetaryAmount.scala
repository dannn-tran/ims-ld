package imsld.model

final case class MonetaryAmount(
    currency: String,
    value: BigDecimal
)
