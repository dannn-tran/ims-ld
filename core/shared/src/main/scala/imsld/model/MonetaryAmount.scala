package imsld.model

final case class MonetaryAmount(
    currency: String,
    value: BigDecimal
):
  override def toString(): String = s"$currency $value"
