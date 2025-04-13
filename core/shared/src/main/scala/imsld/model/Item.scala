package imsld.model

import java.time.LocalDate

final case class Item(
    id: Long,
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[LocalDate],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    linkedEntities: List[(ItemEntityRelType, EntityPartial)],
    tags: List[Tag]
) extends WithId

final case class ItemPartial(
    id: Long,
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[LocalDate],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String]
) extends WithId

final case class ItemDto(
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[LocalDate],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String]
)
