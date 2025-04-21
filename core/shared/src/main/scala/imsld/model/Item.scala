package imsld.model

final case class Item(
    id: Int,
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storage: Option[StoragePartial],
    details: Option[String],
    linkedEntities: List[(ItemEntityRelType, EntityPartial)],
    tags: List[Tag]
) extends WithId

final case class ItemPartial(
    id: Int,
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[String]
) extends WithId

final case class ItemNew(
    slug: Option[String],
    label: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storageId: Option[Int],
    details: Option[String]
)
