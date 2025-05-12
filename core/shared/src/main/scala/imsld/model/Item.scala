package imsld.model

final case class Item(
    id: Int,
    slug: Option[String],
    label: Option[String],
    publishDate: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storage: Option[StorageSlim],
    details: Option[String],
    linkedEntities: List[(ItemEntityRelType, EntityPartial)],
    tags: List[Tag]
) extends WithId

final case class ItemPartial(
    id: Int,
    slug: Option[String],
    label: Option[String],
    publishDate: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storage: Option[StorageSlim],
    details: Option[String]
) extends WithId

final case class ItemSlim(
    id: Int,
    slug: Option[String],
    label: Option[String]
) extends WithId

final case class ItemNew(
    slug: Option[String],
    label: Option[String],
    publishDate: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storageId: Option[Int],
    details: Option[String]
)

final case class ItemUpdated(
    id: Int,
    slug: Option[String],
    label: Option[String],
    publishDate: Option[String],
    acquireDate: Option[String],
    acquirePrice: Option[MonetaryAmount],
    acquireSource: Option[String],
    storageId: Option[Int],
    details: Option[String]
) extends WithId
