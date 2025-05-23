package imsld.model

final case class Storage(
    id: Int,
    slug: Option[String],
    label: Option[String],
    description: Option[String],
    items: List[ItemPartial]
) extends WithId

final case class StoragePut(
    slug: Option[String],
    label: Option[String],
    description: Option[String]
)

final case class StoragePartial(
    id: Int,
    slug: Option[String],
    label: Option[String],
    description: Option[String],
    itemCount: Long
) extends WithId

final case class StorageSlim(
    id: Int,
    slug: Option[String],
    label: Option[String]
)
