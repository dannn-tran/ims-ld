package imsld.model

final case class Storage(
    id: Int,
    slug: Option[String],
    label: Option[String],
    description: Option[String]
) extends WithId

final case class StorageNew(
    slug: Option[String],
    label: Option[String],
    description: Option[String]
)

final case class StoragePartial(
    id: Int,
    slug: Option[String],
    label: Option[String]
) extends WithId
