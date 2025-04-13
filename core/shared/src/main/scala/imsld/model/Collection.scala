package imsld.model

final case class Collection(
    id: Long,
    slug: Option[String],
    label: Option[String],
    items: List[Item],
    collections: List[CollectionPartial],
    tags: List[Tag]
) extends WithId

final case class CollectionPartial(
    id: Long,
    slug: Option[String],
    label: Option[String]
) extends WithId
