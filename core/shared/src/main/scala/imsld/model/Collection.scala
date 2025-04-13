package imsld.model

final case class Collection(
    id: Int,
    slug: Option[String],
    label: Option[String],
    items: List[Item],
    collections: List[CollectionPartial],
    tags: List[Tag]
) extends WithId

final case class CollectionPartial(
    id: Int,
    slug: Option[String],
    label: Option[String]
) extends WithId
