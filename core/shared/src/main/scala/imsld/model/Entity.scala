package imsld.model

final case class Entity(
    id: Long,
    slug: Option[String],
    label: Option[String],
    linkedItems: List[(ItemPartial, ItemEntityRelType)],
    tags: List[Tag]
) extends WithId

final case class EntityPartial(
    id: Long,
    slug: Option[String],
    label: Option[String]
) extends WithId
