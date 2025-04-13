package imsld.model

final case class Entity(
    id: Int,
    slug: Option[String],
    label: Option[String],
    linkedItems: List[(ItemPartial, ItemEntityRelType)],
    tags: List[Tag]
) extends WithId

final case class EntityPartial(
    id: Int,
    slug: Option[String],
    label: Option[String]
) extends WithId
