package imsld.model

final case class ItemEntityRelType(
    slug: String,
    label: Option[String]
) extends WithSlug
