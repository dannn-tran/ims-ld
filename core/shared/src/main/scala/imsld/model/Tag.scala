package imsld.model

final case class Tag(
    slug: String,
    label: Option[String]
) extends WithSlug
