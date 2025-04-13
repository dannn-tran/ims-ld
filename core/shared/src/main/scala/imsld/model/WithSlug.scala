package imsld.model

trait WithSlug:
  def slug: String

final case class InsertedRowWithSlug(slug: String) extends WithSlug
