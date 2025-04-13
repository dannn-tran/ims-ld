package imsld.model

trait WithId:
  def id: Long

final case class InsertedRowWithId(id: Long) extends WithId
