package imsld.model

trait WithId:
  def id: Int

final case class InsertedRowWithId(id: Int) extends WithId
