package imsld.dashboard

import io.circe.Codec

object pages {
  sealed trait Page(val title: String)

  case object HomePage
      extends Page("Inventory Management System with Linked Data")
  case object ItemViewAllPage extends Page("Items - View")
  case object ItemAddBulkPage extends Page("Items - Bulk Creation")
  case object NotFoundPage extends Page("Not Found")
  case object StorageViewAllPage extends Page("Storages - View")

  given pageCodec: Codec[Page] = Codec.derived
}
