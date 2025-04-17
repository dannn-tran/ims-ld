package imsld.dashboard

import io.circe.Codec

object pages {
  sealed trait Page(val title: String)
  sealed abstract class PageWithTitle(title: String) extends Page(title)

  case object ItemViewAllPage extends PageWithTitle("Items - View")
  case object ItemAddBulkPage extends PageWithTitle("Items - Bulk Creation")
  case object NotFoundPage extends Page("Not Found")

  given pageCodec: Codec[Page] = Codec.derived
}
