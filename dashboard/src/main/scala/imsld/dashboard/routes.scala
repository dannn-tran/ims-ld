package imsld.dashboard

import com.raquo.waypoint.{Route, *}

import imsld.dashboard.pages.*

val routes = List(
  Route.static(HomePage, root / endOfSegments),
  Route.static(ItemViewAllPage, root / "items" / "view" / endOfSegments),
  Route[ItemByIdPage, Int](
    encode = _.id,
    decode = ItemByIdPage.apply,
    pattern = root / "items" / segment[Int] / endOfSegments
  ),
  Route.static(ItemAddBulkPage, root / "items" / "add" / endOfSegments),
  Route.static(StorageViewAllPage, root / "storages" / "view" / endOfSegments)
)
