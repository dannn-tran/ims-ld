package imsld.dashboard

import com.raquo.waypoint.{Route, *}

import imsld.dashboard.pages.*

val routes = List(
  Route.static(ItemViewAllPage, root / "items" / "view" / endOfSegments),
  Route.static(ItemAddBulkPage, root / "items" / "add" / endOfSegments)
)
