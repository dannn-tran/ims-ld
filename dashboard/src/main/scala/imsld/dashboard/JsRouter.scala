package imsld.dashboard

import com.raquo.laminar.api.L.*
import com.raquo.waypoint
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalajs.dom

import imsld.dashboard.pages.Page

object JsRouter
    extends waypoint.Router[Page](
      routes = routes,
      getPageTitle = _.title,
      serializePage = _.asJson.noSpaces,
      deserializePage = decode[Page] `andThen` (_.right) `andThen` (_.get),
      routeFallback = _ => pages.NotFoundPage
    ) {
  currentPageSignal.foreach { page => dom.window.scrollTo(x = 0, y = 0) }(owner)

  def titleLink(id: String, caption: String = "#"): Modifier.Base =
    List[Modifier.Base](
      Modifier(parentEl => parentEl.ref.id = id),
      a(href("#$id"), caption)
    )
}
