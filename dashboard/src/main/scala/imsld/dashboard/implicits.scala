package imsld.dashboard

import org.scalajs.dom.Headers

object implicits:
  implicit class HeadersImplicit(headers: Headers):
    def toMap: Map[String, List[String]] = headers.toList
      .map { arr => arr.toList }
      .collect { case key :: values =>
        (key, values)
      }
      .toMap
