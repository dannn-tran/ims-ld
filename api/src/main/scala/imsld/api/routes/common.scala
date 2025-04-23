package imsld.api.routes

import org.http4s.dsl.impl.QueryParamDecoderMatcherWithDefault
import org.http4s.QueryParamDecoder
import cats.syntax.all.*
import org.http4s.ParseFailure

object common:
  val DEFAULT_OFFSET = 0
  val DEFAULT_LIMIT = 50

  object OffsetQueryParamMatcher
      extends QueryParamDecoderMatcherWithDefault[Int](
        "offset",
        DEFAULT_OFFSET
      )
  object LimitQueryParamMatcher
      extends QueryParamDecoderMatcherWithDefault[Int](
        "limit",
        DEFAULT_LIMIT
      )

  enum DetailLevel(val value: String):
    case Partial extends DetailLevel("partial")
    case Slim extends DetailLevel("slim")
  object DetailLevel:
    private lazy val _fromString: Map[String, DetailLevel] =
      DetailLevel.values.map { x =>
        (x.value, x)
      }.toMap
    def fromString(str: String): Either[String, DetailLevel] = _fromString
      .get(str)
      .toRight(
        s"$str is an invalid DetailLevel; valid values are ${DetailLevel.values.map(_.value).mkString(", ")}"
      )
  given QueryParamDecoder[DetailLevel] =
    QueryParamDecoder[String].emap { s =>
      DetailLevel.fromString(s.toLowerCase()).leftMap(ParseFailure(_, s))
    }
  object DetailLevelQueryParamMatcher
      extends QueryParamDecoderMatcherWithDefault[DetailLevel](
        "detail",
        DetailLevel.Slim
      )
