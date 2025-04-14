package imsld.api.routes

import org.http4s.dsl.impl.QueryParamDecoderMatcherWithDefault

object common:
  val DEFAULT_PAGE_NUMBER = 1 // first apge
  val DEFAULT_PAGE_SIZE = 50

  object PageNumberQueryParamMatcher
      extends QueryParamDecoderMatcherWithDefault[Int](
        "page-num",
        DEFAULT_PAGE_NUMBER
      )
  object PageSizeQueryParamMatcher
      extends QueryParamDecoderMatcherWithDefault[Int](
        "page-size",
        DEFAULT_PAGE_SIZE
      )
