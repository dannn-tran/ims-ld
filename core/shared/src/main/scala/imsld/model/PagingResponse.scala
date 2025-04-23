package imsld.model

final case class PagingResponse(
    offset: Int,
    limit: Int,
    total: Int
)
