package imsld.model

final case class PagingResponse(
    pageNumber: Int,
    pageSize: Int,
    totalPages: Int
)
