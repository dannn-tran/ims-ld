package imsld.model

final case class PagedResponse[T](
    data: List[T],
    paging: PagingResponse
)
