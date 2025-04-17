package imsld.api.services

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import skunk.{Query, Session}

import imsld.model.{
  InsertedRowWithId,
  PagedResponse,
  PagingRequest,
  PagingResponse
}

abstract class ServiceBase[F[_]: Sync, T, TNew, TPartial](
    pgSessionPool: Resource[F, Session[F]]
)(implicit companion: ServiceCompanion[T, TNew, TPartial]):
  def insertMany(objs: List[TNew]): F[List[InsertedRowWithId]] =
    pgSessionPool.use { session =>
      for
        query <- session.prepare(companion.insertManyQuery(objs.length))
        ids <- query.stream(objs, 64).compile.toList
      yield ids
    }

  def getAll(pagingRequest: PagingRequest): F[PagedResponse[TPartial]] =
    for
      data <- pgSessionPool.use { session =>
        for
          dataQuery <- session.prepare(companion.getAllQuery)
          data <- dataQuery.stream(pagingRequest, 64).compile.toList
        yield data
      }
      count <- pgSessionPool.use(_.unique(companion.countQuery))
    yield PagedResponse(
      data = data,
      paging = PagingResponse(
        pageNumber = pagingRequest.pageNumber,
        pageSize = pagingRequest.pageSize,
        totalPages = Math.ceilDiv(count, pagingRequest.pageSize)
      )
    )

  def getOneById(id: Int): F[Option[T]] =
    pgSessionPool.use { session =>
      for
        ps <- session.prepare(companion.getOneByIdQuery)
        item <- ps.option(id)
      yield item
    }

trait ServiceCompanion[T, TNew, TPartial]:
  def insertManyQuery(n: Int): Query[List[TNew], InsertedRowWithId]
  def getAllQuery: Query[PagingRequest, TPartial]
  def getOneByIdQuery: Query[Int, T]
  def countQuery: Query[skunk.Void, Int]
