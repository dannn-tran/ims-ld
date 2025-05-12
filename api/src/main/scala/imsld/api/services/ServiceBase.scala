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
import skunk.Command
import skunk.data.Completion

abstract class ServiceBase[F[_]: Sync, T, TPartial, TSlim, TNew, TUpdated](
    pgSessionPool: Resource[F, Session[F]]
)(implicit companion: PgStatementProvider[T, TPartial, TSlim, TNew, TUpdated]):
  def insertMany(objs: List[TNew]): F[List[InsertedRowWithId]] =
    pgSessionPool.use { session =>
      for
        query <- session.prepare(companion.insertManyQuery(objs.length))
        // TODO: handle .recoverWith { case SqlState.UniqueViolation(ex) =>  ...}
        // TODO: handle .recoverWith { case SqlState.ForeignKeyViolation(ex) =>  ...}
        ids <- query.stream(objs, 64).compile.toList
      yield ids
    }

  def getAllPartial(pagingRequest: PagingRequest): F[PagedResponse[TPartial]] =
    for
      data <- pgSessionPool.use { session =>
        for
          dataQuery <- session.prepare(companion.getAllPartialQuery)
          data <- dataQuery.stream(pagingRequest, 64).compile.toList
        yield data
      }
      count <- pgSessionPool.use(_.unique(companion.countQuery))
    yield PagedResponse(
      data = data,
      paging = PagingResponse(
        offset = pagingRequest.offset,
        limit = pagingRequest.limit,
        total = count
      )
    )

  def getAllSlim(pagingRequest: PagingRequest): F[PagedResponse[TSlim]] =
    for
      data <- pgSessionPool.use { session =>
        for
          dataQuery <- session.prepare(companion.getAllSlimQuery)
          data <- dataQuery.stream(pagingRequest, 64).compile.toList
        yield data
      }
      count <- pgSessionPool.use(_.unique(companion.countQuery))
    yield PagedResponse(
      data = data,
      paging = PagingResponse(
        offset = pagingRequest.offset,
        limit = pagingRequest.limit,
        total = count
      )
    )

  def getOneById(id: Int): F[Option[T]] =
    pgSessionPool.use { session =>
      for
        ps <- session.prepare(companion.getOneByIdQuery)
        obj <- ps.option(id)
      yield obj
    }

  def updateMany(items: List[TUpdated]): F[Completion] =
    pgSessionPool.use { session =>
      for
        ps <- session.prepare(companion.updateManyCmd(items.size))
        c <- ps.execute(items)
      yield c
    }

trait PgStatementProvider[T, TPartial, TSlim, TNew, TUpdated]:
  def insertManyQuery(n: Int): Query[List[TNew], InsertedRowWithId]
  def getAllPartialQuery: Query[PagingRequest, TPartial]
  def getAllSlimQuery: Query[PagingRequest, TSlim]
  def getOneByIdQuery: Query[Int, T]
  def countQuery: Query[skunk.Void, Int]
  def updateManyCmd(n: Int): Command[List[TUpdated]]
