package imsld.services

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import skunk.codec.all.*
import skunk.implicits.*
import skunk.syntax.*
import skunk.{Codec, Encoder, Query, Session}

import imsld.model.{
  InsertedRowWithId,
  Item,
  ItemDto,
  ItemPartial,
  MonetaryAmount
}
import imsld.services.ItemService.{
  getAllQuery,
  getOneByIdQuery,
  insertManyQuery
}
import imsld.model.PagedResponse
import imsld.model.PagingRequest
import imsld.services.ItemService.countQuery
import imsld.model.PagingResponse

final case class ItemService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
):
  def insertMany(items: List[ItemDto]): F[List[InsertedRowWithId]] =
    pgSessionPool.use { session =>
      for
        query <- session.prepare(insertManyQuery(items.length))
        ids <- query.stream(items, 64).compile.toList
      yield ids
    }

  def getAll(pagingRequest: PagingRequest): F[PagedResponse[ItemPartial]] =
    pgSessionPool.use { session =>
      for
        dataQuery <- session.prepare(getAllQuery)
        data <- dataQuery.stream(pagingRequest, 64).compile.toList
        count <- session.unique(countQuery)
      yield PagedResponse(
        data = data,
        paging = PagingResponse(
          pageNumber = pagingRequest.pageNumber,
          pageSize = pagingRequest.pageSize,
          totalPages = Math.ceilDiv(count, pagingRequest.pageSize)
        )
      )
    }

  def getOneById(id: Int): F[Option[Item]] =
    pgSessionPool.use { session =>
      for
        ps <- session.prepare(getOneByIdQuery)
        item <- ps.option(id)
      yield item
    }

object ItemService:
  val monetary_amount: Codec[MonetaryAmount] = CompositeTypeCodec
    .mkComposite(
      varchar(3) *: numeric(9, 2),
      CompositeType.Custom("monetary_amount")
    )
    .imap(MonetaryAmount.apply)(a => a.currency -> a.value)

  val itemDtoEnc: Encoder[ItemDto] =
    (varchar(64).opt *: text.opt *: date.opt *: monetary_amount.opt *: text.opt)
      .contramap(dto =>
        (
          dto.slug,
          dto.label,
          dto.acquireDate,
          dto.acquirePrice,
          dto.acquireSource
        )
      )

  def insertManyQuery(n: Int): Query[List[ItemDto], InsertedRowWithId] =
    sql"""
      INSERT INTO items (slug, label, acquire_date, acquire_price, acquire_source)
      VALUES ${itemDtoEnc.values.list(n)}
      RETURNING id
    """
      .query(int4)
      .to[InsertedRowWithId]

  val countQuery: Query[skunk.Void, Int] =
    sql"""
      SELECT COUNT(*) FROM ITEMS
    """.query(int8).map(_.toInt)

  val getAllQuery: Query[PagingRequest, ItemPartial] =
    sql"""
      SELECT id, slug, label, acquire_date, acquire_price, acquire_source
      FROM items
      ORDER BY id
      OFFSET $int4
      LIMIT $int4
    """
      .query(
        int4 *: varchar(
          64
        ).opt *: text.opt *: date.opt *: monetary_amount.opt *:
          text.opt
      )
      .to[ItemPartial]
      .contramap(p => (p.pageSize * p.pageNumber, p.pageSize))

  val getOneByIdQuery: Query[Int, Item] =
    sql"""
      SELECT id, slug, label, acquire_date, acquire_price, acquire_source
      FROM items
      WHERE id = $int4
    """
      .query(
        int4 *: varchar(
          64
        ).opt *: text.opt *: date.opt *: monetary_amount.opt *: text.opt
      )
      .map { case (id, slug, label, acquireDate, acquirePrice, acquireSource) =>
        Item(
          id,
          slug,
          label,
          acquireDate,
          acquirePrice,
          acquireSource,
          List.empty,
          List.empty
        )
      }
