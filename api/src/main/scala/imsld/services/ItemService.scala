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

final case class ItemService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
):
  def insertMany(items: List[ItemDto]): F[List[InsertedRowWithId]] =
    pgSessionPool.use { session =>
      for
        ps <- session.prepare(insertManyQuery(items.length))
        ids <- ps.stream(items, 64).compile.toList
      yield ids
    }

  def getAll(): F[List[ItemPartial]] =
    pgSessionPool.use { session =>
      session.execute(getAllQuery)
    }

  def getOneById(id: Long): F[Option[Item]] =
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
      .query(int8)
      .to[InsertedRowWithId]

  val getAllQuery: Query[skunk.Void, ItemPartial] =
    sql"""
      SELECT id, slug, label, acquire_date, acquire_price, acquire_source
      FROM items
    """
      .query(
        int8 *: varchar(
          64
        ).opt *: text.opt *: date.opt *: monetary_amount.opt *:
          text.opt
      )
      .to[ItemPartial]

  val getOneByIdQuery: Query[Long, Item] =
    sql"""
      SELECT id, slug, label, acquire_date, acquire_price, acquire_source
      FROM items
      WHERE id = $int8
    """
      .query(
        int8 *: varchar(
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
