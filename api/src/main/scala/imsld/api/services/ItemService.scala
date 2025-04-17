package imsld.api.services

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import skunk.codec.all.*
import skunk.implicits.*
import skunk.syntax.*
import skunk.{Codec, Encoder, Query, Session}

import imsld.model.{
  InsertedRowWithId,
  Item,
  ItemNew,
  ItemPartial,
  MonetaryAmount,
  PagingRequest
}

given PgStatementProvider[Item, ItemNew, ItemPartial] = ItemService

final case class ItemService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
) extends ServiceBase[F, Item, ItemNew, ItemPartial](pgSessionPool)

object ItemService extends PgStatementProvider[Item, ItemNew, ItemPartial]:
  val monetary_amount: Codec[MonetaryAmount] = CompositeTypeCodec
    .mkComposite(
      varchar(3) *: numeric(9, 2),
      CompositeType.Custom("monetary_amount")
    )
    .imap(MonetaryAmount.apply)(a => a.currency -> a.value)

  val itemNewEnc: Encoder[ItemNew] =
    (varchar(64).opt *: text.opt *: date.opt *: monetary_amount.opt *: text.opt)
      .contramap { obj =>
        (
          obj.slug,
          obj.label,
          obj.acquireDate,
          obj.acquirePrice,
          obj.acquireSource
        )
      }

  def insertManyQuery(n: Int): Query[List[ItemNew], InsertedRowWithId] =
    sql"""
      INSERT INTO items (slug, label, acquire_date, acquire_price, acquire_source)
      VALUES ${itemNewEnc.values.list(n)}
      RETURNING id
    """
      .query(int4)
      .to[InsertedRowWithId]

  val countQuery: Query[skunk.Void, Int] =
    sql"""
      SELECT COUNT(*) FROM items
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
        int4
          *: varchar(64).opt
          *: text.opt
          *: date.opt
          *: monetary_amount.opt
          *: text.opt
      )
      .to[ItemPartial]
      .contramap(p => (p.pageSize * (p.pageNumber - 1), p.pageSize))

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
