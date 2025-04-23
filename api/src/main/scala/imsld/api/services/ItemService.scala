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
  PagingRequest,
  StorageSlim
}
import imsld.model.ItemSlim

given PgStatementProvider[Item, ItemNew, ItemPartial, ItemSlim] = ItemService

final case class ItemService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
) extends ServiceBase[F, Item, ItemNew, ItemPartial, ItemSlim](pgSessionPool)

object ItemService
    extends PgStatementProvider[Item, ItemNew, ItemPartial, ItemSlim]:
  val monetary_amount: Codec[MonetaryAmount] = CompositeTypeCodec
    .mkComposite(
      varchar(3) *: numeric(9, 2),
      CompositeType.Custom("monetary_amount")
    )
    .imap(MonetaryAmount.apply)(a => a.currency -> a.value)

  val itemNewEnc: Encoder[ItemNew] =
    (
      varchar(64).opt
        *: text.opt
        *: varchar(10).opt
        *: monetary_amount.opt
        *: text.opt
        *: int4.opt
        *: text.opt
    )
      .contramap { obj =>
        (
          obj.slug,
          obj.label,
          obj.acquireDate,
          obj.acquirePrice,
          obj.acquireSource,
          obj.storageId,
          obj.details
        )
      }

  def insertManyQuery(n: Int): Query[List[ItemNew], InsertedRowWithId] =
    sql"""
      INSERT INTO items (slug, label, acquire_date, acquire_price, acquire_source, storage_id, details)
      VALUES ${itemNewEnc.values.list(n)}
      RETURNING id
    """
      .query(int4)
      .to[InsertedRowWithId]

  val countQuery: Query[skunk.Void, Int] =
    sql"""
      SELECT COUNT(*) FROM items
    """.query(int8).map(_.toInt)

  val getAllPartialQuery: Query[PagingRequest, ItemPartial] =
    sql"""
      SELECT i.id, i.slug, i.label, i.acquire_date, i.acquire_price, i.acquire_source, s.id, s.slug, s.label, i.details
      FROM items i
      LEFT JOIN storages s
      ON i.storage_id = s.id
      ORDER BY i.id
      OFFSET $int4
      LIMIT $int4
    """
      .query(
        int4
          *: varchar(64).opt
          *: text.opt
          *: varchar(10).opt
          *: monetary_amount.opt
          *: text.opt
          *: int4.opt
          *: varchar(64).opt
          *: text.opt
          *: text.opt
      )
      .map {
        (
            id,
            slug,
            label,
            acquireDate,
            acquirePrice,
            acquireSource,
            storageId,
            storageSlug,
            storageLabel,
            details
        ) =>
          ItemPartial(
            id,
            slug,
            label,
            acquireDate,
            acquirePrice,
            acquireSource,
            storageId.map { id => StorageSlim(id, storageSlug, storageLabel) },
            details
          )
      }
      .contramap { p => (p.offset, p.limit) }

  val getAllSlimQuery: Query[PagingRequest, ItemSlim] =
    sql"""
      SELECT id, slug, label
      FROM items
      ORDER BY id
      OFFSET $int4
      LIMIT $int4
    """
      .query(int4 *: varchar(64).opt *: text.opt)
      .to[ItemSlim]
      .contramap { p =>
        (p.offset, p.limit)
      }

  val getOneByIdQuery: Query[Int, Item] =
    sql"""
      SELECT i.id, i.slug, i.label, i.acquire_date, i.acquire_price, i.acquire_source, i.storage_id, s.slug, s.label, i.details
      FROM items i
      LEFT JOIN storages s
      ON i.storage_id = s.id
      WHERE i.id = $int4
    """
      .query(
        int4 // i.id
          *: varchar(64).opt // i.slug
          *: text.opt // i.label
          *: varchar(10).opt // i.acquire_date
          *: monetary_amount.opt // i.acquire_price
          *: text.opt // i.acquire_source
          *: int4.opt // i.storage_id
          *: varchar(64).opt // s.slug
          *: text.opt // s.label
          *: text.opt // i.details
      )
      .map {
        case (
              id,
              slug,
              label,
              acquireDate,
              acquirePrice,
              acquireSource,
              storageId,
              storageSlug,
              storageLabel,
              details
            ) =>
          Item(
            id,
            slug,
            label,
            acquireDate,
            acquirePrice,
            acquireSource,
            storageId.map { id =>
              StorageSlim(id, storageSlug, storageLabel)
            },
            details,
            List.empty,
            List.empty
          )
      }
