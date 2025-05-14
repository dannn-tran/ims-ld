package imsld.api.services

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import io.circe.generic.auto.*
import skunk.circe.codec.all.jsonb
import skunk.codec.all.*
import skunk.implicits.*
import skunk.syntax.*
import skunk.{Codec, Encoder, Query, Session}

import imsld.model.{
  InsertedRowWithId,
  ItemPartial,
  PagingRequest,
  Storage,
  StoragePut,
  StoragePartial,
  StorageSlim
}
import skunk.Command

given PgStatementProvider[
  Storage,
  StoragePartial,
  StorageSlim,
  StoragePut
] =
  StorageService

final case class StorageService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
) extends ServiceBase[
      F,
      Storage,
      StoragePartial,
      StorageSlim,
      StoragePut
    ](
      pgSessionPool
    )

object StorageService
    extends PgStatementProvider[
      Storage,
      StoragePartial,
      StorageSlim,
      StoragePut
    ]:
  val storageNewEnc: Encoder[StoragePut] =
    (varchar(64).opt *: text.opt *: text.opt)
      .contramap { obj =>
        (
          obj.slug,
          obj.label,
          obj.description
        )
      }

  def insertManyQuery(n: Int): Query[List[StoragePut], InsertedRowWithId] =
    sql"""
      INSERT INTO storages (slug, label, description)
      VALUES ${storageNewEnc.values.list(n)}
      RETURNING id
    """
      .query(int4)
      .to[InsertedRowWithId]

  val countQuery: Query[skunk.Void, Int] =
    sql"""
      SELECT COUNT(*) FROM storages
    """.query(int8).map(_.toInt)

  val getAllPartialQuery: Query[PagingRequest, StoragePartial] =
    sql"""
      SELECT s.id, s.slug, s.label, s.description, COUNT(i.id)
      FROM storages s
      LEFT JOIN items i
      ON s.id = i.storage_id
      GROUP BY s.id, s.slug, s.label, s.description
      ORDER BY s.id
      OFFSET $int4
      LIMIT $int4
    """
      .query(
        int4
          *: varchar(64).opt
          *: text.opt
          *: text.opt
          *: int8
      )
      .to[StoragePartial]
      .contramap { p => (p.offset, p.limit) }

  val getAllSlimQuery: Query[PagingRequest, StorageSlim] =
    sql"""
      SELECT id, slug, label
      FROM storages
      OFFSET $int4
      LIMIT $int4
    """
      .query(int4 *: varchar(64).opt *: text.opt)
      .to[StorageSlim]
      .contramap { p => (p.offset, p.limit) }

  val getOneByIdQuery: Query[Int, Storage] =
    sql"""
      SELECT 
        s.id,
        s.slug, 
        s.label, 
        s.description, 
        COALESCE(
          JSONB_AGG(
            JSONB_BUILD_OBJECT(
              'id': i.id,
              'slug': i.slug,
              'label': i.label,
              'acquireDate': i.acquire_date
            )
          ) FILTER (WHERE i.id IS NOT NULL),
          '[]'
        )
      FROM storages s
      LEFT JOIN items i
      ON s.id = i.storage_id
      WHERE id = $int4
    """
      .query(
        int4
          *: varchar(64).opt
          *: text.opt
          *: text.opt
          *: jsonb[List[ItemPartial]]
      )
      .map { case (id, slug, label, desc, items) =>
        Storage(
          id,
          slug,
          label,
          desc,
          items
        )
      }

  val updateOneCmd: Command[(Int, StoragePut)] =
    sql"""
      UPDATE storages
      SET
        slug = ${varchar(64).opt},
        label = ${text.opt},
        description = ${text.opt}
       WHERE id = $int4
    """".command
      .contramap[(Int, StoragePut)] { (id, s) =>
        (
          s.slug,
          s.label,
          s.description,
          id
        )
      }
