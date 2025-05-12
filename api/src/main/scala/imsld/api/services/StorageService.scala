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
  StorageNew,
  StoragePartial,
  StorageSlim
}
import imsld.model.StorageUpdated
import skunk.Command

given PgStatementProvider[
  Storage,
  StoragePartial,
  StorageSlim,
  StorageNew,
  StorageUpdated
] =
  StorageService

final case class StorageService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
) extends ServiceBase[
      F,
      Storage,
      StoragePartial,
      StorageSlim,
      StorageNew,
      StorageUpdated
    ](
      pgSessionPool
    )

object StorageService
    extends PgStatementProvider[
      Storage,
      StoragePartial,
      StorageSlim,
      StorageNew,
      StorageUpdated
    ]:
  val storageNewEnc: Encoder[StorageNew] =
    (varchar(64).opt *: text.opt *: text.opt)
      .contramap { obj =>
        (
          obj.slug,
          obj.label,
          obj.description
        )
      }

  def insertManyQuery(n: Int): Query[List[StorageNew], InsertedRowWithId] =
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

  private val storageUpdatedEnc: Encoder[StorageUpdated] =
    (
      int4
        *: varchar(64).opt
        *: text.opt
        *: text.opt
    ).contramap[StorageUpdated] { s =>
      (
        s.id,
        s.slug,
        s.label,
        s.description
      )
    }
  def updateManyCmd(n: Int): Command[List[StorageUpdated]] =
    sql"""
      UPDATE storages AS s
      SET
        s.slug = u.slug,
        s.label = u.label,
        s.description = u.description
      FROM ( VALUES
        ${storageUpdatedEnc.values.list(n)}
      ) AS u(id, slug, label, description)
       WHERE s.id = u.id
    """".command
