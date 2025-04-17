package imsld.api.services

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import skunk.codec.all.*
import skunk.implicits.*
import skunk.syntax.*
import skunk.{Codec, Encoder, Query, Session}

import imsld.model.{
  InsertedRowWithId,
  PagingRequest,
  Storage,
  StorageNew,
  StoragePartial
}

given PgStatementProvider[Storage, StorageNew, StoragePartial] = StorageService

final case class StorageService[F[_]: Sync](
    pgSessionPool: Resource[F, Session[F]]
) extends ServiceBase[F, Storage, StorageNew, StoragePartial](pgSessionPool)

object StorageService
    extends PgStatementProvider[Storage, StorageNew, StoragePartial]:
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

  val getAllQuery: Query[PagingRequest, StoragePartial] =
    sql"""
      SELECT id, slug, label
      FROM storages
      ORDER BY id
      OFFSET $int4
      LIMIT $int4
    """
      .query(
        int4 *: varchar(
          64
        ).opt *: text.opt
      )
      .to[StoragePartial]
      .contramap(p => (p.pageSize * (p.pageNumber - 1), p.pageSize))

  val getOneByIdQuery: Query[Int, Storage] =
    sql"""
      SELECT id, slug, label, description
      FROM storages
      WHERE id = $int4
    """
      .query(
        int4 *: varchar(
          64
        ).opt *: text.opt *: text.opt
      )
      .map { case (id, slug, label, desc) =>
        Storage(
          id,
          slug,
          label,
          desc
        )
      }
