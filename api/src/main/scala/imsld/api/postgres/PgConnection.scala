package imsld.api.postgres

import cats.effect.std.Console
import cats.effect.{Resource, Temporal}
import fs2.io.net.Network
import natchez.Trace
import skunk.Session
import skunk.util.Typer

object PgConnection {
  def single[F[_]: Temporal: Trace: Network: Console](
      config: PgConfig
  ): Resource[F, Session[F]] =
    Session.single(
      host = config.host,
      port = config.port,
      user = config.username,
      password = Some(config.password),
      database = config.database,
      strategy = Typer.Strategy.SearchPath
    )

  def pooled[F[_]: Temporal: Trace: Network: Console](
      config: PgConfig
  ): Resource[F, Resource[F, Session[F]]] =
    Session.pooled(
      host = config.host,
      port = config.port,
      user = config.username,
      password = Some(config.password),
      database = config.database,
      max = 10,
      strategy =
        Typer.Strategy.SearchPath // To specify custom types using typname instaed of oid
    )
}
