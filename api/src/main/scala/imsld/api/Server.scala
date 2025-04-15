package imsld.api

import cats.effect.std.Console
import cats.effect.{Async, Resource}
import fs2.io.net.Network
import natchez.Trace
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.{ErrorHandling, Logger, *}
import org.http4s.server.{Router, Server}
import pureconfig.ConfigSource
import pureconfig.error.ConfigReaderFailures

import imsld.api.postgres.PgConnection
import imsld.api.routes.*
import imsld.api.services.ItemService

object Server {
  private def withLogging[F[_]: Async](httpApp: HttpApp[F]): HttpApp[F] =
    Logger.httpApp(true, true)(ErrorHandling(httpApp))

  private def withCors[F[_]: Async](httpApp: HttpApp[F]): HttpApp[F] =
    CORS.policy.withAllowOriginAll.withAllowMethodsAll.apply(httpApp)

  private def fromConfig[F[_]: Async: Trace: Network: Console](
      config: AppConfig
  ): Resource[F, Server] = for {
    pgSessionPool <- PgConnection.pooled(config.postgres)

    itemService = ItemService(pgSessionPool)

    httpApp = Router(
      "items" -> ItemRouter(itemService).routes
    ).orNotFound

    server <-
      EmberServerBuilder
        .default[F]
        .withHost(config.server.host)
        .withPort(config.server.port)
        .withHttpApp(withLogging(withCors(httpApp)))
        .build
  } yield server

  def build[F[_]: Async: Trace: Network: Console]
      : Either[ConfigReaderFailures, Resource[F, Server]] =
    ConfigSource.default.load[AppConfig].map(fromConfig)
}
