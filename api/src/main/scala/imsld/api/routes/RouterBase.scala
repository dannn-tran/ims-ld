package imsld.api.routes

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.*
import org.http4s.{HttpRoutes, Request, Response}

import imsld.api.routes.common.*
import imsld.api.services.ServiceBase
import imsld.model.PagingRequest

abstract class RouterBase[
    F[_]: Concurrent,
    T,
    TNew,
    TPartial,
    Service <: ServiceBase[F, T, TNew, TPartial]
](
    service: Service
)(using circe.Encoder[T], circe.Decoder[TNew], circe.Encoder[TPartial])
    extends Http4sDsl[F] {
  def routes: HttpRoutes[F] =
    HttpRoutes.of[F](customRouteHandler `orElse` defaultRouteHandler)

  def customRouteHandler: PartialFunction[Request[F], F[Response[F]]] =
    PartialFunction.empty

  val defaultRouteHandler: PartialFunction[Request[F], F[Response[F]]] = {
    case GET -> Root :? PageNumberQueryParamMatcher(
          pageNum
        ) +& PageSizeQueryParamMatcher(pageSize) =>
      for {
        data <- service.getAll(
          PagingRequest(pageNum, pageSize)
        )
        resp <- Ok(data)
      } yield resp

    case GET -> Root / IntVar(id) =>
      for {
        itemOpt <- service.getOneById(id)
        resp <- itemOpt.fold(NotFound()) { item => Ok(item) }
      } yield resp

    case req @ POST -> Root =>
      req
        .attemptAs[List[TNew]]
        .foldF(
          err => BadRequest(err.toString),
          docs =>
            for {
              ids <- service.insertMany(docs)
              resp <- Created(ids)
            } yield resp
        )
  }
}
