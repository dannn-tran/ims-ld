package imsld.routes

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe
import io.circe.generic.auto.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.io.*
import org.http4s.{HttpRoutes, Request, Response}

import imsld.model.ItemDto
import imsld.services.ItemService
import imsld.routes.common.*
import imsld.model.PagingRequest

final class ItemRouter[F[_]: Concurrent](service: ItemService[F])
    extends Http4sDsl[F] {
  val routes: HttpRoutes[F] =
    HttpRoutes.of[F] {
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
          .attemptAs[List[ItemDto]]
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
