package imsld.api.routes

import cats.effect.Concurrent
import io.circe.generic.auto.*

import imsld.api.services.ItemService

final class ItemRouter[F[_]: Concurrent](service: ItemService[F])
    extends RouterBase(service)
