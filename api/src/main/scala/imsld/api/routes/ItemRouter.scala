package imsld.api.routes

import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe
import io.circe.generic.auto.*

import imsld.api.services.ItemService
import imsld.model.ItemNew
import imsld.model.Item
import imsld.model.ItemPartial

final class ItemRouter[F[_]: Concurrent](service: ItemService[F])
    extends RouterBase(service)
