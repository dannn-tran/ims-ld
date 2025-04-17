package imsld.api.routes

import cats.effect.Concurrent
import io.circe.generic.auto.*

import imsld.api.services.StorageService

final class StorageRouter[F[_]: Concurrent](service: StorageService[F])
    extends RouterBase(service)
