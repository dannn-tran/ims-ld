package imsld.dashboard.utils

import imsld.model.StorageSlim
import com.raquo.airstream.core.EventStream
import imsld.model.PagedResponse
import com.raquo.airstream.web.FetchStream
import imsld.dashboard.constants.BACKEND_ENDPOINT
import io.circe.parser.decode
import io.circe.generic.auto.*

object FetchStorages:
  def stream: EventStream[Either[Throwable, PagedResponse[StorageSlim]]] =
    FetchStream
      .get(s"$BACKEND_ENDPOINT/storages")
      .recoverToEither
      .map(
        _.fold(
          err => Left(err),
          decode[PagedResponse[StorageSlim]]
        )
      )
