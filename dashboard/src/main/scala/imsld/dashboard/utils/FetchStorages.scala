package imsld.dashboard.utils

import com.raquo.airstream.core.EventStream
import com.raquo.airstream.web.FetchStream
import io.circe.generic.auto.*
import io.circe.parser.decode

import imsld.dashboard.constants.BACKEND_ENDPOINT
import imsld.model.{PagedResponse, StorageSlim}

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
