package imsld.api

import com.comcast.ip4s.{IpAddress, Port}
import pureconfig.ConfigReader
import pureconfig.error.CannotConvert
import pureconfig.generic.derivation.default.derived

import imsld.api.postgres.PgConfig

final case class AppConfig(
    server: ServerConfig,
    postgres: PgConfig
) derives ConfigReader

final case class ServerConfig(host: IpAddress, port: Port)

object ServerConfig {
  final case class _ServerConfig(host: String, port: Int) derives ConfigReader

  given ConfigReader[ServerConfig] = ConfigReader[_ServerConfig].emap { conf =>
    for {
      host <- IpAddress
        .fromString(conf.host)
        .toRight(
          CannotConvert(
            conf.host,
            "com.comcast.ip4s.IpAddress",
            "Invalid format"
          )
        )
      port <- Port
        .fromInt(conf.port)
        .toRight(
          CannotConvert(
            conf.port.toString,
            "com.comcast.ip4s.Port",
            "Invalid format"
          )
        )
    } yield ServerConfig(host, port)
  }
}
