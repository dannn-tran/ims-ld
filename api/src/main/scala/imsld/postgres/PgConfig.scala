package imsld.postgres

final case class PgConfig(
    host: String,
    port: Int,
    username: String,
    password: String,
    database: String
)
