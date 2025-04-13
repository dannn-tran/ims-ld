package imsld

import cats.effect.{IO, IOApp}
import natchez.Trace.Implicits.noop

object Main extends IOApp.Simple {
  val run = Server
    .build[IO]
    .fold(
      failure => IO.println(s"Fail to load application config:\n$failure"),
      _.use { _ =>
        IO.println("Application server ready.") *> IO.never
      }
    )
}
