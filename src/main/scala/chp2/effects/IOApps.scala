package chp2.effects

import cats.effect.{ExitCode, IO, IOApp}
import cats.effect.unsafe.implicits.global
import chp2.effects.IOApps.program

import scala.io.StdIn

// 4
object IOApps {
  val program: IO[Unit] = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"Just written: $line"))
  } yield ()
}

object TestApp {
  def main(args: Array[String]): Unit = {
    program.unsafeRunSync()
  }
}

object FirstApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
//    program.map(_ => ExitCode.Success) // or simply:
    program.as(ExitCode.Success)
  }
}

object MySimpleApp extends IOApp.Simple {
  override def run: IO[Unit] = program
}
