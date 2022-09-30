package chp2.effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

// 3
object IOErrorHandling {

  // IO: pure, delay, defer
  // Create failed computations/effects
  val failedCompute: IO[Int] = IO.delay(throw new RuntimeException("Error occurred"))

  // storing failed computation, instead of throwing them
  val failure: IO[Int] = IO.raiseError(new RuntimeException("Managed failure"))

  // handle exceptions
  val dealWithException: IO[AnyVal] = failure.handleErrorWith { case _: RuntimeException =>
    IO.delay(println("Still working"))
  }

  // turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = failure.attempt

  // redeem: transform the failure and the success in 1 go
  val resultAsString: IO[String] = failure.redeem(thr => s"FAILURE: $thr", value => s"SUCCESS: $value")

  // redeemWith
  val resultAsEffect: IO[Unit] =
    failure.redeemWith(thr => IO(println(s"FAILURE: $thr")), value => IO(println(s"SUCCESS: $value")))

  /** Exercises
    */
  // 1- construct potentially failed IOs from standard data types: Option, Try, Either
  def optionToIO[A](optional: Option[A])(isEmpty: Throwable): IO[A] = {
    optional match {
      case Some(value) => IO.pure(value)
      case None        => IO.raiseError(isEmpty)
    }
    // or use: IO.fromOption()
  }

  def tryToIO[A](aTry: Try[A]): IO[A] = {
    aTry match {
      case Success(value)     => IO.pure(value)
      case Failure(exception) => IO.raiseError(exception)
    }
    // or use: IO.fromTry()
  }

  def eitherToIO[A](either: Either[Throwable, A]): IO[A] = {
    either match {
      case Right(value)    => IO.pure(value)
      case Left(exception) => IO.raiseError(exception)
    }
    // or use: IO.fromEither()
  }

  // 2- handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = {
//    io.redeem(ex => handler(ex), value => value) // or simply:
    io.redeem(handler, identity)
  }

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = {
//    io.redeemWith(ex => handler(ex), value => IO.pure(value)) // or simply:
    io.redeemWith(handler, IO.pure)
  }
// handleErrorWith:
  failure.handleErrorWith(thr => IO.raiseError(thr))
  failure.handleErrorWith(IO.raiseError)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
//    failedCompute.unsafeRunSync()
//    failure.unsafeRunSync()
//    dealWithException.unsafeRunSync()
//    println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()
  }
}
