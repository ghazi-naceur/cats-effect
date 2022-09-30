package chp2.effects

import cats.Parallel
import cats.effect.IO.Par
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple {

  // IOs are usually sequential
  val isaacIO: IO[String] = IO(s"[${Thread.currentThread().getName}] Isaac")
  val beyondIO: IO[String] = IO(s"[${Thread.currentThread().getName}] Beyond")

  val composedIO: IO[String] = for {
    isaac <- isaacIO
    beyond <- beyondIO
  } yield s"$isaac and $beyond are hunters"

  // debug extension method
  import utils._
  val someInt: IO[Int] = IO.delay(12)
  val someString: IO[String] = IO.delay("Scala")

  import cats.syntax.apply._
  val composed: IO[String] =
    (someInt.debug, someString.debug).mapN((valInt, valString) => s"int: $valInt, string: $valString")

  // parallelism
  // convert a sequential IO to parallel IO
  val parallelIO1: IO.Par[Int] = Parallel[IO].parallel(someInt.debug)
  val parallelIO2: IO.Par[String] = Parallel[IO].parallel(someString.debug)

  import cats.effect.implicits._
  val composedParallel: IO.Par[String] =
    (parallelIO1, parallelIO2).mapN((valInt, valString) => s"int: $valInt, string: $valString")
  // turning 'IO.Par'(parallel) to 'IO'(sequential) to execute 'run' method:

  val composedParallelBackToSequential: IO[String] = Parallel[IO].sequential(composedParallel)

  // shorthand:
  import cats.syntax.parallel._
  val shortcut: IO[String] = {
    (someInt.debug, someString.debug).parMapN((valInt, valString) => s"int: $valInt, string: $valString")
    // 'parMapN' is a parallel version of 'mapN', so no need to specifically instantiate values using 'IO.Par'.
    // This will be done in the background after calling 'parMapN'.. Each input and the output will be evaluated in a distinct thread
  }

  // regarding failure:
  val failure: IO[String] = IO.raiseError(new RuntimeException("Error occurred"))
  // composing success with failure
  val mixedParallel: IO[String] = (someInt.debug, failure.debug).parMapN(_ + _)

  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second error occurred"))

  val twoFailures: IO[String] = (failure.debug, anotherFailure.debug).parMapN(_ + _)

  val twoFailuresDelayed: IO[String] = (failure.debug, IO(Thread.sleep(1000)) >> anotherFailure.debug).parMapN(_ + _)

  override def run: IO[Unit] = {
//    composedIO.map(println)
//    composed.map(println) // running in the same thread: sequential
//    composedParallelBackToSequential.map(println) //or:
//    composedParallelBackToSequential.debug.void
//    shortcut.debug.void

//    mixedParallel.debug.void // number is shown, then the exception has been thrown, so no final result
//    twoFailures.debug.void // only 1 exception will be shown: the first to fail
    twoFailuresDelayed.debug.void // we delayed the second failure, so that the first failure will be thrown first
    // The first effect to fail will give the failure to the result
  }
}
