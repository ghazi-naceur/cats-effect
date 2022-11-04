package chp3.concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{FiberIO, IO, IOApp, OutcomeIO}
import utils.DebugWrapper
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

// 3
object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] =
    (
      IO(s"Starting computation: $value").debug >>
        IO.sleep(duration) >>
        IO(s"Computation for $value is done") >>
        IO(value)
    )
      .onCancel(IO(s"Computation cancelled for '$value'").debug.void)

  def testRace(): IO[String] = {
    val number = runWithSleep(12, 1 second)
    val string = runWithSleep("Scala", 2 seconds)
    val first: IO[Either[Int, String]] = IO.race(number, string)
    /*
    Both IOs run on separate fibers
      - The first one to finish will = complete the result
      - The loser will be cancelled
     */

    first.flatMap {
      case Left(num)  => IO(s"The value '$num' was faster")
      case Right(str) => IO(s"The value '$str' was faster")
    }
  }

  // Race is managing fibers automatically.
  // In case we want to manage fibers manually, we can use '.racePair' that allows to handle the fiber of the loosing
  // effect, so you can cancel for example, or do whatever you want in that case.

  def testRacePair() = {
    val number = runWithSleep(12, 1 second)
    val string = runWithSleep("Scala", 2 seconds)
    val raceResult: IO[Either[(OutcomeIO[Int], FiberIO[String]), (FiberIO[Int], OutcomeIO[String])]] =
      IO.racePair(number, string)
    // 'racePair' returns the Outcome of the winner IO and the Fiber of the looser IO
    // 'racePair' gives much more control
    raceResult flatMap {
      case Left((outputNumber, fiberString))  => fiberString.cancel >> IO("Number won").debug >> IO(outputNumber).debug
      case Right((fiberNumber, outputString)) => fiberNumber.cancel >> IO("String won").debug >> IO(outputString).debug
    }
  }

  /** Exercises:
    * 1- Implement a timeout pattern with race
    * 2- Implement a method to return the loosing effect from a race (hint: use racePair)
    * 3- Implement race in terms of racePair
    */

  // 1
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeoutEffect: IO[Unit] = IO.sleep(duration)
    val result: IO[Either[A, Unit]] = IO.race(io, timeoutEffect)
    result flatMap {
      case Left(value) => IO(value)
      case Right(_)    => IO.raiseError(new RuntimeException("Computation timed out"))
    }
  }

  val someTask: IO[Int] = IO.sleep(2 seconds) >> IO(12).debug
  val testTimeout: IO[Int] = timeout(someTask, 1 second)
  val testTimeoutV2: IO[Int] = someTask.timeout(3 seconds) // 'timeout' provided by Cats

  // 2
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob) flatMap {
      case Left((_, fiberB)) =>
        fiberB.join.flatMap {
          case Succeeded(resultEffect) => resultEffect.map(result => Right(result))
          case Errored(e)              => IO.raiseError(e)
          case Canceled()              => IO.raiseError(new RuntimeException("Looser cancelled"))
        }
      case Right((fiberA, _)) =>
        fiberA.join.flatMap {
          case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
          case Errored(e)              => IO.raiseError(e)
          case Canceled()              => IO.raiseError(new RuntimeException("Looser cancelled"))
        }
    }

  def testUnrace(): IO[String] = {
    val number = runWithSleep(12, 1 second)
    val string = runWithSleep("Scala", 2 seconds)
    val first: IO[Either[Int, String]] = unrace(number, string)

    first.flatMap {
      case Left(num)  => IO(s"The value '$num' was slower")
      case Right(str) => IO(s"The value '$str' was slower")
    }
  }

  // 3
  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((outputA, fiberB)) =>
        outputA match {
          case Succeeded(effectA) => fiberB.cancel >> effectA.map(a => Left(a))
          case Errored(e)         => fiberB.cancel >> IO.raiseError(e)
          case Canceled() =>
            fiberB.join.flatMap { // B becomes winner, as A got cancelled
              case Succeeded(effectB) => effectB.map(b => Right(b))
              case Errored(e)         => IO.raiseError(e)
              case Canceled()         => IO.raiseError(new RuntimeException("Both A and B computations got cancelled"))
            }
        }
      case Right((fiberA, outputB)) =>
        outputB match {
          case Succeeded(effectB) => fiberA.cancel >> effectB.map(b => Right(b))
          case Errored(e)         => fiberA.cancel >> IO.raiseError(e)
          case Canceled() =>
            fiberA.join.flatMap { // A becomes winner, as B got cancelled
              case Succeeded(effectA) => effectA.map(a => Left(a))
              case Errored(e)         => IO.raiseError(e)
              case Canceled()         => IO.raiseError(new RuntimeException("Both A and B computations got cancelled"))
            }
        }
    }

  override def run: IO[Unit] = {
//    testRace().debug.void
//    testRacePair().void
//    timeout(IO.sleep(2 seconds) >> IO(12).debug, 1 second).void
//    testTimeout.void
//    testTimeoutV2.void
//    testUnrace().debug.void

    val number = runWithSleep(12, 1 second)
    val string = runWithSleep("Scala", 2 seconds)
    simpleRace(number, string).debug.void
  }
}
