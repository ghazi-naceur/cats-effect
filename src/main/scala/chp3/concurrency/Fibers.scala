package chp3.concurrency

import cats.effect.kernel.Outcome
import cats.effect.{Fiber, IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt

// 1
object Fibers extends IOApp.Simple {

  val number: IO[Int] = IO.pure(12)
  val string: IO[String] = IO.pure("simple")

  def sameThreadIOs(): IO[Unit] = for {
    num <- number.debug
    str <- string.debug
  } yield ()

  // Fiber: similar to the concept of thread in the jvm, but lightweight compared to a thread
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually

  // The fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = number.debug.start

  def differentThreadIOs(): IO[Unit] = for {
    _ <- aFiber
    _ <- string.debug
  } yield ()

  // Joining a fiber: waiting for a fiber to finish in pure functional way
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join // an effect which waits for the fiber to terminate
  } yield result
  /*
  IO[ResultType of fib.join]
  fib.join = Outcome[IO, Throwable, A]

  Possible outcomes:
    - success with an IO
    - failure with an exception
    - cancelled
   */

  val someIOOnAnotherThread: IO[Outcome[IO, Throwable, Int]] = runOnSomeOtherThread(number)
  val someResultFromAnotherThread: IO[Int] = someIOOnAnotherThread.flatMap {
    case Outcome.Succeeded(effect)  => effect
    case Outcome.Errored(exception) => IO(0)
    case Outcome.Canceled()         => IO(0)
  }

  def throwOnAnotherThread(): IO[Outcome[IO, Throwable, Int]] = for {
    fib <- IO.raiseError[Int](new RuntimeException("No number")).start
    result <- fib.join
  } yield result

  def testCancel(): IO[Outcome[IO, Throwable, String]] = {
    val task = IO("Starting").debug >> IO.sleep(1.second) >> IO("Done").debug
    val taskWithCancellationHandler = task.onCancel(IO("Cancellation in process...").debug.void)
    // => a finalizer when task gets cancelled (similar somehow to a shutdownhook). We can use it to release resources/ports/io...

    for {
      fib <- taskWithCancellationHandler.start // run on a separate thread
      _ <- IO.sleep(500.millis) >> IO("Cancelling").debug // run on the calling thread
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  override def run: IO[Unit] = {
//    sameThreadIOs()
//    differentThreadIOs()
//    runOnSomeOtherThread(number) // IO(Succeeded(IO(12)))
//      .debug.void

//    throwOnAnotherThread() // IO(Errored(java.lang.RuntimeException: No number))
//      .debug.void

    testCancel() // Canceled() // Cancel was called half-way the waiting time, so "done" won't be printed
      .debug.void

  }
}
