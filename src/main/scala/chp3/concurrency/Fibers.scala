package chp3.concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{Fiber, IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.language.postfixOps

// 1
object Fibers extends IOApp.Simple {

  // Fiber = description of an effect being executed on some other thread
  // Creating a Fiber is an effectful operation: The Fiber will wrapped in an IO
  // Managing a Fiber is an effectful operation: The Fiber will wrapped in anther IO
  // Fiber is a passive data structure, not like the JVM thread, which is an active entity
  // Cats Effect has a thread pool that manages the execution of effects.
  // Tens of millions of fibers can be created per 1 GB of heap
  // Cats Effect schedules fibers for execution

  /*
  Why we need fibers :
    - no more need for threads and locks
    - delegate thread management to Cats Effect
    - avoid asynchronous code with callbacks
    - maintain pure functional programming
    - keep low-level primitives (blocking, waiting, joining, interrupting, cancelling)

  Fiber scheduling concepts & implementation details:
    - blocking effects in a fiber lead to descheduling
    - semantic blocking: Cats Effect feature; a blocked Fiber doesn't mean that the JVM thread is blocked: no resources are wasted
    - cooperative scheduling: Once a fiber gets descheduled, the JVM thread will take another fiber to process
    - the same fiber can run on multiple JVM threads
    - work-stealing thread pool: For better performance, a free thread can detect if another thread is having too much work, so it can
      some of its work, so this will contribute to a balanced workload between threads
   */


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

  /** Exercises:
    *  1. Write a function that runs an IO on another thread, and, depending on the result of the fiber
    *    - return the result in an IO
    *    - if errored or cancelled, return a failed IO
    *
    * 2. Write a function that takes two IOs, runs them on different fibers and returns an IO with a tuple containing both results.
    *    - if both IOs complete successfully, tuple their results
    *    - if the first IO returns an error, raise that error (ignoring the second IO's result/error)
    *    - if the first IO doesn't error but second IO returns an error, raise that error
    *    - if one (or both) canceled, raise a RuntimeException
    *
    * 3. Write a function that adds a timeout to an IO:
    *    - IO runs on a fiber
    *    - if the timeout duration passes, then the fiber is canceled
    *    - the method returns an IO[A] which contains
    *      - the original value if the computation is successful before the timeout signal
    *      - the exception if the computation is failed before the timeout signal
    *      - a RuntimeException if it times out (i.e. cancelled by the timeout)
    */
  //1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val ioResult = for {
      fiber <- io.debug.start
      result <- fiber.join
    } yield result

    ioResult.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled()    => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testEx1: IO[Unit] = {
    val aComputation = IO("starting").debug >> IO.sleep(1 second) >> IO("done!").debug >> IO(12)
    processResultsFromFiber(aComputation).void
  }

  // 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    val result = for {
      fibA <- ioa.start
      fibB <- iob.start
      resultA <- fibA.join
      resultB <- fibB.join
    } yield (resultA, resultB)

    result.flatMap {
      case (Succeeded(fa), Succeeded(fb)) =>
        for {
          a <- fa
          b <- fb
        } yield (a, b)
      case (Errored(e), _) => IO.raiseError(e)
      case (_, Errored(e)) => IO.raiseError(e)
      case _               => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testEx2: IO[Unit] = {
    val firstIO = IO.sleep(2 seconds) >> IO(1).debug
    val secondIO = IO.sleep(3 seconds) >> IO(2).debug
    tupleIOs(firstIO, secondIO).debug.void
  }

  // 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation = for {
      fib <- io.start
      _ <- IO.sleep(duration) >> fib.cancel
//      _ <- (IO.sleep(duration) >> fib.cancel).start // Be careful: Fibers can leak
      result <- fib.join
    } yield result

    computation.flatMap {
      case Succeeded(fa) => fa
      case Errored(e)    => IO.raiseError(e)
      case Canceled()    => IO.raiseError(new RuntimeException("Computation cancelled"))
    }
  }

  def testEx3: IO[Unit] = {
    val aComputation = IO("starting").debug >> IO.sleep(1 second) >> IO("done!").debug >> IO(12)
    timeout(aComputation, 5 millis).debug.void
  }

  override def run: IO[Unit] = {
//    sameThreadIOs()
//    differentThreadIOs()
//    runOnSomeOtherThread(number) // IO(Succeeded(IO(12)))
//      .debug.void

//    throwOnAnotherThread() // IO(Errored(java.lang.RuntimeException: No number))
//      .debug.void

//    testCancel() // Canceled() // Cancel was called half-way the waiting time, so "done" won't be printed
//      .debug.void

//    testEx1
//    testEx2
    testEx3
  }
}
