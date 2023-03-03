package chp3.concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

// 6
object AsyncIOs extends IOApp.Simple {

  // IOs can run asynchronously on fibers, without having to manually manage the fiber lifecycle
  val threadPool: ExecutorService = Executors.newFixedThreadPool(8)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeSomething(): Int = {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computing something on some other thread...")
    12
  }
  def computeSomethingEither(): Either[Throwable, Int] = Try { computeSomething() }.toEither

  def computeSomethingOnThreadPool(): Unit = {
    threadPool.execute(() => computeSomethingEither())
  }
  /*
  Problem:
  The delegation ot the scheduling of 'computeSomething' to run on 'threadPool' (calling 'computeSomethingOnThreadPool')
  returns a unit: This is a side effect computation, and we cannot really create IO effect that will get a hold of the result
  of this computation, because it happens on another thread that's not managed by Cats-Effect. The goal here is to lift
  the result of this computation 'computeSomething' into an IO, that we can after that compose with other IOs
  Solution:
  We can use 'IO.async_'
   */

  // lift async computation to an IO
  val asyncComputeSomething: IO[Int] = IO.async_ { callback: Callback[Int] =>
    // 'IO.async_' will block (semantically) the effect until the 'callback' is evaluated or invoked from some other thread
    // (though some notification mechanism)
    threadPool.execute { () =>
      val result = computeSomethingEither()
      callback(result) // notifying the Cats-Effect thread that the computation has been completed
    }
  }

  /** Exercise: Generalize the previous example
    */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { callback: Callback[A] =>
      ec.execute { () =>
        val result = Try(computation()).toEither
        callback(result)
      }
    }

  val asyncSomethingV2: IO[Int] = asyncToIO(computeSomething)(ec)

  /** Exercise: Lifting a Future to an IO
    */

  // ': =>' evaluation by name, because we don't want to start the evaluation of the future before passing it as an argument
  def convertFutureToIO[A](future: => Future[A]): IO[A] =
    IO.async_ { callback: Callback[A] =>
      future.onComplete { tryResult =>
        val result = tryResult.toEither
        callback(result)
      }
    }

  lazy val computeSomethingFuture: Future[Int] = Future { computeSomething() }(ec)

  val asyncSomethingV3: IO[Int] = convertFutureToIO(computeSomethingFuture)
  val asyncSomethingV4: IO[Int] = IO.fromFuture(IO(computeSomethingFuture))

  /** Exercise: Defining a never-ending IO
    */
  val neverEndingIO: IO[Int] = IO.async_[Int] { _ => () } // no callback, no finish
  val neverEndingIOV2: IO[Int] = IO.never

  /*
    Full async call: 'IO.async' (dealing with cancellation)
   */
  def demoAsyncCancellation() = {
    val asyncSomethingIOV2: IO[Int] = IO.async { callback: Callback[Int] =>
      // 1- Defining a finalizer in case computation gets cancelled
      // 2- Finalizers are of type 'IO[Unit]'
      // 3- Not specifying a finalizer means that you need 'Option[IO[Unit]]'
      // 4- Create this Option is an effect, so we need an IO => 'IO[Option[IO[Unit]]]'
      // 5- return IO[Option[IO[Unit]]]
      IO {
        threadPool.execute { () =>
          val result = computeSomethingEither()
          callback(result)
        }
      } // IO[Unit]
        .as(Some(IO("cancelled!").debug.void)) // IO[Option[IO[Unit]]]
    }
    for {
      fib <- asyncSomethingIOV2.start
      _ <- IO.sleep(1 millis) >> IO("cancelling...").debug >> fib.cancel
      _ <- fib.join
    } yield ()
  }

  override def run: IO[Unit] = {
//    asyncComputeSomething.debug >> IO(threadPool.shutdown())
    /*
    Result:
      [pool-1-thread-1] computing something on some other thread...
      [io-compute-7] 12
    => The result was computed by thread '[pool-1-thread-1]' managed by the thread pool (not CE), and then the result was
    returned thanks to the callback to the CE thread '[io-compute-7]'
    => Through 'IO.async_', we've been able somehow to pull a result that was computed on some thread that was not managed by
    CE, onto an IO effect that is managed by CE: Bringing an asynchronous computation from another thread pool into Cats-Effect
    => IO.async_ is a Foreign Function Interface (FFI)
     */

//    asyncSomethingV2.debug >> IO(threadPool.shutdown())
//    asyncSomethingV3.debug >> IO(threadPool.shutdown())
//    asyncSomethingV4.debug >> IO(threadPool.shutdown())
    demoAsyncCancellation().debug >> IO(threadPool.shutdown())
    /*
    Result:
        [io-compute-10] cancelling...
        [io-compute-10] cancelled!
        [io-compute-10] ()
        [pool-1-thread-1] computing something on some other thread...

        => IO is properly cancelled, but there is no way from the CE runtime context to cancel/interrupt '[pool-1-thread-1]'
        which runs on a thread outside the CE runtime context. If you want to interrupt this thread (interrupt it outside
        CE runtime context), you may want to use 'Thread.interrupted()'
     */
  }
}
