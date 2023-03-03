package chp3.concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

// 5
object BlockingIOs extends IOApp.Simple {

  val someSleeps: IO[Unit] = for {
    _ <- IO.sleep(1 second).debug
    _ <- IO.sleep(1 second).debug
  } yield ()
  // 'Semantic' blocking IOs: running in 2 different threads, because each '.sleep' holds to a fiber
  // Semantic blocking: The thread won't be blocked, but the thread will be scheduled to do the computation after 1 second

  val aBlockingIO: IO[Int] = IO.blocking {
    Thread.sleep(1000)
    println(s"[${Thread.currentThread().getName}] computed a blocking code")
    12
  }
  // 'Really' blocking IOs: will evaluate on a thread from another thread pool specific for blocking calls

  // Yielding control over a thread:
  val iosOnManyThreads: IO[Unit] = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread: This is a hint for the Cats Effect runtime to start
    // running the rest of this for-comprehension on a different thread (equivalent to 'IO.shift' in Cats-Effect 2)
    _ <- IO("second").debug // the rest of this effect may run on another thread (not necessarily)
    _ <- IO.cede
    _ <- IO("third").debug
  } yield ()
  // If I don't put 'IO.cede', all the previous computations will be done in the same thread sequentially
  // In this example, we may find that all computations are being executed on the same thread, because Cats-Effect may
  // perform some optimizations, since the 'cede' calls are few

  val thousandCedes: IO[Int] = (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug)
  // The Cats-Effect optimization may done here as well, since CE has a smart thread pool
  // The thread-switching may occur when defining another thread pool:

  def testThousandsEffectsSwitch(): IO[Int] = {
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
    // afterwards, we need to close the execution context manually, because Cats-Effect will detects the other threads
    // that are outside of the scope of Cats-Effect.
    // Here we're going to call the execution context only when we can the method 'testThousandsEffectsSwitch'
  }

  // Blocking calls and IO.sleep perform both semantic blocking and yield control over calling the thread automatically

  override def run: IO[Unit] = {
//    someSleeps
//    aBlockingIO.void
//    iosOnManyThreads
//    thousandCedes.void
    testThousandsEffectsSwitch().void // thread-switching
  }
}
