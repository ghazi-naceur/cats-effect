package chp4.coordination

import cats.effect.{IO, IOApp, Ref}
import utils.DebugWrapper

// 1
object Refs extends IOApp.Simple {

  // Ref = purely functional atomic reference
  val atomicVal_v1: IO[Ref[IO, Int]] = Ref[IO].of(12)
  val atomicVal_v2: IO[Ref[IO, Int]] = IO.ref(12)

  // Modifying is an effect:
  val increasedVal: IO[IO[Unit]] = atomicVal_v1.map { ref =>
    ref.set(24)
  }
  // => using flatmap to get rid off double IOs:
  val flattenedIncreasedVal: IO[Unit] = atomicVal_v1.flatMap { ref =>
    ref.set(24) // Thread-safe because we're an atomic reference
  }

  // Obtain a value:
  val mol: IO[Int] = atomicVal_v1.flatMap { ref =>
    ref.get // Thread-safe
  }

  // Get and set in 1 transaction:
  val getAndSet: IO[Int] = atomicVal_v1.flatMap { ref =>
    ref.getAndSet(56) // getting old value and storing new one
  }

  // Updating with a function:
  val fVal: IO[Unit] = atomicVal_v1.flatMap { ref =>
    ref.update(value => value * 10)
  }

  // Updating and getting a value in 1 transaction:
  val updatedfVal: IO[Int] = atomicVal_v1.flatMap { ref =>
    ref.updateAndGet(value => value * 10) // Getting the new value
  }

  // Getting and updating the old value in 1 transaction:
  val getAndUpdateVal: IO[Int] = atomicVal_v1.flatMap { ref =>
    ref.getAndUpdate(value => value * 10)
  }

  // Modifying with a function returning a different type
  val modifiedVal = atomicVal_v1.flatMap { ref =>
    ref.modify(value => (value * 10, s"My current value is $value"))
  }

  // Ref is useful to perform concurrent + thread-safe reads/writes over shared values, in a purely functional way
  def demoConcurrentWorkImpure(): IO[Unit] = {

    import cats.syntax.parallel._

    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").debug
        _ <- IO(count += wordCount)
      } yield ()
    }

    List("Cats effect", "Isaac Netero", "This is a cat effect course")
      .map(task)
      .parSequence // similar to a parallel traverse .. import cats.syntax.parallel._
      .void
  }
  /*
  Drawbacks:
    - hard to read/write
    - mix pure/impure code
    - not thread-safe
   */

  def demoConcurrentWorkPure(): IO[Unit] = {
    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {

      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for '$workload': $wordCount").debug
        newCount <- total.updateAndGet(currentValue => currentValue + wordCount)
        _ <- IO(s"New total: $newCount").debug
      } yield ()
    }

    import cats.syntax.parallel._
    for {
      initialCount <- Ref[IO].of(0)
      _ <- List("Cats effect", "Isaac Netero", "This is a cat effect course")
        .map(word => task(word, initialCount))
        .parSequence
    } yield ()
  }

  override def run: IO[Unit] = {
//    demoConcurrentWorkImpure()
    // => This example is not thread-safe, so it won't give an correct result

    demoConcurrentWorkPure()
  }
}
