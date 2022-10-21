package chp2.effects

import cats.effect.IO

import java.security.Principal
import scala.io.StdIn

// 2
object IOIntroduction {

  // IO embodies any computation that can perform side-effect.
  // IO is like a description of some computation
  // IO is a bridge between :
  //  - pure functional programming & referential transparency
  //  - impure FP/imperative programming & side effects
  // Effect properties:
  //  - it describes what kind of computation will be performed
  //  - the type signature describes the value that will be calculated
  //  - it separates effect description from effect execution (when externally visible effects are produced)

  // Cats Effect IO:
  // The ultimate effect type:
  //  - any computation that might perform side effects
  //  - produces a value of type A if it's successful
  //  - the effect construction is separate from the effect execution

  // Expressions and methods returning IOs are called effectful
  // IO is a monad
  // IO compositions read like an imperative program + pure FP is preserved

  // IO handles errors and can be transformed to also hold failures to process later (.attempt)

  // IO parallelism:
  //  - effects are evaluated on different threads
  //  - synchronization and coordination are automatic

  // IO traversal:
  //  - useful when we want to 'unwrap' double-nested containers
  //  - can be done in parallel

  val ourFirstIO: IO[Int] = IO.pure(12) // pure contains an arg that should not have side effect
  val aDelayedIO: IO[Int] = IO.delay({
    println("producing an integer")
    13
  }) // delay takes an arg passed by name (=>)
  // the code won't be evaluated until we call some unsafe run method on this IO datatype

  val shouldNotDoTHis: IO[Int] = IO.pure {
    println("producing an integer eagerly")
    13
  }

  // If you're not sure that your expression does produce a side-effect, just use .delay

  // Another way to write 'IO.delay' is to use 'IO.apply' (or simply calling 'IO {...}') method, because IO is supposed
  // to suspend any expression that may produce a side effect
  val aDelayedIO2: IO[Int] = IO {
    println("producing an integer eagerly")
    13
  }

  // map, flatMap
  val improvedMeaningOfLife: IO[Int] = ourFirstIO.map(_ * 2)
  val printedMeaningOfLife: IO[Unit] = ourFirstIO.flatMap(mol => IO.delay(println(mol)))

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife).mapN(_ + _)
  def smallProgram2(): IO[Unit] = (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**   1- Sequence 2 IOs and take the result of the last one
    *   use flatMap
    */
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa.flatMap(_ => iob)
  def sequenceTakeLast2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // '*>': 'andThen' eager call
  def sequenceTakeLast3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // '>>': 'andThen with by-name lazy call'

  /**  2 - Sequence 2 IOs and take the result of the first one
    *  use flatMap
    */
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))
  def sequenceTakeFirst2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob

  /**  3 - repeat IO effect forever
    *  use flatMap + recursion
    */
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))
  def forever2[A](io: IO[A]): IO[A] =
    io >> forever2(io) // io andThen forever2(io): lazy call
  def forever3[A](io: IO[A]): IO[A] = {
    io *> forever3(io) // io andThen forever3(io): eager call
    // this continuous eager call will cause stackoverflow
  }

  def forever4[A](io: IO[A]): IO[A] =
    io.foreverM // with tail recursion: provided by cats

  /**  4 - convert an IO to a different type
    *   use map
    */
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)
  def convert2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value)

  /**  5 - discard value inside an IO, just return Unit
    */
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ())
  def asUnit2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // discouraged
  def asUnit3[A](ioa: IO[A]): IO[Unit] =
    ioa.void // provided by cats

  /**  6 - fix stack recursion
    */
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else
      for {
        lastNumber <- IO(n)
        prevSum <- sumIO(n - 1)
      } yield prevSum + lastNumber
  // This implementation is based on for-comprehension, therefore it's based on flatMap chains, so it's stack-safe and
  // it will crash with stackoverflow

  /** 7- write a fibonacci IO that does not crush on recursion
    * Use recursion, ignore exponential complexity and use flatMap heavily
    */
  def fibonacci(n: Int): IO[BigInt] = {
    if (n < 2) IO(1)
    else {
      for {
        last <- IO.defer(fibonacci(n - 1)) //== IO(fibonacci(n - 1)).flatten == IO.delay(fibonacci(n - 1)).flatten
        prev <- IO.defer(fibonacci(n - 2)) //== IO(fibonacci(n - 2)).flatten == IO.delay(fibonacci(n - 1)).flatten
      } yield last + prev
    }
    // stack safety + avoiding stackoverflow
  }

  def main(args: Array[String]): Unit = {

    // How to start unsafe run on IOs ?
    import cats.effect.unsafe.implicits.global
    // only in this place, we're running side effects
//    println(aDelayedIO.unsafeRunSync())
//    println(smallProgram().unsafeRunSync())
//    println(smallProgram2().unsafeRunSync())

//    forever(IO {
//      println("hello")
//      Thread.sleep(500)
//    }).unsafeRunSync()
//
//    forever2(IO {
//      println("hello2")
//      Thread.sleep(500)
//    }).unsafeRunSync()

//    forever3(IO {
//      println("hello2")
//      Thread.sleep(500)
//    }).unsafeRunSync()

    println(sumIO(20000).unsafeRunSync()) // this won't crash the jvm, because it's based on IO
//    sum(20000) // This will crash the jvm

    println(fibonacci(10).unsafeRunSync())

    (1 to 100).foreach(i => println(fibonacci(i).unsafeRunSync()))
  }
}
