package chp2.effects

import cats.effect.IO

import scala.io.StdIn

// 2
object IOIntroduction {

  // IO embodies any computation that can perform side-effect.
  // IO is like a description of some computation

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

  def main(args: Array[String]): Unit = {

    // How to start unsafe run on IOs ?
    import cats.effect.unsafe.implicits.global
    // only in this place, we're running side effects
    println(aDelayedIO.unsafeRunSync())
    println(smallProgram().unsafeRunSync())
    println(smallProgram2().unsafeRunSync())
  }
}
