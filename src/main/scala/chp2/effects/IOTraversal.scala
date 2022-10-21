package chp2.effects

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random

// 6
object IOTraversal extends IOApp.Simple {

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workLoad: List[String] = List("This is a cats-effect course", "This is a tutorial", "Scala course")

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workLoad.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._
  val listTraverse: Traverse[List] = Traverse[List]

  def traverseFutures(): Unit = {
    // traverse
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workLoad)(heavyComputation)
    // This stores all the results
    singleFuture.foreach(println)
  }

  // traverse for IO
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workLoad.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workLoad)(computeAsIO) // computeAsIO(element) = IO[Int]

  // parallel traversal
  import cats.syntax.parallel._ // parTraverse extension method
  val parallelSingleIO: IO[List[Int]] = workLoad.parTraverse(computeAsIO)

  /** Exercises
    */
  // Use Traverse API
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listTraverse.traverse(listOfIOs)(x => x) // lambda(elem) = IO[A]

  // Hard version
  def sequenceV2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(wrapperOfIOs)(x => x)

  // Parallel version
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] =
    listOfIOs.parTraverse(x => x)

  // Parallel hard version
  def parSequenceV2[F[_]: Traverse, A](wrapperOfIOs: F[IO[A]]): IO[F[A]] =
    wrapperOfIOs.parTraverse(x => x)

  // Existing sequence API
  val singleIOV2: IO[List[Int]] = listTraverse.sequence(ios)

  // Parallel sequencing
  val parallelSingleIOV2: IO[List[Int]] = parSequence(ios)
  // Using the extension method from the parallel syntax package
  val parallelSingleIOV3: IO[List[Int]] = ios.parSequence

  override def run: IO[Unit] = {
//    singleIO.debug.void
//    singleIO.map(_.sum).debug.void
//    parallelSingleIO.debug.void
//    parallelSingleIO.map(_.sum).debug.void
    parallelSingleIOV3.debug.void
  }
}
