package chp3.concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration._
import scala.language.postfixOps

// 2
object Resources extends IOApp.Simple {

  // Use case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"Opening connection to $url").debug
    def close: IO[String] = IO(s"Closing connection to $url").debug
  }

  val asynFetchUrl: IO[Unit] = for {
    fib <- (new Connection("google.com").open *> IO.sleep(Int.MaxValue seconds)).start
    _ <- IO.sleep(1 second) *> fib.cancel
  } yield ()
  // => Problem: leaking resources, because connection is not closed

  val correctAsynFetchUrl: IO[Unit] = for {
    connection <- IO(new Connection("google.com"))
    fib <- (connection.open *> IO.sleep(Int.MaxValue seconds)).onCancel(connection.close.void).start
    _ <- IO.sleep(1 second) *> fib.cancel
  } yield ()

  // Bracket pattern by Cats Effect
  // someIO.bracket(useResourceCode)(releaseResourceCode)
  val bracketFetchUrl: IO[Unit] = IO(new Connection("google.com"))
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue seconds))(conn => conn.close.void)

  val bracketProgram: IO[Unit] = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1 second) *> fib.cancel
  } yield ()

  /** Exercise:
    * - Open a scanner
    * - Read the file line by line each 100 millis
    * - Close the scanner
    * - If cancelled/throws error: close the scanner
    */
  def openFileScanner(path: String): IO[Scanner] =
    IO(new Scanner(new FileReader(new File(path))))

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine)
      IO(scanner.nextLine()).debug >> IO.sleep(100 millis) >> readLineByLine(scanner)
    // If we try to eagerly (*>) sequence IOs here, we will have a stackoverflow, that's why we need to use the lazy impl (>>)
    else IO.unit


  def bracketReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path").debug >> openFileScanner(path).bracket { scanner =>
      readLineByLine(scanner)
    } { scanner =>
      IO(s"closing file at $path").debug >> IO(scanner.close())
    }

  override def run: IO[Unit] = {
//    correctAsynFetchUrl.void
//    bracketProgram.void
    bracketReadFile("src/main/scala/chp3/concurrency/Resources.scala")
  }
}
