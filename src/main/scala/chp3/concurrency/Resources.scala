package chp3.concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp, Resource}
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

  /** Resources:
    */
  def connectionFromConfig(path: String): IO[Unit] =
    openFileScanner(path).bracket { scanner =>
      //acquire a connection based on the file
      IO(new Connection(scanner.nextLine())).bracket { conn =>
        conn.open >> IO.never // '.never' is an IO that never terminates
      }(conn => conn.close.void)
    }(scanner => IO("closing file").debug >> IO(scanner.close()))
  //nested '.bracket', nested resources are tedious
  //solution: Resource
  val connectionResource: Resource[IO, Connection] =
    Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close.void)

  val resourceFetchUrl: IO[Unit] = for {
    fib <- connectionResource.use(conn => conn.open >> IO.never).start
    _ <- IO.sleep(1 second) >> fib.cancel
  } yield ()

  // Resources are equivalent to Brackets
  val simpleResource: IO[String] = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the string: $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"finalizing the string: $string").debug.void

  val usingResourceWithBracket: IO[String] = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource: IO[String] = Resource.make(simpleResource)(releaseResource).use(usingResource)

  /** Exercise:
    * Read a text file with one line every 100 millis, using Resource
    * (refactor the bracket exercise to use Resource)
    */

  def getResourceFromFile(path: String): Resource[IO, Scanner] = Resource.make(openFileScanner(path)) { scanner =>
    IO(s"closing file at $path").debug >> IO(scanner.close())
  }

  def resourceReadFile(path: String): IO[Unit] =
    IO(s"opening file at $path") >>
      getResourceFromFile(path).use { scanner => readLineByLine(scanner) }

  def cancelReadFile(path: String): IO[Unit] = for {
    fib <- resourceReadFile(path).start
    _ <- IO.sleep(2 seconds) >> fib.cancel
  } yield ()

  // Nested Resources by flatMap
  def connFromConfigResource(path: String): Resource[IO, Connection] =
    Resource
      .make(IO("opening file").debug >> openFileScanner(path))(scanner =>
        IO("closing file").debug >> IO(scanner.close())
      )
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void))

  def connFromConfigResourceClean(path: String): Resource[IO, Connection] = for {
    scanner <- Resource
      .make(IO("opening file").debug >> openFileScanner(path))(scanner =>
        IO("closing file").debug >> IO(scanner.close())
      )
    conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void)
  } yield conn

  val openConnection = connFromConfigResource("src/main/resources/connection.txt").use(conn => conn.open >> IO.never)

  val openConnectionClean =
    connFromConfigResourceClean("src/main/resources/connection.txt").use(conn => conn.open >> IO.never)
  /*
  'connFromConfigResource("src/main/scala/chp3/concurrency/Resources.scala")' is a Resource of Connection, closure and
  the releasing of all the dependant resources will happen automatically:
  connection + file: will close automatically
   */

  val cancelledConnection: IO[Unit] = for {
    fib <- openConnection.start
    _ <- IO.sleep(1 second) >> IO("cancelling!").debug >> fib.cancel
  } yield ()

  val cancelledConnectionClean: IO[Unit] = for {
    fib <- openConnectionClean.start
    _ <- IO.sleep(1 second) >> IO("cancelling!").debug >> fib.cancel
  } yield ()

  // Finalizers to regular IOs
  val ioWithFinalizer: IO[String] = IO("Some resource").debug.guarantee(IO("freeing resource").debug.void)
  // with 'guarantee' takes an IO[Unit] that will performed regardless of weather 'IO("Some resource")' will finish
  // successfully, or it fails or even gets cancelled

  val ioWithFinalizerV2: IO[String] = IO("Some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"releasing resource: $result").debug).void
    case Errored(e) => IO("Nothing to release").debug.void
    case Canceled() => IO("Resource got cancelled, releasing what's left").debug.void
  }

  override def run: IO[Unit] = {
//    correctAsynFetchUrl.void
//    bracketProgram.void
//    bracketReadFile("src/main/scala/chp3/concurrency/Resources.scala")
//    resourceFetchUrl.void
//    resourceReadFile("src/main/scala/chp3/concurrency/Resources.scala").void
//    cancelReadFile("src/main/scala/chp3/concurrency/Resources.scala").void // after cancelling, release clause is called
//    openConnection.void // closing connection then closing file, once processing is done
//    cancelledConnection
//    cancelledConnectionClean
//    ioWithFinalizer.void
    ioWithFinalizerV2.void
  }
}
