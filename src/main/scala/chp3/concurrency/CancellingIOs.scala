package chp3.concurrency

import cats.effect.{IO, IOApp}
import utils.DebugWrapper

import scala.concurrent.duration.DurationInt
import scala.language.postfixOps

// 4
object CancellingIOs extends IOApp.Simple {

  /*
  Cancelling IOs
    - fib.cancel
    - IO.race & other APIs
    - manual cancellation:
      val chainOfIOs: IO[Int] = IO("waiting").debug >> IO.canceled >> IO(12).debug
      // 'IO(12)' won't be reached, because a manual cancellation 'IO.canceled' has been called before it
   */

  // Uncancellable
  // Example: Online store, payment processor
  // Payment process must not be cancelled
  val specialPaymentSystem: IO[String] = (
    IO("Payment running, don't cancel it").debug >>
      IO.sleep(1 second) >>
      IO("Payment completed").debug
  ).onCancel(IO("Error happened").debug.void)

  val cancellation: IO[Unit] = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500 millis) >> fib.cancel
    _ <- fib.join
  } yield ()

  // Preventing cancellation: uncancelable == masking
  val atomicPayment: IO[String] = IO.uncancelable(_ => specialPaymentSystem) // "masking"
  // Cats Effect will check what kind of effect is 'specialPaymentSystem' (IO.pure, IO.cancel ...etc)
  // When 'atomicPayment' will be evaluated, Cats Effect runtime will know that this effect is not able to be cancellable
  // due to '.uncancelable', so Cats Effect runtime will never run a cancellation operation on 'specialPaymentSystem',
  // even if it's requested by other fiber

  val atomicPaymentV2: IO[String] = specialPaymentSystem.uncancelable // same

  val noCancellation: IO[Unit] = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500 millis) >> IO("Attempting cancellation...").debug >> fib.cancel
    _ <- fib.join
  } yield ()
  // => With "uncancelable", you can suppress cancellation signals coming from other fibers

  /*
  The "uncancelable" API is more complex and more general.
  It takes a function from Poll[IO] to IO. In the example above, we aren't using that Poll instance.
  The Poll object can be used to mark sections within the returned effect which CAN BE CANCELLED.
   */

  /** Example: Authentication service that has 2 parts:
    *  - input password, can be cancelled, because otherwise we might block indefinitely on user input
    *  - verify password, CANNOT be cancelled once it's started
    */
  val inputPassword: IO[String] =
    IO("Input password:").debug >> IO("(typing password)").debug >> IO.sleep(5 seconds) >> IO("some_password")

  val verifyPassword: String => IO[Boolean] = (password: String) =>
    IO("verifying...").debug >> IO.sleep(2 seconds) >> IO(password == "some_password")

  val authFlow: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- inputPassword.onCancel(IO("Authentication timed out. Try again later.").debug.void)
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful").debug else IO("Authentication failed").debug
    } yield ()
  }

  val authProgram: IO[Unit] = for {
    authFib <- authFlow.start
    _ <- IO.sleep(3 seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  // => The cancellation won't work here, because the 'authFlow' in wrapped inside 'IO.uncancelable'

  val authFlowWithPoll: IO[Unit] = IO.uncancelable { poll =>
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timed out. Try again later.").debug.void)
      // We're unmasking 'inputPassword' using 'poll', which makes this line cancellable again
      verified <- verifyPassword(pw) // This is not cancellable
      _ <-
        if (verified) IO("Authentication successful").debug
        else IO("Authentication failed").debug // This is not cancellable
    } yield ()
  }

  val authProgramWithPoll: IO[Unit] = for {
    authFib <- authFlowWithPoll.start
    _ <- IO.sleep(3 seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  // => The cancellation works work here, because the 'authFlowWithPoll' is using Poll to make things cancellable again

  // => "poll" is the opposite of "uncancelable"
  /*
  'uncancelable' calls are MASKS which suppress cancellation.
  'poll' calls are "gaps opened" in the 'uncancelable' region.
   */

  /** Exercises:
    */

//    1-
  val cancelBeforeMol: IO[Int] = IO.canceled >> IO(12).debug
  val uncancelBeforeMol: IO[Int] = IO.uncancelable(_ => IO.canceled >> IO(12).debug)
  // => 'uncancelable' suppress all 'cancel' calls/points whether it's from the current fiber or from other fibers

//    2-
  val invincibleAuthProgram: IO[Unit] = for {
    authFib <- IO.uncancelable(_ => authFlowWithPoll).start
    _ <- IO.sleep(3 seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  // => 'uncancelable' will fix the gap created by the 'poll' set in 'authFlowWithPoll', which makes the code uncancelable
  // one more time

  val vincibleAuthProgram: IO[Unit] = for {
    authFib <- IO.uncancelable(poll => poll(authFlowWithPoll)).start
    _ <- IO.sleep(3 seconds) >> IO("Authentication timeout, attempting cancel...").debug >> authFib.cancel
    _ <- authFib.join
  } yield ()
  // => In this case, 'uncalcelable' and 'poll' will cancel each other out, and we'll get the behavior of 'authFlowWithPoll'
  // as a result, so whatever its output (cancelled or not depending on the IO.sleep set)

//      3-
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("first cancelable").debug >> IO.sleep(1 second) >> IO("first cancelable end").debug) >>
        IO("uncancelable").debug >> IO.sleep(1 second) >> IO("uncancelable end").debug >>
        poll(IO("second cancelable").debug >> IO.sleep(1 second) >> IO(" second cancelable end").debug)
    }
//     You will notice that uncancelable section (2nd line) will complete successfully before the cancel action will start
//     in the cancellable section (line 3) (you won't see the logs lines for the second cancellable section, because the
//     cancellation took place in that cancelable section)
//    => The cancellation signal will be handled by the first cancelable region, if you have one

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500 millis) >> IO("cancelling").debug >> fib.cancel
      _ <- fib.join
    } yield ()

  }

  override def run: IO[Unit] = {
//    cancellation
//    noCancellation
//    authFlow
//    authProgram
//    authProgramWithPoll

//    cancelBeforeMol.void
//    uncancelBeforeMol.void
//    invincibleAuthProgram.void
//    vincibleAuthProgram.void
    threeStepProgram()
  }
}
