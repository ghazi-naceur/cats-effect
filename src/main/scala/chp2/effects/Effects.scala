package chp2.effects

import scala.concurrent.Future

// 1
object Effects {

  // pure functional programming
  // substitution
  def combine(a: Int, b: Int): Int = a + b
  val five: Int = combine(2, 3)
  val fiveV2: Int = 2 + 3 // implementation of 'combine(2, 3)' (a + b)
  val fiveV3: Int = 5 // value
  // => referential transparency: you can substitute/replace 'combine(2, 3)' with the number '5', as many times as you
  // like in your functional code, without changing the behavior of your program

  // In impure functional programming (presence of side effects), the referential transparency is broken, and the substitution
  // won't work
  // example of side effect: print to the console
  val printSomething: Unit = println("Cats effect")
  val printSomethingV2: Unit = () // not the same as 'printSomething'
  // value is unit, but it is not the same as the act of printing something
  // () unit is not equal to println

  // example: changing a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1) // this is the act of changing a variable, not the effect of changing
  val changingVarV2: Unit = () // not the same as 'changingVar'

  // Effect: the concept that bridges the necessity to create a side-effect with our desire to write functional code
  // Effect is a data type that embodies the concept of a side effect or any sort of computation that we might need in our code

  /*
    Effect types

    Properties:
      - type signature describes the kind of calculation that will be performed
      - type signature describes the Value that will be calculated
      - when side effects are needed, effect construction is separate from effect execution
   */

  val anOption: Option[Int] = Option(12)
  /*
    Example: Option is an Effect type
      - describes the possibility of the absence of a value
      - computes a value of type A, if it exists
      - side effects are not needed to construct an option
   */

  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)
  /*
    Example: Future is not a good effect type
      - describes an asynchronous computation that will be performed at some time in the future
      - computes a value of type A, if it's successful
      - side effect is required (allocating/scheduling a thread), and execution is not separate from construction
   */

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }
  /*
    Example: MyIO from Monad lesson: It is an effect type
      - describes any computation that might produce a side effect
      - calculates a value of type A, if it's successful
      - side effects are required for the evaluation of '() => A', and the creation of MyIO does not produce the side
        effects on construction
   */

  val anIO: MyIO[Unit] = MyIO(() => {
    println("This is a statement") // this side effect won't be printed unless we call 'unsafeRun: () => A' directly.
    // In other terms, unless we call this val in the main method, like follows: 'anIO.unsafeRun()'
    12
  })

  def main(args: Array[String]): Unit = {
    anIO.unsafeRun()
  }
}
