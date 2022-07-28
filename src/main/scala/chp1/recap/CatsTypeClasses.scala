package chp1.recap

object CatsTypeClasses {

  /*
    - functor
    - applicative (extends functor)
    - flatMap (extends functor)
    - monad : applicative + flatMap
    - apply
    - applicativeError/monadError
    - traverse
   */

  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._
  val listFunctor: Functor[List] = Functor[List]

  // generalizable "mapping" api
  def increment[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  // functor extension methods
  import cats.syntax.functor._
  def increment2[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1) // .map is an extension method already provided by Functor

  // applicative - the ability to "wrap" types
  trait MyApplicative[F[_]] extends MyFunctor[F] {
    def pure[T](value: T): F[T]
  }

  import cats.Applicative
  val applicativeList: Applicative[List] = Applicative[List]
  val simpleList: List[Int] = applicativeList.pure(12)

  import cats.syntax.applicative._ // importing the 'pure' extension method on any type
  val simpleList2: List[Int] = 12.pure[List]

  // flatMap - ability to chain multiple computation
  trait MyFlatMap[F[_]] extends MyFunctor[F] {
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList: FlatMap[List] = FlatMap[List]

  import cats.syntax.flatMap._ // flatMap extension method
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // monad - combination of applicative and flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] =
      flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList: Monad[List] = Monad[List]

  def crossProduct2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // Error-like type classes
  // 1- ApplicativeError
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  type ErrorOr[A] = Either[String, A]

  val appErrorEither: ApplicativeError[ErrorOr, String] = ApplicativeError[ErrorOr, String]
  val desirableValue: ErrorOr[Int] = appErrorEither.pure(2)
  val failedValue: ErrorOr[Int] = appErrorEither.raiseError("Error occurred")

  import cats.syntax.applicativeError._ // importing raiseError extension method
  val failedValueV2: ErrorOr[Int] = "Something failed".raiseError[ErrorOr, Int]

  // 2- MonadError
  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with Monad[F]

  import cats.MonadError
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

  // traverse: turning nested wrappers inside out
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_], A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  val listOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  import cats.Traverse
  val listTraverse: Traverse[List] = Traverse[List]
  val optionList: Option[List[Int]] = listTraverse.traverse(List(1, 2, 3))(x => Option(x))

  import cats.syntax.traverse._
  val optionListV2: Option[List[Int]] = List(1, 2, 3).traverse(x => Option(x))

  def main(args: Array[String]): Unit = {}
}
