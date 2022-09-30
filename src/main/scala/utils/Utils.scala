package utils

import cats.effect.IO

//class Utils {}
// Extension method using Scala 3
//extension [A](io: IO[A])
//  def debug: IO[A] = for {
//    a <- io
//    t = Thread.currentThread().getName
//    _ = println(s"[$t] $a")
//  } yield a