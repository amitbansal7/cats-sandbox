package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._
import cats.instances.list._
import cats.instances.option._
import cats.Functor
import scala.language.higherKinds

object Monads {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    trait Monad_[F[_]] {
      def pure[A](a: A): F[A]

      def flatMap[A, B](value: F[A])(f: A => F[B]): F[B]

      def map[A, B](value: F[A])(f: A => B): F[B] =
        flatMap(value)(a => pure(f(a)))
    }

    import cats.Monad
    import cats.instances.option._ // for Monad
    import cats.instances.list._ // for Monad

    val opt1 = Monad[Option].pure(3)
    println(opt1)

    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    println(opt2)

    val opt3 = Monad[Option].map(opt2)(a => 100 * a)
    println(opt3)

    val list1 = Monad[List].pure(2)
    println(list1)

    val list2 = Monad[List].flatMap(List(1, 2, 3, 4))(a => List(a, a * 10))
    println(list2)

    val list3 = Monad[List].map(list2)(a => a + 123)
    println(list3)

    import cats.Monad
    import cats.syntax.functor._
    import cats.syntax.flatMap._
    import cats.instances.option._
    import cats.instances.list._

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
      a.flatMap(x => b.map(y => x * x + y * y))

    println(sumSquare(Option(3), Option(4)))
    //Some(25)

    println(sumSquare(List(1, 2, 3, 4), List(2, 3)))
    //List(5, 10, 8, 13, 13, 18, 20, 25)

  }
}