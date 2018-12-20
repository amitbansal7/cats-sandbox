package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._
import cats.instances.list._
import cats.instances.option._
import cats.Functor
import scala.language.higherKinds

object Functors {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    import cats.instances.list._
    import cats.instances.option._
    import cats.Functor
    val res = Functor[List].map(List(1, 2, 3))(_ * 2)
    println(res)

    val func = ((a: Int) => a + 1).map((a: Int) => a * 2).map((a: Int) => a+"!")
    println(func(123))

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(_ * 2 + 1)

    println(doMath(Option(123)))
    println(doMath(List(1, 2, 3, 4)))

    //3.5.4 Exercise: Branching out with Functors
    import cats.Functor
    sealed trait Tree[+A]

    object Tree{
      def branch[A](l: Tree[A], r:Tree[A]):Tree[A] =
        Branch(l, r)

      def leaf[A](v: A):Tree[A] =
        Leaf(v)
    }

    final case class Branch[A](left: Tree[A], right: Tree[A])
      extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        tree match {
          case Branch(l, r) => Branch(map(l)(f), map(r)(f))
          case Leaf(d)      => Leaf(f(d))
        }
    }

    println(Tree.branch(Tree.leaf(2), Tree.leaf(3)).map(_*2))

  }
}