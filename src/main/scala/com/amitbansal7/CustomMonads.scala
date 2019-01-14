package com.amitbansal7

object CustomMonads {
  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code = {

    import cats.Monad

    val optionMonad = new Monad[Option] {
      def flatMap[A, B](opt: Option[A])(f: A => Option[B]): Option[B] =
        opt.flatMap(f)

      def pure[A](a: A): Option[A] =
        Some(a)

      import scala.annotation.tailrec
      @tailrec
      def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
        f(a) match {
          case None           => None
          case Some(Left(a))  => tailRecM(a)(f)
          case Some(Right(b)) => Some(b)
        }
    }

    sealed trait Tree[+A]

    final case class Branch[A](left: Tree[A], right: Tree[A])
      extends Tree[A]

    final case class Leaf[A](value: A) extends Tree[A]

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)

    implicit val treeMonad = new Monad[Tree] {
      def flatMap[A, B](node: Tree[A])(f: A => Tree[B]): Tree[B] = node match {
        case Leaf(l)      => f(l)
        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }

      def pure[A](a: A) = Leaf(a)

      def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???
    }

  }
}