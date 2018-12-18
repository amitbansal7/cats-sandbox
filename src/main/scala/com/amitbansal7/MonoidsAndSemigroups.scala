package com.amitbasnal7

object MonoidsAndSemigroups {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    {
      trait Semigroup[A] {
        def combine(x: A, y: A): A

      }

      trait Monoid[A] extends Semigroup[A] {
        def empty: A
      }

      object Monoid {
        def apply[A](implicit monoid: Monoid[A]) = monoid
      }

      implicit val booleanAndMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean): Boolean = a && b
        def empty = true
      }

      implicit val booleanOrMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean): Boolean = a || b
        def empty = false
      }

      implicit val booleanEitherMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean): Boolean =
          (a && !b) || (!a && b)
        def empty = false
      }

      implicit val booleanXnorMonoid: Monoid[Boolean] = new Monoid[Boolean] {
        def combine(a: Boolean, b: Boolean): Boolean =
          (a || !b) && (!a || b)
        def empty = true
      }

      implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
        def combine(a: Set[A], b: Set[A]) = a union b
        def empty = Set.empty[A]
      }

      implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] = new Semigroup[Set[A]] {
        def combine(a: Set[A], b: Set[A]) = a intersect b
      }

      implicit def setSymDiffMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
        def combine(a: Set[A], b: Set[A]) = (a diff b) union (b diff a)
        def empty: Set[A] = Set.empty[A]
      }
    }

    import cats.Monoid
    import cats.Semigroup
    import cats.instances.string._

    println(Monoid[String].combine("amit ", "bansal"))

    println(Semigroup[String].combine("amit ", "bansal"))

    import cats.instances.int._
    import cats.instances.option._

    println(Monoid[Int].combine(1, 2))

    val a = Option(12)
    val b = Option(13)
    println(Monoid[Option[Int]].combine(a, b))

    import cats.syntax.semigroup._

    println("amit " |+| "bansal")

    println(12 |+| 15 |+| Monoid[Int].empty)

    def add(items: List[Int]): Int =
      items.foldLeft(Monoid[Int].empty)(_ |+| _)

    def add2[A: Monoid](items: List[A]): A =
      items.foldLeft(Monoid[A].empty)(_ |+| _)

    import cats.instances.int._

    add2(List(1, 2, 3, 4))

    import cats.instances.option._

    add2(List(Some(1), Some(2), Some(3), None, Some(5)))

    final case class Order(totalCost: Double, quantity: Double)

    implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
      def combine(a: Order, b: Order): Order =
        Order(
          a.totalCost + b.totalCost,
          a.quantity + b.quantity)

      def empty = Order(0d, 0d)
    }
  }
}

