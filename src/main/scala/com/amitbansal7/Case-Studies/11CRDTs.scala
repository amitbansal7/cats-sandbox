package com.amitbansal7

import cats.Semigroup
import cats.instances.list._
import cats.syntax.semigroup._
import cats.syntax.either._

object CRDTs {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code() = {

    {
      final case class GCounter(counters: Map[String, Int]) {

        def inc(machine: String, amount: Int) = {
          val current = counters.getOrElse(machine, 0)
          GCounter(counters + (machine -> (current + amount)))
        }

        def merge(that: GCounter): GCounter = {
          GCounter(that.counters ++ this.counters.map {
            case (k, v) => k -> (v.max(that.counters.getOrElse(k, 0)))
          })
        }

        def total: Int = counters.values.sum
      }
    }

    import cats.kernel.CommutativeMonoid
    import cats.instances.list._
    import cats.instances.map._
    import cats.syntax.semigroup._
    import cats.syntax.foldable._

    trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
      def combine(a: A, B: A): A
      def empty: A
    }

    object BoundedSemiLattice {
      implicit val intInstance: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
        def combine(a: Int, b: Int): Int = a max b
        def empty: Int = 0
      }

      implicit def setInstance[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
        def combine(a: Set[A], b: Set[A]): Set[A] = a union b
        def empty: Set[A] = Set.empty[A]
      }
    }

    {
      final case class GCounter[A](counters: Map[String, A]) {

        def inc(machine: String, amount: A)(implicit m: CommutativeMonoid[A]): GCounter[A] = {
          val current = counters.getOrElse(machine, m.empty)
          GCounter(counters + (machine -> (current |+| amount)))
        }

        def merge(that: GCounter[A])(implicit m: BoundedSemiLattice[A]): GCounter[A] = {
          GCounter(that.counters |+| this.counters)
        }

        def total(implicit m: CommutativeMonoid[A]): A =
          this.counters.values.toList.combineAll
      }
    }

    import cats.instances.map._
    import cats.syntax.semigroup._

    trait GCounter[F[_, _], K, V] {
      def inc(f: F[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): F[K, V]

      def merge(a: F[K, V], b: F[K, V])(implicit m: BoundedSemiLattice[V]): F[K, V]

      def total(f: F[K, V])(implicit m: CommutativeMonoid[V]): V
    }

    object GCounter {
      def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter
    }

    implicit def mapIntance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
      def inc(map: Map[K, V])(k: K, v: V)(implicit m: CommutativeMonoid[V]): Map[K, V] = {
        val current = map.getOrElse(k, m.empty)
        map + (k -> (current |+| v))
      }

      def merge(m1: Map[K, V], m2: Map[K, V])(implicit m: BoundedSemiLattice[V]): Map[K, V] =
        m1 |+| m2

      def total(map: Map[K, V])(implicit m: CommutativeMonoid[V]): V = {
        map.values.toList.combineAll
      }

    }

    import cats.instances.int._
    val one = Map("a" -> 7, "b" -> 3)
    val two = Map("a" -> 2, "b" -> 5)

    val counter = GCounter[Map, String, Int]

    val merged = counter.merge(one, two)
    println(counter.total(merged))

    trait KeyValueStore[F[_, _]] {
      def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

      def get[K, V](f: F[K, V])(k: K): Option[V]

      def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
        get(f)(k).getOrElse(default)

      def values[K, V](f: F[K, V]): List[V]
    }

    implicit val mapIntanceKV: KeyValueStore[Map] = new KeyValueStore[Map] {

      def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] =
        f + (k -> v)

      def get[K, V](f: Map[K, V])(k: K): Option[V] =
        f.get(k)

      override def getOrElse[K, V](f: Map[K, V])(k: K, default: V): V =
        f.getOrElse(k, default)

      def values[K, V](f: Map[K, V]): List[V] =
        f.values.toList
    }

  }
}