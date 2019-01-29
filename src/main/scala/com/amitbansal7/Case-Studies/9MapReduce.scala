package com.amitbansal7

object MapReduce {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code() = {

    import cats.{ Monoid, Monad }
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.semigroup._
    import cats.instances.list._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    def foldMap[A, B: Monoid](seq: Vector[A])(f: A => B): B =
      seq.foldLeft(Monoid[B].empty)(_ |+| f(_))

    println(foldMap(Vector(1, 2, 3))(identity))

    println(foldMap(Vector(1, 2, 3))(_.toString+"!"))

    println(foldMap("Vector(1,2,3)".toVector)(_.toString.toUpperCase))

    def parallelForldMap[A, B: Monoid](vals: Vector[A])(f: A => B): Future[B] = {
      val cores = Runtime.getRuntime.availableProcessors

      val size = (1.0 * vals.size / cores).ceil.toInt

      val groups = vals.grouped(size)

      val futs = groups.map { group =>
        Future(foldMap(group)(f))
      }

      // .sequence : List[Future[B]] to Future[List[B]]
      Future.sequence(futs).map { it =>
        it.foldLeft(Monoid[B].empty)(_ |+| _)
      }
    }

    val res = parallelForldMap((1 to 1000000).toVector)(identity)

    println(Await.result(res, 1 seconds))

  }
}