package com.amitbansal7

object WriterMonad {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code = {

    import cats.data.Writer
    import cats.instances.vector._

    val vec = Writer(Vector(
      "It was the best of times",
      "It was the worst of times"), 1859)

    println(vec)

    type Logged[A] = Writer[Vector[String], A]

    import cats.syntax.applicative._
    println(123.pure[Logged])

    import cats.syntax.writer._

    println(Vector("msg1", "msg2", "msg3").tell)

    val a = Writer(Vector("msg1", "msg2", "msg3"), "some res")
    println(a)

    val b = "some ress".writer(Vector("1", "2"))
    println(b)

    println(a.value)

    println(b.written)

    println(a.run)

    val w1 = for {
      a <- 1.pure[Logged]
      _ <- Vector("1", "2", "3").tell
      b <- 42.writer(Vector("a", "b", "c"))
    } yield a + b

    println(w1.run)

    val w2 = for {
      a <- 1.pure[Logged]
      _ <- Vector("1", "2", "3").tell
      b <- 42.writer(Vector("a", "b", "c"))
    } yield a

    println(w2)

    println(w1.mapWritten(_.map(_.toUpperCase)))

    println(w1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 10))

    println(w1.mapBoth { (log, res) =>
      (log.map(_+"!"), res * 10)
    })

    println(w1.reset)

    println(w1.swap)

    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    def fact(n: Int): Int = {
      val res = slowly(if (n == 1) 1 else n * fact(n - 1))
      println(s"fact ${n} ${res}")
      res
    }

    fact(5)

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    Await.result(Future.sequence(Vector(
      Future(fact(5)),
      Future(fact(5)))), 5.seconds)

    def factWriter(n: Int): Logged[Int] = {
      for {
        res <- if (n == 1) 1.pure[Logged]
        else slowly(factWriter(n - 1).map(_ * n))

        _ <- Vector(s"fact ${n} $res").tell
      } yield res
    }

    println(factWriter(5))

    val fw = Await.result(Future.sequence(Vector(
      Future(factWriter(5)),
      Future(factWriter(5)))), 5.seconds)

    println(fw)

  }
}
