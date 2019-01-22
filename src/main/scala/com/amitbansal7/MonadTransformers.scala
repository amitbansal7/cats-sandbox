package com.amitbansal7

object MonadTransformers {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code = {
    import cats.data.OptionT

    type ListOption[A] = OptionT[List, A]

    import cats.Monad
    import cats.instances.list._
    import cats.syntax.applicative._

    val res1 = OptionT(List(Option(12)))
    val res2 = 32.pure[ListOption]

    println(res1)
    println(res2)

    println(res1.flatMap(x => res2.map(y => x + y)))

    import scala.concurrent.Future
    import cats.data.{ EitherT, OptionT }

    // case class EitherT[F[_], E, A](stack: F[Either[E, A]])

    type FutureEither[A] = EitherT[Future, String, A]

    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import cats.instances.future._
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val futureEitherOr: FutureEitherOption[Int] = for {
      a <- 12.pure[FutureEitherOption]
      b <- 10.pure[FutureEitherOption]
    } yield (a + b)

    println(futureEitherOr)

    import cats.instances.either._
    import cats.instances.option._

    // val err1 = OptionT[ErrorOr, Int](Right(Some(10)))
    // println(err1)

    // val err2 = 42.pure[ErrorOrOption]
    // println(err2)

    import cats.data.Writer

    type Logged[A] = Writer[List[String], A]

    def parseNumbers(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case Some(v) => Writer(List(s"Read $str"), Some(v))
        case None    => Writer(List(s"Failed on $str"), None)
      }

    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
      import cats.data.OptionT

      val res = for {
        a <- OptionT(parseNumbers(a))
        b <- OptionT(parseNumbers(b))
        c <- OptionT(parseNumbers(c))
      } yield (a + b + c)

      res.value
    }

    println(addAll("1", "2", "3"))
    println(addAll("1", "error", "3"))

    import cats.data.EitherT
    import scala.concurrent.Future

    type Response[A] = EitherT[Future, String, A]

    val powerLevels = Map(
      "Jazz" -> 6,
      "Bumblebee" -> 8,
      "Hot Rod" -> 10)

    def getPowerLevel(autobot: String): Response[Int] =
      powerLevels.get(autobot) match {
        case Some(lvl) => EitherT.right(Future(lvl))
        case None      => EitherT.left(Future(s"unreachable $autobot"))
      }

    def canSpecialMove(a1: String, a2: String): Response[Boolean] =
      for {
        a <- getPowerLevel(a1)
        b <- getPowerLevel(a2)
      } yield (a + b) > 15

    def tacticalReport(a1: String, a2: String): String = {
      val res = canSpecialMove(a1, a2).value

      Await.result(res, 1.second) match {
        case Right(true)  => s"$a1, $a2 true"
        case Right(false) => s"$a1 $a2 false"
        case Left(log)    => s"error: $log"
      }
    }

    println(tacticalReport("Jazz", "Hot Rod"))
    println(tacticalReport("Jazz", "Bumblebee"))
    println(tacticalReport("Bumblebee", "fake"))
  }
}