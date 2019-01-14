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


    //Incomplete - 5, 3.6, 6

  }
}