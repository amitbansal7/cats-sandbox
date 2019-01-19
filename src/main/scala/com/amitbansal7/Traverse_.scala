package com.amitbansal7

object Traverse_ {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code = {

    import scala.concurrent.duration._
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global

    val hosts = List(
      "amitbansal7.com",
      "hey@amitbansal7.com",
      "example.com")

    def getUptime(hostname: String): Future[Int] =
      Future(hostname.length * 60)

    val allUptimes: Future[List[Int]] =
      hosts.foldLeft(Future(List.empty[Int])) { (acc, host) =>
        val uptime = getUptime(host)
        for {
          acc <- acc
          uptime <- uptime
        } yield (acc :+ uptime)
      }

    println(Await.result(allUptimes, 1.second))

    println(Future.traverse(hosts)(getUptime))

    import cats.Applicative
    import cats.instances.future._
    import cats.syntax.applicative._
    import scala.language.higherKinds
    import cats.syntax.apply._

    def listTraverse[F[_]: Applicative, A, B](ls: List[A])(f: A => F[B]): F[List[B]] =
      ls.foldLeft(List.empty[B].pure[F]) { (acc, i) =>
        (acc, f(i)).mapN(_ :+ _)
      }
  }
}