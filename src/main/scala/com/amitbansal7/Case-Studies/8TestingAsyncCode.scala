package com.amitbansal7


object TestingAsyncCode {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code() = {

    import scala.concurrent.Future
    import cats.instances.future._
    import cats.instances.list._
    import cats.syntax.traverse._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.language.higherKinds
    import cats.Id
    import cats.Applicative
    import cats.syntax.functor._

    trait UptimeClient[F[_]]{
      def getUptime(hostname: String):F[Int]
    }

    trait RealUptimeClient extends UptimeClient[Future]{
      def getUptime(hostname: String):Future[Int]
    }

    trait TestUptimeClient extends UptimeClient[Id]{
      def getUptime(hostname: String):Int
    }

    class UptimeService[F[_]: Applicative](client: UptimeClient[F]){
      def getTotalUptime(hostnames: List[String]):F[Int] =
        hostnames.traverse(client.getUptime).map(_.sum)
    }

    class TestUptimeClientC(hosts: Map[String, Int]) extends TestUptimeClient{
      def getUptime(hostname: String):Int =
        hosts.getOrElse(hostname, 0)
    }

    def testTotalUptime() = {
      val hosts = Map("1" -> 10, "2" -> 4)
      val client = new TestUptimeClientC(hosts)
      val service = new UptimeService(client)
      val act = service.getTotalUptime(hosts.keys.toList)
      val expec = hosts.values.sum
      println(assert(act == expec))
    }

    testTotalUptime()
  }

}
