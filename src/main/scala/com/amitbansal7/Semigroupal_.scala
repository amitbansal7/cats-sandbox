package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._

object Semigroupal_ {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code() = {

    import cats.Semigroupal
    import cats.instances.option._

    println(Semigroupal[Option].product(Some(123), Some("abc")))

    println(Semigroupal[Option].product(None, Some(123)))

    println(Semigroupal.tuple3(Option(1), Option(2), Option(3)))

    println(Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int]))

    println(Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _))

    println(Semigroupal.map2(Option(1), Option.empty[Int])(_ + _))

    import cats.instances.option._
    import cats.syntax.apply._

    println((Option(123), Option("abc")).tupled)

    println((Option(123), Option("abc"), Option(true)).tupled)

    {
      case class Cat(name: String, born: Int, color: String)

      println((Option("name"), Option(2000), Option("white")).mapN(Cat.apply))
    }

    import cats.Monoid
    import cats.instances.int._
    import cats.instances.invariant._
    import cats.instances.list._
    import cats.instances.string._
    import cats.syntax.apply._

    case class Cat(name: String, birth: Int, favFoods: List[String])

    val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _

    val catToTuple: Cat => (String, Int, List[String]) = cat => (cat.name, cat.birth, cat.favFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]).imapN(tupleToCat)(catToTuple)

    import cats.syntax.semigroup._

    val one = Cat("one", 2000, List("food1"))
    val two = Cat("two", 2010, List("food2"))

    println(one |+| two)

    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.language.higherKinds

    val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123))

    println(Await.result(futurePair, 1 second))

    import cats.syntax.apply._

    val futureCat = (
      Future("name"),
      Future(123),
      Future(List("food1"))).mapN(Cat.apply)

    println(Await.result(futureCat, 1 second))

    import cats.instances.list._

    println(Semigroupal[List].product(List(1, 2), List(3, 4, 5)))

    import cats.instances.either._

    {
      type ErrorOr[A] = Either[Vector[String], A]

      println(
        Semigroupal[ErrorOr].product(
          Left(Vector("error1")),
          Left(Vector("error2"))))
    }

    import cats.Monad

    def product[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
      x.flatMap(a => y.map(b => (a, b)))

    import cats.data.Validated

    type AllErrorOr[A] = Validated[List[String], A]

    println(
      Semigroupal[AllErrorOr].product(
        Validated.invalid(List("error1")),
        Validated.invalid(List("error2"))))

    println(Validated.Valid(123))

    println(Validated.Invalid(List("errrors")))

    println(Validated.valid[List[String], Int](123))

    println(Validated.invalid[List[String], Int](List("errors")))

    import cats.syntax.validated._

    println(123.valid[List[String]])

    println(List("eroor").invalid[Double])

    import cats.syntax.applicative._
    import cats.syntax.applicativeError._

    type ErrorOr[A] = Validated[List[String], A]

    println(123.pure[ErrorOr])

    println(List("errorrr").raiseError[ErrorOr, Int])

    println(Validated.catchOnly[NumberFormatException]("no".toInt))

    type AllErrorOrA[A] = Validated[String, A]

    println((
        "err1".invalid[Int],
        "err2".invalid[Int]).tupled)

    import cats.instances.vector._

    println((
        Vector(404).invalid[Int],
        Vector(500).invalid[Int]).tupled)

    import cats.data.NonEmptyVector

    println((
      NonEmptyVector.of("Err1").invalid[Int],
      NonEmptyVector.of("Err1").invalid[Int],
    ).tupled)


    println(123.valid.map(_*100))

    println("?".invalid.leftMap(_+"?"))

    println(123.valid[String].bimap(_ + "!", _ * 2))

    println{
      32.valid.andThen{ a =>
        42.valid.map{ b =>
          a+b
        }
      }
    }

    import cats.syntax.either._

    println("erros".invalid[Int].toEither)

    println("erros".invalid[Int].toEither.toValidated)

    println("error".invalid[Int].getOrElse(-1))

    println("error".invalid[Int].fold(_+"rr", _.toString))

    //6.4.4

    case class User(name: String, age:Int)

    def getValue(key: String)(data: Map[String, String]): Either[List[String], String] =
      data.get(key) match{
        case Some(d) => Right(d)
        case None => Left(List(s"${key} doesn't exist"))
      }

    def parseInt(name: String)(data: String): Either[List[String], Int] =
      Either
        .catchOnly[NumberFormatException](data.toInt)
        .leftMap(_ => List(s"$name must be an integer"))

    def nonBlank(name: String)(data: String):Either[List[String], String] =
      Right(data).ensure(List(s"${name} cannot be blank"))(_.nonEmpty)

    def nonNegative(name: String)(data: Int):Either[List[String], Int] =
      Right(data).ensure(List(s"${name} cannot be <= 0"))(_ > 0)

    def readName(data: Map[String, String]):Either[List[String], String] =
      getValue("name")(data).flatMap(nonBlank("name"))


    def readAge(data: Map[String, String]):Either[List[String], Int] =
      getValue("age")(data)
        .flatMap(nonBlank("age"))
        .flatMap(parseInt("age"))
        .flatMap(nonNegative("age"))


    def readUser(data: Map[String, String]):Validated[List[String], User] =
      (
        readName(data).toValidated,
        readAge(data).toValidated
      ).mapN(User.apply)

    println(readUser(Map("name"->"amit", "age"->"20")))
    println(readUser(Map("name"->"", "age"->"-1")))
    println(readUser(Map("age"->"-1s")))
    println(readUser(Map("age"->"-1")))
  }
}
