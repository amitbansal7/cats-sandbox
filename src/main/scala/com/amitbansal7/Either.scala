package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._
import cats.instances.list._
import cats.instances.option._
import cats.Functor
import scala.language.higherKinds

object Either_ {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    import cats.syntax.either._

    val a = 3.asRight[String]
    println(a)

    val b = 4.asRight[String]
    println(b)

    val c = for {
      x <- a
      y <- b
    } yield x * x + y * y

    println(c)

    // def countPositive(nums: List[Int]) =
    //   nums.foldLeft(Right(0)) { (acc, num) =>
    //     if (num > 0)
    //       acc.map(_ + 1)
    //     else Left("Negative!")
    //   }

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (acc, num) =>
        if (num > 0)
          acc.map(_ + 1)
        else Left("Negative!")
      }

    println(countPositive(List(1, 2, 3, 4)))
    println(countPositive(List(1, 2, -3, 4)))

    println(Either.catchOnly[NumberFormatException]("aa".toInt))

    println(Either.catchNonFatal(sys.error("error")))

    println(Either.fromTry(scala.util.Try("foo".toInt)))

    println(Either.fromOption[String, Int](None, "inttt"))

    println("error".asLeft[Int].getOrElse(12))

    println("error".asLeft[Int].orElse(2.asRight[String]))

    println(-1.asRight.ensure("Number must not be -ve")(_ > 0))

    println("error".asLeft[Int].recover {
      case str: String => -1
    })

    println("error".asLeft[Int].recoverWith {
      case str: String => Right(-1)
    })

    println("foo".asLeft[Int].leftMap(_.reverse))

    println(6.asRight[String].bimap(_.reverse, _ * 7))

    println("baar".asLeft[Int].bimap(_.reverse, _ * 7))

    println(123.asRight[String].swap)

    val res = for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "divideBy0".asLeft[Int]
      else (a / b).asRight[String]
    } yield c * 100

    println(res)

    type Result[A] = Either[Throwable, A]

    //****************************
    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String) extends LoginError

    final case class PasswordIncorrect(username: String) extends LoginError

    case object UnexpectedError extends LoginError

    case class User(userame: String, password: String)

    type LoginResult = Either[LoginError, User]
    //****************************

    def handleError(error: LoginError): Unit =
      error match {
        case UserNotFound(username) =>
          println(s"User not found ${username}")

        case PasswordIncorrect(u) =>
          println(s"PasswordIncorrect ${u}")

        case UnexpectedError =>
          println(s"UnexpectedError")
      }

    val res1 = User("amit", "pass").asRight
    println(res1)

    val res2 = UserNotFound("abc").asLeft
    println(res2)

    res1.fold(handleError, println)

    res2.fold(handleError, println)

    //****************************


    import cats.MonadError
    import cats.instances.either._

    type ErrorOr[A] = Either[String, A]

    val monadError = MonadError[ErrorOr, String]

    val success = monadError.pure(42)
    println(success)

    val failure = monadError.raiseError("error!!")
    println(failure)

    val me = monadError.handleError(failure){
      case "error!!" =>
        monadError.pure("Is's ok?")
      case other =>
        monadError.raiseError("It's not ok?")
    }
    println(me)

    println(monadError.ensure(success)("number is too low")(_ > 100))
  }
}