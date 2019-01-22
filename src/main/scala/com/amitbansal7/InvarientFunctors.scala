package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._

object InvarientFunctor {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    trait Codec[A] {
      def encode(value: A): String

      def decode(value: String): A

      def imap[B](dec: A => B, enc: B => A): Codec[B] = {

        val self = this

        new Codec[B] {

          def encode(value: B): String =
            self.encode(enc(value))

          def decode(value: String): B =
            dec(self.decode(value))

        }
      }
    }

    def encode[A](value: A)(implicit codec: Codec[A]): String =
      codec.encode(value)

    def decode[A](value: String)(implicit codec: Codec[A]): A =
      codec.decode(value)

    implicit val stringCodec: Codec[String] = new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

    implicit val intCodec: Codec[Int] =
      stringCodec.imap(_.toInt, _.toString)

    implicit val boolCodec: Codec[Boolean] =
      stringCodec.imap(_.toBoolean, _.toString)

    implicit val doubleCodec: Codec[Double] =
      stringCodec.imap[Double](_.toDouble, _.toString)

    case class Box[A](value: A)

    implicit def boxCodec[A](implicit aCodec: Codec[A]): Codec[Box[A]] =
      aCodec.imap[Box[A]](Box(_), _.value)

    println(encode(123.4))
    println(decode[Double]("123.4"))
    println(decode[Box[Double]]("123.4"))

    import cats.Monoid
    import cats.instances.string._
    import cats.syntax.invariant._
    import cats.syntax.semigroup._

    implicit val symbolMonoid: Monoid[Symbol] =
      Monoid[String].imap(Symbol.apply)(_.name)

    println(Monoid[Symbol].empty)
    println('a |+| 'b |+| 'c)

  }
}