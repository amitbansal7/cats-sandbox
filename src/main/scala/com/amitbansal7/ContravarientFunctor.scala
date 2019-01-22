package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._

object ContravarientFunctor {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    trait Printable[A] {
      self =>

      def format(value: A): String

      def contramap[B](f: B => A): Printable[B] = new Printable[B] {
        def format(value: B): String = self.format(f(value))
      }

    }

    def format[A](value: A)(implicit p: Printable[A]): String =
      p.format(value)

    implicit val stringPrintabel: Printable[String] =
      new Printable[String] {
        def format(value: String): String =
          "\""+value+"\""
      }

    implicit val boolPrintable: Printable[Boolean] =
      new Printable[Boolean] {
        def format(value: Boolean): String =
          if (value) "true"
          else "false"
      }

    println(format("amit"))
    println(format(true))

    final case class Box[A](value: A)

    // implicit def boxPrintable[A](implicit p: Printable[A]) =
    //   new Printable[Box[A]] {
    //     def format(box: Box[A]): String =
    //       p.format(box.value)
    //   }

    implicit def boxPrintable2[A](implicit p: Printable[A]) =
      p.contramap[Box[A]](_.value)

    println(format(Box("some val")))

    println(format(Box(true)))

    println(Box(true))

    import cats.Contravariant
    import cats.Show
    import cats.instances.string._

    val showString = Show[String]

    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) => s"'${sym.name}")

    println(showSymbol.show('amit))

    import cats.syntax.contravariant._

    println(showString.contramap[Symbol](_.name).show('amit))
  }

}
