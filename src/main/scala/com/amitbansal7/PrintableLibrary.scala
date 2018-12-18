package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._

object PrintableLibrary {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() {
    trait Printable[A] {
      def format(value: A): String
    }

    object PrintableInstances {
      implicit val intPrintable: Printable[Int] = new Printable[Int] {
        def format(value: Int): String = value.toString
      }

      implicit val stringPrintable: Printable[String] = new Printable[String] {
        def format(value: String): String = value
      }
    }

    import PrintableInstances._

    object Printable {
      def format[A](value: A)(implicit printable: Printable[A]): String =
        printable.format(value)

      def print[A](value: A)(implicit printable: Printable[A]): Unit =
        println(format(value))
    }

    final case class Cat(name: String, age: Int, color: String)

    implicit val catPrintable = new Printable[Cat] {
      def format(cat: Cat) = {
        s"${Printable.format(cat.name)}:${Printable.format(cat.age)}:${Printable.format(cat.color)}"
      }
    }

    val cat = Cat("123", 12, "white")

    object PrintableSyntax {
      implicit class PrintableOps[A](value: A) {
        def format(implicit p: Printable[A]) =
          Printable.format(value)

        def print(implicit p: Printable[A]) =
          Printable.print(value)
      }
    }

    import PrintableSyntax._
    cat.print
  }

}