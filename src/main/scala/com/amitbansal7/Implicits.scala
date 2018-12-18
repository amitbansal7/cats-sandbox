package com.amitbansal7

import cats._
import cats.data._
import cats.implicits._

object Implicits {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {
    sealed trait Json
    final case class JsObject(get: Map[String, Json]) extends Json
    final case class JsString(get: String) extends Json
    final case class JsNumber(get: Double) extends Json
    case object JsNull extends Json

    trait JsonWriter[A] {
      def write(value: A): Json
    }

    final case class Person(name: String, email: String)

    object JsonWriterInstances {
      implicit val stringWriter: JsonWriter[String] =
        new JsonWriter[String] {
          def write(value: String): Json =
            JsString(value)
        }

      implicit val personWriter: JsonWriter[Person] =
        new JsonWriter[Person] {
          def write(person: Person): Json =
            JsObject(Map(
              "name" -> JsString(person.name),
              "email" -> JsString(person.email)))
        }
    }

    object Json {
      def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }

    import JsonWriterInstances._

    object JsonSyntax {
      implicit class JsonStringOps[A](value: A) {
        def toJson(implicit w: JsonWriter[A]): Json =
          w.write(value)
      }
    }

    import JsonSyntax._

    println(Person("amit", "amit@gmail.com").toJson)

    println(implicitly[JsonWriter[Person]])

    implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
      new JsonWriter[Option[A]] {
        def write(option: Option[A]): Json = option match {
          case Some(value) => writer.write(value)
          case None        => JsNull
        }
      }

    println(implicitly[JsonWriter[Option[Person]]])

  }
}