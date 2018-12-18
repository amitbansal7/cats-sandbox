package com.amitbansal7
import cats._
import cats.implicits._
import cats.instances.string._
import cats.syntax.show._
import java.util.Date
import cats.syntax.show._
import cats.instances.int._

object MeetCats {

  // def main(args: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code() = {

    // val showInt = Show.apply[Int]
    // println(showInt.show(123))
    // println(123.show)

    final case class Cat(name: String, age: Int, color: String)

    // implicit val dateShow: Show[Date] =
    //   new Show[Date] {
    //     def show(date: Date): String =
    //       s"${date.getTime} ms since epoch"
    //   }

    implicit val catShow = Show.show[Cat] { cat =>
      s"${cat.name} is ${cat.age} years old and color is ${cat.color}"
    }

    import cats.Show
    import cats.instances.int._ // for Show
    import cats.instances.string._ // for Show
    import cats.syntax.show._

    println(Cat("123", 12, "white").show)

    import cats.Eq
    import cats.syntax.eq._

    val data = List(1, 2, 3).map(Option(_)).filter(item => item =!= Some(1))

    println(123 =!= 2)

    import cats.syntax.option._

    println(1.some === none[Int])

    import cats.instances.long._

    implicit val dateEq: Eq[Date] = Eq.instance[Date] { (d1, d2) =>
      d1.getTime === d2.getTime
    }

    val d1 = new Date()
    val d2 = new Date()

    println(d1 === d2)
    println(d1 =!= d2)

    implicit val catEq = Eq.instance[Cat] { (c1, c2) =>
      c1.name === c2.name
    }

    val cat1 = Cat("Garfield", 38, "orange and black")
    val cat2 = Cat("Heathcliff", 33, "orange and black")

    val optionCat1 = Option(cat1)
    val optionCat2 = Option.empty[Cat]


    println(cat1 === cat2)
    println(cat1 =!= cat2)

  }

}