package com.amitbansal7

object Foldable_ {

  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }

  def code = {

    //7.1.2

    println(List(1, 2, 3).foldLeft(List.empty[Int])((acc, i) => i :: acc))

    println(List(1, 2, 3).foldRight(List.empty[Int])((i, acc) => i :: acc))
    //**


    //7.1.3

    def map[A, B](ls: List[A])(f: A => B): List[B] =
      ls.foldLeft(List.empty[B])((acc, i) => f(i) :: acc)

    println(map(List(2, 4, 6))(_ * 2))

    def flatMap[A, B](ls: List[A])(f: A => List[B]): List[B] =
      ls.foldLeft(List.empty[B])((acc, i) => f(i) ::: acc)

    println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))

    def filter[A](ls: List[A])(f: A => Boolean): List[A] =
      ls.foldLeft(List.empty[A]) { (acc, i) =>
        if (f(i)) i :: acc
        else acc
      }

    println(filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0))

    //**

    import cats.Foldable
    import cats.instances.list._

    val ints = List(1, 2, 3, 4)
    println(Foldable[List].foldLeft(ints, 0)(_ + _))

    import cats.instances.option._

    val maybeInt = Option(42)

    println(Foldable[Option].foldLeft(maybeInt, 10)(_ * _))

    lazy val bigData = (1 to 100000).toStream

    lazy val stackOverflowError = bigData.foldRight(0l)(_ + _)

    import cats.instances.stream._
    import cats.Eval

    val eval: Eval[Long] = Foldable[Stream].foldRight(bigData, Eval.now(0l)) { (num, eval) =>
      eval.map(_ + num)
    }

    println(eval.value)

    println(Foldable[Option].nonEmpty(Option(42)))

    println(Foldable[List].find(List(1, 2, 3, 4, 5))(_ % 2 == 0))

    import cats.instances.int._

    println(Foldable[List].combineAll(List(1, 2, 3, 4, 5)))

    import cats.instances.string._

    println(Foldable[List].foldMap(List(1, 2, 3, 4))(_.toString))

    import cats.syntax.foldable._

    println(List(1, 2, 3, 4).combineAll)

    println(List(1, 2, 3, 4, 5).foldMap(_.toString))
  }
}