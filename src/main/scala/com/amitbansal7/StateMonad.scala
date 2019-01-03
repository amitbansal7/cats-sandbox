package com.amitbansal7

object StateMonad {
  // def main(agrs: Array[String]): Unit = {
  //   println("*" * 150)
  //   code
  //   println("*" * 150)
  // }
  def code = {
    import cats.data.State

    val a = State[Int, String] { state =>
      (state, s"State is ${state}")
    }
    println(a.run(12).value)

    val step1 = State[Int, String] { n =>
      val res = n + 1
      (res, s"step1 ${res}")
    }

    val step2 = State[Int, String] { n =>
      val res = n * 12
      (res, s"step2 ${res}")
    }

    val step3 = State[Int, String] { n =>
      val res = n * -1
      (res, s"step3 ${res}")
    }

    val both = for {
      a <- step1
      b <- step2
      c <- step3
    } yield (a, b, c)

    println(both)

    println(both.run(20).value)

    //Post order calculator

    type CalcState[A] = State[List[Int], A]

    def operator(f: (Int, Int) => Int): CalcState[Int] =
      State[List[Int], Int] {
        case b :: a :: tail =>
          val ans = f(a, b)
          (ans :: tail, ans)
        case _ => sys.error("error")
      }

    def operand(num: Int): CalcState[Int] =
      State[List[Int], Int] { stack =>
        (num :: stack, num)
      }

    def evalOne(c: String): CalcState[Int] =
      c match {
        case "+" => operator(_ + _)
        case "-" => operator(_ - _)
        case "*" => operator(_ * _)
        case "/" => operator(_ / _)
        case num => operand(num.toInt)

      }
    import cats.syntax.applicative._

    def evalAll(inp: List[String]): CalcState[Int] =
      inp.foldLeft(0.pure[CalcState]) { (a, b) =>
        a.flatMap(_ => evalOne(b))
      }

    def evalInput(inp: String): Int =
      evalAll(inp.split(" ").toList).runA(Nil).value

    println(evalInput("1 2 + 4 5 + *"))
  }
}