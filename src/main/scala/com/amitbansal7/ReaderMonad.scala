package com.amitbansal7

object ReaderMonad {

  def main(agrs: Array[String]): Unit = {
    println("*" * 150)
    code
    println("*" * 150)
  }

  def code = {
    import cats.data.Reader

    case class Cat(name: String, favFood: String)

    val catName: Reader[Cat, String] =
      Reader(cat => cat.name)

    val cat = Cat("abc", "cookie")

    println(catName.run(cat))

    val greeKitty: Reader[Cat, String] =
      Reader(cat => s"Hello, ${cat.name}")

    println(greeKitty(cat))

    val feedKitty: Reader[Cat, String] =
      Reader(cat => s"eat ${cat.favFood}")

    val greeAndFeed: Reader[Cat, String] =
      for {
        greet <- greeKitty
        feed <- feedKitty
      } yield s"$greet, $feed"

    println(greeAndFeed(cat))

    //4.8.3 Hacking on Readers

    case class Db(
      usernames: Map[Int, String],
      passwords: Map[String, String])

    type DbReader[A] = Reader[Db, A]

    def findUsername(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    import cats.syntax.applicative._

    def checkLogin(userId: Int, password: String): DbReader[Boolean] =
      for {
        username <- findUsername(userId)
        passwordOk <- username.map(uname => checkPassword(uname, password)).getOrElse(false.pure[DbReader])
      } yield passwordOk

    val users = Map(
      1 -> "a",
      2 -> "b",
      3 -> "c")

    val passwords = Map(
      "a" -> "ap",
      "b" -> "bp",
      "c" -> "cp")

    val db = Db(users, passwords)

    println(checkLogin(1, "ap").run(db))
    println(checkLogin(2, "ap").run(db))
  }
}