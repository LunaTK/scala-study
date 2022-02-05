package example
import cats.Applicative.catsApplicativeForArrow
import cats.data.Reader
import cats.implicits._

final case class Cat(name: String, favoriteFood: String)

object MyReader extends scala.App {
  val catName: Reader[Cat, String] =
    Reader(cat => cat.name)

  catName.run(Cat("Garfield", "lasagne"))

  val greetKitty: Reader[Cat, String] =
    catName.map(name => s"Hello $name")

  greetKitty.run(Cat("Heathcliff", "JunkFood"))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed"

  println(greetAndFeed.run(Cat("Garfield", "lasagne")))

  // 4.8.3 Exercise: Hacking on Readers
  final case class Db(usernames: Map[Int, String], passwords: Map[String, String])
  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    passwordOk <- username.map(username =>
      checkPassword(username, password))
      .getOrElse
        {
          // catsDataMonadForKleisliId should be inferred from the companion object of Reader(alias of Kleisli)
          catsSyntaxApplicativeId(false).pure[DbReader](catsApplicativeForArrow)
        }
  } yield passwordOk

  val users = Map(
    1 -> "Dave",
    2 -> "Alice",
    3 -> "Bob"
  )

  val passwords = Map(
    ("Dave", "zerocool"),
    ("Alice", "acidburn"),
    ("Bob", "secret")
  )

  val db = Db(users, passwords)

  println(checkLogin(1, "zerocool").run(db))
}

