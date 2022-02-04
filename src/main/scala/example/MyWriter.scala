package example

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future} // for tell

object MyWriter extends scala.App {
  val v = Writer(Vector(
    "It was the best of times",
    "It was the worst of times"
  ), 1859)

  type Logged[A] = Writer[Vector[String], A]

  println(123.pure[Logged])

  println(Vector("msg1", "msg2", "msg3").tell)

  val a = 123.writer(Vector[String]())

  def putLog[A](msg: String) =
    Vector(msg).tell

  val vvv = for {
    v <- a
    _ <- Vector("logo").tell
    _ <- Vector("logo2").tell
  } yield v
  println(vvv)

  val writer1 = for {
    a <- 10.pure[Logged]
    _ <- Vector("a", "b", "c").tell
    b <- 32.writer(Vector("x", "y", "z"))
  } yield a + b

  println(writer1)

  def slowly[A](body: => A) =
    try body finally Thread.sleep(100)

  def factorial(n: Int): Logged[Int] =
    for {
      next <- if (n==0) 1.pure[Logged] else factorial(n-1)
      ans = if (n==0) next else next * (n)
      _ <- Vector(s"fact $n $ans").tell
    } yield ans

  val logs = Await.result(Future.sequence(Vector(
    Future(factorial(5)),
    Future(factorial(5))
  )), 5.second)

  println(logs)
  //  println(factorial(10))
}
