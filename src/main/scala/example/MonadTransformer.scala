package example

import cats.Monad
import cats.data.{OptionT,EitherT}
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.implicits.catsSyntaxApplicativeId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

object MonadTransformer extends scala.App {
  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  val result2: ListOption[Int] = 32.pure[ListOption]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )
  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(ally: String): Response[Int] =
    powerLevels.get(ally) match {
      case Some(avg) => EitherT.right(Future(avg))
      case None => EitherT.left(Future(s"$ally unreachable"))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield power1 + power2 > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val stack: Future[Either[String, Boolean]] = canSpecialMove(ally1, ally2).value

    Await.result(stack, 1.second) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(true) => s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) => s"$ally1 and $ally2 need a recharge."
    }
  }

  println(tacticalReport("Jazz", "Bumblebee"))
  println(tacticalReport("Hot Rod", "Bumblebee"))
  println(tacticalReport("Jazz", "Ironhide"))
}
