package example

import cats.data.State
import cats.implicits.catsSyntaxApplicativeId

import scala.util.Try

sealed trait Expression

final case class Num(num: Int) extends Expression
final case class Op(op: Char) extends Expression

object MyState extends scala.App {
  type CalcState[A] = State[List[Int], A]

  def evalOne(symbol: String): CalcState[Int] =
    symbol match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

  def operator(fn: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] { oldStack =>
      val a :: b :: tail = oldStack
      val ans = fn(a, b)
      (ans :: tail, ans)
    }

  def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { oldStack =>
      (num :: oldStack, num)
    }

  println(evalOne("42").run(Nil). value)

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    _ <- evalOne("+")
    ans <- evalOne(symbol = "*")
  } yield ans

  // 어려웠음
  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (acc, sym) =>
      acc.flatMap(_ => evalOne(sym))
    }
  }

  val multistageProgram = evalAll(List("1", "2", "+", "3", "*"))
  println(multistageProgram.runA(Nil).value)

  val biggerProgram = for {
    _ <- evalAll(List("1", "2", "+"))
    _ <- evalAll(List("3", "4", "+"))
    ans <- evalOne("*")
  } yield ans

  println(biggerProgram.run(Nil).value)

  def evalInput(input: String): Int =
    evalAll(input.split(' ').toList).runA(Nil).value

  println(evalInput("1 2 + 5 *"))
}

