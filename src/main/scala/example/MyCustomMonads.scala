package example

import cats.Monad
import scala.annotation.tailrec
import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map

object MyCustomMonads extends scala.App {

//  val optionMonad = new Monad[Option] {
//    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
//      fa flatMap f
//
//    override def pure[A](x: A): Option[A] =
//      Some(x)
//
//    @tailrec
//    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] =
//      f(a) match {
//        case None => None
//        case Some(Left(a1)) => tailRecM(a1)(f)
//        case Some(Right(b)) => Some(b)
//      }
//  }

  def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    f(start).flatMap{ a =>
      retry(a)(f)
    }

  def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
    Monad[F].tailRecM(start)(a => f(a).map(a2 => Left(a2)))

  // works for small value
  println(retryTailRecM(1000)(a => if (a==0) None else Some(a-1)))

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeMonad = new Monad[Tree] {
    override def pure[A](x: A): Tree[A] = leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(f(a)) {
        case Left(value) => tailRecM(value)(f)
        case Right(value) => Leaf(value)
      }
  }
  // verify that the Moand provides Functor-like behaviour
  val tree = branch(leaf(1), leaf(3))
}
