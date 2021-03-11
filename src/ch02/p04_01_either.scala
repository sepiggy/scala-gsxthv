package ch02

import ch02.Either.ops.eitherOps
import ch02.Either.syntax.eitherSyntax

/**
 * FP中推崇将数据和操作分开
 */

sealed trait Either[+A, +B]

case class Left[A](a: A) extends Either[A, Nothing]

case class Right[B](b: B) extends Either[Nothing, B]

object Either {
  def left[A](a: A): Either[A, Nothing] = Left(a)

  def right[B](b: B): Either[Nothing, B] = Right(b)

  object syntax {
    implicit def eitherSyntax[A](a: A): EitherSyntax[A] =
      new EitherSyntax(a)
  }

  object ops {
    implicit def eitherOps[A, B](e: Either[A, B]): EitherOps[A, B] =
      new EitherOps(e)
  }

}

/**
 * 将Either有关的语法糖全部集中到一起
 */
private[ch02] class EitherSyntax[A](a: A) {
  def left[B]: Either[A, B] = Either.left(a)

  def right[B]: Either[B, A] = Either.right(a)
}

/**
 * 将Either有关的操作全部集中到一起
 */
private[ch02] class EitherOps[A, B](e: Either[A, B]) {

  import Either.syntax._

  def map[C](f: B => C): Either[A, C] =
    e match {
      case Left(x) => x.left[C]
      case Right(x) => f(x).right[A]
    }

  def flatMap[C](f: B => Either[A, C]): Either[A, C] =
    e match {
      case Left(x) => x.left[C]
      case Right(x) => f(x)
    }

}

object EitherMain {
  def div(a: Int, b: Int): Either[Exception, Int] =
    try {
      (a / b).right[Exception]
    } catch {
      case x: Exception => x.left[Int]
    }

  def main(args: Array[String]): Unit = {
    println(div(1, 2).map(_ + 3))
    println(div(1, 0).map(_ + 3))
    val d1 = div(6, 2)
    val d2 = div(4, 2)
    println(d1.flatMap(x => d2))
    println(d1.flatMap(x => d2.flatMap(y => div(x, y))))
  }
}