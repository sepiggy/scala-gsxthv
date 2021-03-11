/**
 * Either的特征(trait): 装有某种对象的容器
 * 可能是一个异常
 * 也可能是一个正常的返回值
 * 进一步泛化，可能是这种类型的对象，也可能是另一种类型的对象
 */
sealed trait Either[+A, +B]

// 表示错误的值
case class Left[A](value: A) extends Either[A, Nothing]

// 表示正确的值
case class Right[B](value: B) extends Either[Nothing, B]

object Either {
  // 智能构造器
  def left[A](a: A): Either[A, Nothing] = Left(a)

  def right[B](b: B): Either[Nothing, B] = Right(b)

  // 隐式转换
  class EitherSyntax[A](a: A) {
    def left[B]: Either[A, B] = Either.left(a)

    def right[B]: Either[B, A] = Either.right(a)
  }

  object syntax {
    implicit def eitherSyntax[A](a: A): EitherSyntax[A] =
      new EitherSyntax(a)
  }

}

Either.left("bad name")
Either.right("good name")
Either.right(1)

import Either.syntax.eitherSyntax

new Exception("bad name").left
new Exception("bad name").left[Double]

def div(a: Int, b: Int): Either[Exception, Int] =
  try {
    (a / b).right[Exception]
  } catch {
    case x: Exception => x.left[Int]
  }

div(1, 2)
div(1, 0)

def div1(a: Int, b: Int): Either[Exception, Int] =
  (a, b) match {
    case (_, 0) => Left(new Exception("初零错误"))
    case _ => Right(a / b)
  }

def eitherDiv(a: Either[Exception, Int], b: Either[Exception, Int]): Either[Exception, Int] =
  (a, b) match {
    case (Left(x), _) => Left(x)
    case (_, Left(x)) => Left(x)
    case (Right(x), Right(y)) => div1(x, y)
  }

eitherDiv(Right(2),Right(0))



