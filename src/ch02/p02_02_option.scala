package ch02

import ch02.Option.ops.optionOps
import ch02.Option.syntax.optionSyntax

/*
 Option
 装有某种特定对象的容器
 要么一个对象也没有
 要么有一个对象
*/

// 协变
// sealed用来约束trait
sealed trait Option[+A]

// 伴生对象
object Option {
  // smart ctor (智能构造器)
  def none: Option[Nothing] = None

  def some[A](value: A): Option[A] = Some(value)

  /*
   * 使用如下这些基本的函数，可以完成针对Option容器的各种计算
   */
  // map函数
  def map[A, B](a: Option[A])(f: A => B): Option[B] =
    a match {
      case None => None
      case Some(get) => Some(f(get))
    }

  // product函数, 组合两个元素
  def product[A, B](a: Option[A], b: Option[B]): Option[(A, B)] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some((a, b))
    }

  // map2函数
  // 写法1.
  //  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //    (a, b) match {
  //      case (None, _) => None
  //      case (_, None) => None
  //      case (Some(a), Some(b)) => Some(f(a, b))
  //  }

  // 写法2.
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    map(product(a, b)) {
      case (x, y) => f(x, y)
    }

  // map3函数
  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    map2(map2(a, b)((_, _)), c) {
      case ((x, y), z) => f(x, y, z)
    }

  // flatten函数
  def flatten[A](a: Option[Option[A]]): Option[A] =
    a match {
      case None => None
      case Some(x) => x
    }

  // flatMap函数
  def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] =
    flatten(map(a)(f))

  // 隐式转换 (Implicit Conversion)
  // 定义隐式的语法糖class, 提供简单的构造语法
  object ops {
    // "implicit"隐式方法没有必要调用，只要将其导入即可
    implicit def optionOps[A](a: A): OptionOps[A] = new OptionOps(a)
  }

  object syntax {
    implicit def optionSyntax[A](option: Option[A]): OptionSyntax[A] = new OptionSyntax[A](option)
  }

}

// None: 啥都没有
// Nothing: 啥也不是
case object None extends Option[Nothing]

// get是属性名
case class Some[+A](get: A) extends Option[A]

private[ch02] class OptionOps[+A](a: A) {
  def some: Option[A] = Option.some(a)

  def none: Option[A] = Option.none
}

private[ch02] class OptionSyntax[A](option: Option[A]) {
  def map[B](f: A => B): Option[B] = Option.map(option)(f)
}

object Main {
  def main(args: Array[String]): Unit = {
    println(1.some)
    println(1.none)

    val name = "zhangsan".some
    val score = 10.some
    println(Option.map(score)(x => if (x >= 10) "good" else "bad"))

    println(1.some.map(_ + 1))

    1.some
  }
}
