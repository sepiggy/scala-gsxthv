object fib {
  def apply(n: Long): Long = {
    go(0, 1, n)
  }

  /**
   * 尾递归
   */
  private def go(a: Long, b: Long, n: Long): Long = n match {
    case 0 => 0
    case 1 => b
    case x => go(b, a + b, n - 1)
  }
}

fib(10)
fib(12) + fib(13) == fib(1)