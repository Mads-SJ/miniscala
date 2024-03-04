{
  def fib(n: Int): Int = if (n == 0 | n - 1 == 0) n else fib(n - 1) + fib(n - 1 - 1);
  fib(10)
}
