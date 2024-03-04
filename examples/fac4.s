{
  def fac(n: Int): Int = {
    def f(n: Int, acc: Int): Int =
      if (n == 0) acc
      else f(n - 1, n * acc);
    f(n, 1)
  };

  fac(5)
}
