{
  def fac(n: Int): Int = fac2(n, 1);

  def fac2(n: Int, acc: Int): Int =
    if (n == 0) acc
    else fac2(n - 1, n * acc);

  fac(10)
}
