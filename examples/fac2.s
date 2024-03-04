{
  val fac2 = (f) => (n) =>
    if (n == 0) 1
    else n * f(f)(n - 1);
  {
    val fac = fac2(fac2);
    fac(5)
  }
}
