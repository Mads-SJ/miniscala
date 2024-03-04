{
  def even(x: Int): Boolean =
    if (x == 0) true else odd(x-1);

  def odd(x: Int): Boolean =
    if (x == 0) false else even(x-1);

  even(3)
}