{
  def sqrt(x: Float): Float = sqrtIter(x, 1.0f, 4);

  def improve(x: Float, y: Float): Float =
    (y + x / y) / 2;

  def sqrtIter(x: Float, y: Float, i: Int): Float =
    if (i == 0) y
    else sqrtIter(x, improve(x, y), i - 1);

  sqrt(a)
}
