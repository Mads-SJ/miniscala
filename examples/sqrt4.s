{
  def sqrt(x: Float): Float = {

    def improve(y: Float): Float =
      (y + x / y) / 2;

    def sqrtIter(y: Float, i: Int): Float =
      if (i == 0) y
      else sqrtIter(improve(y), i - 1);

    sqrtIter(1.0f, 3)
  };

  sqrt(2.0f)
}
