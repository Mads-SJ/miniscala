{
  def makeEmpty() = (x) => -1;

  def extend(e, x, v) = (y) => if (x == y) v else e(y);

  def lookup(e, x) = e(x);

  {
    val env1 = makeEmpty();
    val env2 = extend(env1, "x", 17);
    val env3 = extend(env2, "y", 87);
    val env4 = extend(env3, "x", 42);
    lookup(env4, "x")
  }
}