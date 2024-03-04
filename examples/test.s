{ val x: (Int, Null) = (42, null);
  val y: (Float, String) = x;
  def f(t: Float): Float = {t + 1};
  def g(e: Float => Float): Float = {e(1)};
  class C() {};
  { var z: C = null;
    z = null;
    g(f)
} }