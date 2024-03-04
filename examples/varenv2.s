{
  class Cons(x, v, next) {
    def lookup(y) = if (x == y) v else next.lookup(y)
  };

  class Nil() {
    def lookup(y) = new NotFoundError()
  };

  class NotFoundError() {};

  {
    def makeEmpty() = new Nil();

    def extend(e, x, v) = new Cons(x, v, e);

    {
      val env1 = makeEmpty();
      val env2 = extend(env1, "x", 17);
      val env3 = extend(env2, "y", 87);
      val env4 = extend(env3, "x", 42);
      env4.lookup("x")
    }
  }
}