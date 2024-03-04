{
  class Cons(x, xs) {

    def head() = x;

    def tail() = xs;

    def toString() = "Cons(" + x + ", " + xs.toString() + ")";

    def length() = xs.length() + 1;

    def append(y) = new Cons(x, xs.append(y));

    def reverse() = xs.reverse().append(x)
  };

  class Nil() {

    def head() = new ListEmptyError();

    def tail() = new ListEmptyError();

    def toString() = "Nil";

    def length() = 0;

    def append(y) = new Cons(y, new Nil());

    def reverse() = new Nil()
  };

  class ListEmptyError() {};

  {
    val xs = new Cons(1, new Cons(2, new Cons(3, new Nil())));
    val ys = xs.reverse();
    "reversed list " + ys.toString() + " has length " + ys.length()
  }
}