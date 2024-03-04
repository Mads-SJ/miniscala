{
  class A(b : B) {
    val x: A = b.foo()
  };
  class B(a : A) {
    def foo(): A = a
  }
}
