{
  class A(b : B) {
    val x: Int = b.foo();
    def bar(): Int = 42
  };
  class B(a : A) {
    val y: Int = a.x;
    var z: Int = a.bar();
    def foo(): Int = 42
  }
}
