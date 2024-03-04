{
  class C() {
    val x: Int = 42
  };
  {
    def foo(): C = new C();
    foo().x
  }
}
