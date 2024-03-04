{
  class C() {
    val a: Boolean = false
  };
  {
    var c: C = new C();
    def f(p: C): Boolean =  p.a;
    class C() {
      val a: Int = 42
    };
    {
      //val d: C = c;
      f(c);
      f(new C());
      //c = new C();
      f(c)
    }
  }
}