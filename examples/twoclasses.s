{
  val x1 = {
    class C() {
      val a: Int = 42
    };
    {
      val t1: C = new C();
      {
        val t2: C = t1;
        t2
      }
    }
  };

  val x2 = {
    class C() {
      val a: Boolean = true
    };
    new C()
  };

  val a1: Int = x1.a;
  val a2: Boolean = x2.a;

  if (a2) a1 else 0
}