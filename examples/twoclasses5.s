{
  class C(c: C, d: D) {
    val foo: D = new D(null, null);
    var bar: D = new D(null, null);
    def baz(d: D, c: C) : D = d
  };
  class D(c: C, d: D) {
    val foo: C = null;
    var bar: C = null;
    def baz(d: D, c : C) : C = c
  };
  new C(null, null)
}
