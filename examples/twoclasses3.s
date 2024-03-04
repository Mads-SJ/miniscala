{
  var t: Int = 0;
  class C(a: C, b: D) {
    val x: C = if ({t = t + 1; t} < 3) new C(null, null) else null;
    val y: D = if ({t = t + 1; t} < 3) new D(null, null) else null
  };
  class D(a: C, b: D) {
    val x: C = if ({t = t + 1; t} < 3) new C(null, null) else null;
    val y: D = if ({t = t + 1; t} < 3) new D(null, null) else null
  };
  new C(null, null)
}