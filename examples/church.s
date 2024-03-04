{
  val tt = x => y => x;
  val ff = x => y => y;
  val ite = x => y => z => x(y)(z);
  val and = x => y => x(y)(x => y => y);
  val or = x => y => x (x => y => x)(y);
  val not = x => x (x => y => y)(x => y => x);
  val zero = s => z => z;
  val one = s => z => s(z);
  val two = s => z => s(s(z));
  val three = s => z => s(s(s(z)));
  val succ = n => s => z => s(n(s)(z));
  val pred = n => f => x => n((g) => (h) => h(g(f)))((u) => x)((u) => u);
  val iszero = n => n(x => (x => y => y))(x => y => x);
  val plus = (m => n => f => x => m(f)(n(f)(x)));
  val mul = (m => n => f => n(m(f)));
  and(iszero(pred(pred(plus(mul(one)(two)(one))))))(iszero(mul(zero)(two)))
}