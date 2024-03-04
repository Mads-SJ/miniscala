{
  class Counter(init: Int) {
    var c: Int = init;
    def getValue(): Int = c;
    def inc(): Unit = { c = c + 1 }
  };

  {
    val x: Counter = new Counter(3);

    x.inc();
    x.inc();

    x.getValue()
  }
}
