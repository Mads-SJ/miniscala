{
  class Counter(init: Int) {
    var c = init;
    def getValue(): Int = c;
    def inc() = { c = c + 1 }
  };

  {
    val x = new Counter(3);

    x.inc();
    x.inc();

    x.getValue()
  }
}
