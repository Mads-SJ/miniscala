{
  class Ping() {

    def ping(x) =
      if (100 < x)
        x
      else {
        val p = new Pong(x + 1);
        p.pong()
      }

  };

  class Pong(y) {

    def pong() =
      if (100 < y)
        y
      else {
        val p = new Ping();
        p.ping(y * 2)
      }

  };

  new Ping().ping(7)
}