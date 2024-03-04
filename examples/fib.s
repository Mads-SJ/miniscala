{
    var x: Int = 1;
    var y: Int = 0;
    var z: Int = 0;
    var i: Int = 0;
    while (i < a) {
      z = x + y;
      x = y;
      y = z;
      i = i + 1
    };
    z
}
