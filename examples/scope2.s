{ val x = 1;
  { def q() = {
      val x = 2;
      x
    };
    { val t = q();
      t + x
    }
  }
}
