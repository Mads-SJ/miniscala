{ val x = 1;
  { def q(a) = x + a;
    { val x = 2;
      q(3)
    }
  }
}
