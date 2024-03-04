{ def mc91(n: Int): Int =
    if (100 < n) n - 10
    else mc91(mc91(n + 11));
  mc91(n)
}
