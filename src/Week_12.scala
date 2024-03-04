object Week_12 {
  sealed abstract class List[+T] {
    def concat[S >: T](xs: List[S]): List[S] = this match {
      case Nil => xs
      case Cons(z, zs) => Cons(z, zs.concat(xs))
    }
  }

  case object Nil extends List[Nothing]

  case class Cons[T](x: T, xs: List[T]) extends List[T]

  case class Person()
  case class Student() extends Person
  case class Teacher() extends Person

  def zip[T, U](xs: List[T], ys: List[U]): List[(T, U)] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(z, zs), Cons(w, ws)) => Cons((z, w), zip(zs, ws))
  }

  def main(args: Array[String]): Unit = {
    val a: List[Teacher] = Cons(new Teacher, Cons(new Teacher, Nil))
    val b: List[Student] = Cons(new Student, Cons(new Student, Nil))
    val c: List[(Teacher, Student)] = zip(a, b)
    println(c)
  }
}
