package miniscala

import miniscala.Ast.{IntType, Type}
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test111 {

  def main(args: Array[String]): Unit = {
    testTypeFail("{ class A() {}; new B() }")
    testTypeFail("{ val X = 42; new X() }")
    testTypeFail("{ class C(i: Int) { }; new C(1 - \"hello\") }")
    testTypeFail("{ class C(i: Int) { }; new C(1, 2) }")
    testTypeFail("42.f")
    testTypeFail("{ class C() { def f(x: Int): Int = 42 }; { val o = new C(); o.g } }")
    testTypeFail("{ class C(i) { }; new C(1) } ")
    testTypeFail("{ class C(i) { 1 - \"hello\" } } ")
    testTypeFail("{ class A() {}; class B() {}; { val x: A = new B() }}")
    testTypeFail(
      """
        |    { class C() { val a: Boolean = false };
        |      {
        |        {
        |          var x: C = new C();
        |          class C() { val a: Int = 42 };
        |          { val y: C = x }
        |        }
        |      }
        |    }""".stripMargin)
    testTypeFail(
      """
        |    { class C() { val a: Boolean = false };
        |      {
        |        {
        |          var x: C = new C();
        |          class C() { val a: Int = 42 };
        |          { x = new C() }
        |        }
        |      }
        |    }""".stripMargin)

    testType(
      """{ class A() { };
        |  class B() { var x: A = new A() } }""".stripMargin, unitType)
    testType(
      """{
        |  class Counter(init: Int) {
        |    var c: Int = init;
        |    def getValue(): Int = c;
        |    def inc(): Unit = { c = c + 1 }
        |  };
        |  {
        |    val x = new Counter(3);
        |    x.inc();
        |    x.inc();
        |    x.getValue()
        |  }
        |}""".stripMargin, IntType())
    testType("{ class A(x: Int) { val x: Int = x }; { def f(a: A): Int = a.x; f(new A(2)) } }", IntType())
    testType("{ class A() { }; class B(a: A) { }; new B(new A()) ; {} }", unitType)

    println("All tests passed successfully!")
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map(), ctenv: ClassTypeEnv = Map()): Unit = {
    assert(typeCheck(parse(prg), tenv, ctenv) == out)
  }

  def testTypeFail(prg: String): Unit = {
    try {
      typeCheck(parse(prg), Map(), Map())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}
