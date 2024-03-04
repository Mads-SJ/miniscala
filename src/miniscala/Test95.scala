/*
package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test95 {

  def main(args: Array[String]): Unit = {
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
    testVal("{ var z: Int = 0; { var t: Int = x; while (y <= t) { z = z + 1; t = t - y }; z } }", IntVal(3), Map("x" -> IntVal(17), "y" -> IntVal(5)))
    testType("{ var z: Int = 0; { var t: Int = x; while (y <= t) { z = z + 1; t = t - y }; z } }", IntType(), Map("x" -> IntType(), "y" -> IntType()))
    testVal("{ var x: Int = 0; def inc(): Int = { x = x + 1; x }; inc(); inc() }", IntVal(2))
    testType("{ var x: Int = 0; def inc(): Int = { x = x + 1; x }; inc(); inc() }", IntType())
    testVal("""{ def make(a: Int): Int => Int = {
              |    var c: Int = a;
              |    def add(b: Int): Int = { c = c + b; c };
              |    add
              |  };
              |  { val c1 = make(100);
              |    val c2 = make(1000);
              |    c1(1) + c1(2) + c2(3) } }""".stripMargin, IntVal(101 + 103 + 1003))
    testFail("{val x: Int = 2; x = x + 1; x}")
    testFail("{var x: Int = 1; x = true; x}")
    test("{var a: Int = 1; def f(x: Int, y: Int): Int = x+y; f({a = a+2; a}, {a = a+2; a})}", IntVal(8), IntType())
    test("{var a: Int = 0; var b: Int = 0; var x: Int = 1; var y: Int = 2; {var x: Int = 3; x = 4; y = 5; a = x+y}; b = x+y; (a, b)}", TupleVal(List(IntVal(9), IntVal(6))), TupleType(List(IntType(), IntType())))

    println("All tests passed successfully!")
  }

  def test(prg: String, rval: Val, rtype: Type): Unit = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String): Unit = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map(), sto: Sto = Map()): Unit = {
    val (res, _) = eval(parse(prg), env, Map[Id, Constructor](), sto)
    assert(res == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()): Unit = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String,env: Env = Map[Id, Val](), sto: Sto = Map[Loc, Val]()): Unit = {
    try {
      eval(parse(prg), env, Map[Id, Constructor](), sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String): Unit = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}*/
