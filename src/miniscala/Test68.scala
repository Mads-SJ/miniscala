/*
package miniscala

import miniscala.Ast._
import miniscala.Interpreter._
import miniscala.TypeChecker._
import miniscala.parser.Parser.parse

object Test68 {

  def main(args: Array[String]): Unit = {
    // miniscala v4
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    test("{ val a = 2; def f(x: Int): Int = x + a; f(2) }", IntVal(4), IntType())
    test("{ def f(x: Int, y: Int): Boolean = x < y; f(1,2) }", BoolVal(true), BoolType())
    test("{ def f(x: Int): Int = x + 5; {val y = f(5); y * 10}}", IntVal(100), IntType())
    testFail("{ def f(x: Int): Int = x; f(true) }")
    testFail("{ def f(x: Int): Int = x < 5; f(1) }")
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")

    // minscala v5
    test("{ val add: Int => (Int => Int) = (x: Int) => (y: Int) => x + y; val inc: Int => Int = add(1); add(1)(2) + inc(3)}", IntVal(7), IntType())
    testTypeFail("{ def f(x: Int): (Int => Int) = (y) => x + y; f(1) }")
    testTypeFail("{ def f(x: Int): (Int => Int) = (y: String) => x + y; f(1) }")
    testTypeFail("{ def f(x: Int): (Int => Int) = (y: Int, z) => x + y + z; f(1) }")
    test("{ val x: Int = 1; { def q(a: Int): Int = x + a; { val x: Int = 2; q(3)}}}", IntVal(4), IntType())
    test("{ def even(x: Int): Boolean = if (x == 0) true else odd(x-1); def odd(x: Int): Boolean = if (x == 0) false else even(x-1); even(3) }", BoolVal(false), BoolType())
    testFail("{ def f(x: Int): (Int => Int) = (y: Int) => x + y; (2+2)(1) }")
    test("{val n = 5; def tak(x: Int): (Int => Int => Int) = (y: Int) => (z: Int) => if (x <= y) y else tak(tak(x-1)(y)(z))(tak(y-1)(z)(x))(tak(z-1)(x)(y)); tak(n)(0)(n+1)}", IntVal(6), IntType())


    // Test 49
    test("{ def f(x: Int): Int = x; f(2) }", IntVal(2), IntType())
    test("{ val a = 2; def f(x: Int): Int = x + a; f(2) }", IntVal(4), IntType())
    test("{ def f(x: Int, y: Int): Boolean = x < y; f(1,2) }", BoolVal(true), BoolType())
    test("{ def f(x: Int): Int = x + 5; {val y = f(5); y * 10}}", IntVal(100), IntType())
    test("{ def even(x: Int): Boolean = if (x == 0) true else odd(x-1); def odd(x: Int): Boolean = if (x == 0) false else even(x-1); even(3) }", BoolVal(false), BoolType())
    testFail("{ def f(x: Int): Int = x; f(true) }")
    testFail("{ def f(x: Int): Int = x < 5; f(1) }")
    testFail("{ def f(x: Int): Int = x; f(2, 3) }")
  }

  def test(prg: String, rval: Val, rtype: Type) = {
    testVal(prg, rval)
    testType(prg, rtype)
  }

  def testFail(prg: String) = {
    testValFail(prg)
    testTypeFail(prg)
  }

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()) = {
    assert(eval(parse(prg), env, Map[Id, Constructor](), Map())._1 == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()) = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String) = {
    try {
      eval(parse(prg), Map[Id, Val](), Map[Id, Constructor](), Map())
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

  def testTypeFail(prg: String) = {
    try {
      typeCheck(parse(prg), Map[Id, Type]())
      assert(false)
    } catch {
      case _: TypeError => assert(true)
    }
  }
}
*/
