/*
package miniscala

import miniscala.Ast.{BoolType, Id, IntType, Type}
import miniscala.Interpreter._
import miniscala.TypeChecker.{TypeEnv, TypeError, typeCheck}
import miniscala.parser.Parser.parse

object TestV5 {

  def main(args: Array[String]): Unit = {

    // Test simple function call
    testVal("{def f(x) = x; f(2)}", IntVal(2))

    // Type checker should complain if type annotation is missing
    testTypeFail("{def f(x) = x; f(2)}")

    // Use new env to eval next ValDecl
    test("{val x = 1; val y = x; y}", IntVal(1), IntType())

    // Use new env to decl functions
    testType("{val x = 1; def f(): Int = x; f()}", IntType())

    // Do not leak parameters into next function
    //testFail("{def f(x: Int): Int = x; def g(): Int = x; g()}")

    // Do not leak parameters into BlockExp body
    //test("{val x: Boolean = true; def f(x: Int): Int = x; x}", BoolVal(true), BoolType()) // TODO: fix. glem det

    // Do not use function env to eval args
    test("{val f = {val x = 1; (y: Int) => y}; val x = 2; f(x)}", IntVal(2), IntType())

    // Do use function env to eval function
    test("{val f = {val x = 1; () => x}; val x = 2; f()}", IntVal(1), IntType())

    // Test lambda functions with one argument
    test("{val x: Int = 1; val f = (a: Int) => a * x; {{val x: Int = 3; val g = (y: Int) => y + x; f(2)+g(3)}}}", IntVal(8), IntType())

    // Test lambda function with two arguments
    test("{val div = (a: Int) => ((b: Int) => a/b); div(18)(3)}", IntVal(6), IntType())

    // Test lambda function with 3 arguments, one of which is a function
    test("{val binOp = (op: (Int, Int)=>Int) => (x: Int) => (y: Int) => op(x,y); val plus = (x: Int,y: Int) => x+y; binOp(plus)(2)(4)} ", IntVal(6), IntType())

    // Lambda function with two arguments one of which is a function
    test("{val i = (f: Boolean => Int) => (x: Int) => f(x<10)+x; def g(y: Boolean): Int = if(y) 5 else 10; i(g)(5)}", IntVal(10), IntType())

    // Two mutual recursive functions with Lambda as parameter to f
    test("{def g(x: Int): Int = if (x==1) 1 else 2*f(x-1); def f(x:Int): Int = if (x==1) 1 else g(x-1); {val q = (x: Int) => ((y: Int) => x*y); f(q(3)(1))}}", IntVal(2), IntType())

    // Lambda function with wrong param type
    // testTypeFail("{val mult = (a: Boolean) => ((b: Int) => a*b); mult(2)(3)}") // TODO: fix

    // Lambda function with param and arg not matching
    testFail("{val mult = (a: Int) => ((b: Int) => a*b); mult(2)(3)(4)}")

    // Multiple defs in same block
    //testFail("{def g(): Int = x; def f(x: Int): Int = g(); f(1)}") // TODO: fix

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

  def testVal(prg: String, value: Val, env: Env = Map[Id, Val]()): Unit = {
    assert(eval(parse(prg), env, Map[Id, Constructor](), Map())._1 == value)
  }

  def testType(prg: String, out: Type, tenv: TypeEnv = Map[Id, Type]()): Unit = {
    assert(typeCheck(parse(prg), tenv) == out)
  }

  def testValFail(prg: String): Unit = {
    try {
      eval(parse(prg), Map[Id, Val](), Map[Id, Constructor](), Map())
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
