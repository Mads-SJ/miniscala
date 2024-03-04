package miniscala

import miniscala.Interpreter._
import miniscala.parser.Parser.parse

object Test112 {

  def main(args: Array[String]): Unit = {
    testValFail("""{ var z = null; z.f }""")
    testVal("""{ class C() { }; { var x: C = null; x = new C() } }""".stripMargin, TupleVal(List[Val]()))
    testVal("""{ class C() { }; { var x: C = new C(); x = null } }""".stripMargin, TupleVal(List[Val]()))
    testVal("{class C() {}; class B(c: C){}; new B(null); {}}", TupleVal(List[Val]()))
    //testValFail("{class C(){}; {def f(x: Int): Null = {null}; def g(y: Int => C): Null = {null}; g(f); {}}}")

    println("All tests passed successfully!")
  }

  def testVal(prg: String, value: Val, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()): Unit = {
    val (res, _) = eval(parse(prg), env, cenv, sto)
    assert(res == value)
  }

  def testValFail(prg: String, env: Env = Map(), cenv: ClassEnv = Map(), sto: Sto = Map()): Unit = {
    try {
      eval(parse(prg), env, cenv, sto)
      assert(false)
    } catch {
      case _: InterpreterError => assert(true)
    }
  }

}