package miniscala

import miniscala.Ast._
import miniscala.parser.Parser.parse
import miniscala.Unparser._
import miniscala.parser.Parser
import miniscala.Interpreter._
import miniscala.TypeChecker._

object Test {
  def main(args: Array[String]): Unit = {
    /*assert(1 + 1 == 2)
    assert(1 + 1 != 3)
    assert(5 * 5 == 25)
    assert((10 max 5)==10)
    assert((10 max 10)==10)
    assert((10 max -10)==10)
    val p = Parser.readFile("examples/test1.s")
    assert(parse(unparse(parse(p))) == parse(p))
    println("All good!")*/
    //println(Interpreter.simplify(parse("x / 1")))
    //println(unparse(parse("(1,2) match {case (x, y) => x + y; case (x,y,z) => x + y + z}")))
    //println(eval(parse("if (1) 2 + 2 else 1"), Map[Var, Val]()))
    //println(unparse(parse("2 + 2"), Map[Var, Val](), Map[Fun, Closure]()).asInstanceOf[AstNode])

    //println(2.0f == 2)
  }

}
