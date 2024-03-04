package miniscala

import miniscala.Ast._
import miniscala.Interpreter.DynamicClassType
import miniscala.TypeChecker.StaticClassType

/**
  * Unparser for MiniScala.
  */
object Unparser {

  def unparse(n: AstNode): String = n match {
    case IntType() => "Int"
    case FloatType() => "Float"
    case BoolType() => "Boolean"
    case StringType() => "String"
    case NullType() => "Null"
    case TupleType(list) =>
      var str = "("
      for (t <- list) {
        str += unparse(t) + ", "
      }
      str.dropRight(2) + ")"
    case IntLit(c) => c.toString
    case FloatLit(c) => c.toString
    case BoolLit(c) => c.toString
    case StringLit(c) => c
    case VarExp(x) => x
    case BinOpExp(leftexp, op, rightexp) =>
      val leftval = unparse(leftexp)
      val rightval = unparse(rightexp)
      op match {
        case PlusBinOp() => s"($leftval + $rightval)"
        case MinusBinOp() => s"($leftval - $rightval)"
        case MultBinOp() => s"($leftval * $rightval)"
        case DivBinOp() => s"($leftval / $rightval)"
        case ModuloBinOp() => s"($leftval % $rightval)"
        case MaxBinOp() => s"($leftval max $rightval)"
        case EqualBinOp() => s"($leftval == $rightval)"
        case LessThanBinOp() => s"($leftval < $rightval)"
        case LessThanOrEqualBinOp() => s"($leftval <= $rightval)"
        case AndBinOp() => s"($leftval & $rightval)"
        case OrBinOp() => s"($leftval | $rightval)"
      }
    case UnOpExp(op, exp) =>
      val expval = unparse(exp)
      op match {
        case NegUnOp() => s"(-$expval)"
        case NotUnOp() => s"(!$expval)"
      }
    case IfThenElseExp(condexp, thenexp, elseexp) => s"(if (${unparse(condexp)}) ${unparse(thenexp)} else ${unparse(elseexp)})"
    /*case BlockExp(vals, defs, exps) =>
      var str = "{"
      for (d <- vals) {
        str += unparse(d) + "; "
      }
      for (d <- defs) {
        str += unparse(d) + "; "
      }
      str + unparse(exp) + "}"*/
    case BlockExp(_, _, _, _, _) => "Hej. fuck off"
    case ValDecl(x, t, exp) => s"val $x${unparse(t)} = ${unparse(exp)}"
    case DefDecl(fun, params, optrestype, body) =>
      var str = ""
      for (p <- params) {
        str += unparse(p) + ", "
      }
      s"def $fun(${str.dropRight(2)})${unparse(optrestype)} = ${unparse(body)}"
    case FunParam(x, opttype) => s"$x${unparse(opttype)}"
    case FunType(paramtypes, restype) =>
      var str = "("
      for (p <- paramtypes) {
        str += unparse(p) + ", "
      }
      str.dropRight(2) + s") => ${unparse(restype)}"
    case CallExp(fun, args) =>
      var str = ""
      for (a <- args) {
        str += unparse(a) + ", "
      }
      s"${unparse(fun)}(${str.dropRight(2)})"
    case TupleExp(exps) =>
      var str = "("
      for (e <- exps) {
        str += unparse(e) + ", "
      }
      str.dropRight(2) + ")"
    case MatchExp(exp, cases) =>
      var str = "(" + unparse(exp) + " match {"
      for (c <- cases) {
        str += unparse(c)
      }
      str.dropRight(2) + "})"
    case MatchCase(list, exp) =>
      var str = "case ("
      for (v <- list) {
        str += v + ", "
      }
      str.dropRight(2) + ") => " + unparse(exp) + "; "

    case LambdaExp(params, body) =>
      var str = "("
      for (p <- params) {
        str += p.x + ", "
      }
      str.dropRight(2) + ") => " + unparse(body)
    case NewObjExp(klass, args) => "???"
    case ClassNameType(klass) => s"ClassNameType: $klass"
    case DynamicClassType(srcpos) => "DynamicClassType"
    case StaticClassType(a, b, c, d, e) => s"$a, $b, $c, $d, $e"

    case _ => throw new MiniScalaError(s"Unexpected AstNode $n")
  }

  def unparse(ot: Option[Type]): String = ot match {
    case Some(t) => ": " + unparse(t)
    case None => ""
  }
}
