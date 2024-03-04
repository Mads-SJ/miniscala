package miniscala

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
  * Abstract syntax tree representation of MiniScala programs.
  */
object Ast {

  /**
    * An AST node contains information about its position in the source code.
    */
  sealed abstract class AstNode extends Positional

  /**
    * Identifiers are just strings.
    */
  type Id = String

  /**
    * Expressions (excluding literals).
    */
  sealed abstract class Exp extends AstNode

  case class VarExp(x: Id) extends Exp

  case class BinOpExp(leftexp: Exp, op: BinOp, rightexp: Exp) extends Exp

  case class UnOpExp(op: UnOp, exp: Exp) extends Exp

  case class IfThenElseExp(condexp: Exp, thenexp: Exp, elseexp: Exp) extends Exp

  case class BlockExp(vals: List[ValDecl], vars: List[VarDecl], defs: List[DefDecl], classes: List[ClassDecl], exps: List[Exp]) extends Exp

  case class TupleExp(exps: List[Exp]) extends Exp

  case class MatchExp(exp: Exp, cases: List[MatchCase]) extends Exp

  case class CallExp(funexp: Exp, args: List[Exp]) extends Exp

  case class LambdaExp(params: List[FunParam], body: Exp) extends Exp

  case class AssignmentExp(x: Id, exp: Exp) extends Exp

  case class WhileExp(cond: Exp, body: Exp) extends Exp

  case class NewObjExp(klass: Id, args: List[Exp]) extends Exp

  case class LookupExp(objexp: Exp, member: Id) extends Exp

  /**
    * Literals.
    */
  sealed abstract class Literal extends Exp

  case class IntLit(c: Int) extends Literal

  case class BoolLit(c: Boolean) extends Literal

  case class FloatLit(c: Float) extends Literal

  case class StringLit(c: String) extends Literal

  case class NullLit() extends Literal

  /**
    * Binary operators.
    */
  sealed abstract class BinOp extends AstNode

  case class PlusBinOp() extends BinOp

  case class MinusBinOp() extends BinOp

  case class MultBinOp() extends BinOp

  case class DivBinOp() extends BinOp

  case class EqualBinOp() extends BinOp

  case class LessThanBinOp() extends BinOp

  case class LessThanOrEqualBinOp() extends BinOp

  case class ModuloBinOp() extends BinOp

  case class MaxBinOp() extends BinOp

  case class AndBinOp() extends BinOp

  case class OrBinOp() extends BinOp

  /**
    * Unary operators.
    */
  sealed abstract class UnOp extends AstNode

  case class NegUnOp() extends UnOp

  case class NotUnOp() extends UnOp

  /**
    * Declarations.
    */
  sealed abstract class Decl extends AstNode

  case class ValDecl(x: Id, opttype: Option[Type], exp: Exp) extends Decl

  case class VarDecl(x: Id, opttype: Option[Type], exp: Exp) extends Decl

  case class DefDecl(fun: Id, params: List[FunParam], optrestype: Option[Type], body: Exp) extends Decl

  case class ClassDecl(klass: Id, params: List[FunParam], body: BlockExp) extends Decl

  /**
    * Function parameters.
    */
  case class FunParam(x: Id, opttype: Option[Type]) extends AstNode

  /**
    * Match cases.
    */
  case class MatchCase(pattern: List[Id], exp: Exp) extends AstNode

  /**
    * Types.
    */
  abstract class Type extends AstNode

  case class IntType() extends Type

  case class BoolType() extends Type

  case class FloatType() extends Type

  case class StringType() extends Type

  case class TupleType(types: List[Type]) extends Type

  case class FunType(paramtypes: List[Type], restype: Type) extends Type

  case class ClassNameType(klass: Id) extends Type

  case class NullType() extends Type

  /**
    * Exception with a message and (optionally) a source code position.
    */
  class MiniScalaError(msg: String, pos: Position = NoPosition)
    extends RuntimeException(if (pos != NoPosition) s"$msg at line ${pos.line} column ${pos.column}" else msg)
}
