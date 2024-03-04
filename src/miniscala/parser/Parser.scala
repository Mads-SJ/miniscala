package miniscala.parser

import java.io.{File, IOException}

import miniscala.Ast._
import Tokens._

import scala.io.Source
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input._

/**
  * Parser for MiniScala.
  *
  * (You do *not* need to read this code!)
  */
object Parser extends PackratParsers {

  override type Elem = MiniScalaToken

  private lazy val prog: PackratParser[Exp] = phrase { expr() }

  private def expr(antiPrecedence: Int = 7): PackratParser[Exp] =
    antiPrecedence match {
      case 7 =>
        lambda |
          ifthenelse |
          wwhile |
          assignment |
          mmatch |
          expr(6)
      case x if x >= 0 =>
        binopexp(antiPrecedence) |
          expr(x - 1)
      case -1 =>
        unopexp |
          call |
          tupleexp |
          expr(-2)
      case -2 =>
        lookup |
          expr(-3)
      case -3 =>
        newobj |
          literal |
          identifier ^^ { id => VarExp(id.str).setPos(id.pos) } |
          block |
          parens
    }

  private lazy val parens: PackratParser[Exp] =
    (LEFT_PAREN() ~ expr() ~ RIGHT_PAREN()) ^^ { case _ ~ exp ~ _ => exp }

  private lazy val mmatch: PackratParser[Exp] = positioned {
    (expr(6) ~ MATCH() ~! LEFT_BRACE() ~! repsep(ccase, SEMICOLON()) ~! RIGHT_BRACE()) ^^ {
      case target ~ _ ~ _ ~ cases ~ _ => MatchExp(target, cases)
    }
  }

  private lazy val tupleexp: PackratParser[Exp] = positioned {
    (LEFT_PAREN() ~ RIGHT_PAREN()) ^^ {
      case _ ~ _ => TupleExp(List())
    } |
    (LEFT_PAREN() ~ expr() ~ COMMA() ~ rep1sep(expr(), COMMA()) ~ RIGHT_PAREN()) ^^ {
      case _ ~ exp ~ _ ~ others ~ _ => TupleExp(exp :: others)
    }
  }

  private lazy val ccase: PackratParser[MatchCase] = positioned {
    CASE() ~! tuplepattern ~! ARROW() ~! expr() ^^ { case _ ~ matcher ~ _ ~ body => MatchCase(matcher, body) }
  }

  private lazy val tuplepattern: PackratParser[List[String]] =
    (LEFT_PAREN() ~ RIGHT_PAREN()) ^^ {
      case _ ~ _ => List()
    } |
    (LEFT_PAREN() ~ identifier ~ COMMA() ~ rep1sep(identifier, COMMA()) ~ RIGHT_PAREN()) ^^ {
      case _ ~ id ~ _ ~ ids ~ _ => (id :: ids).map(_.str)
    }

  private lazy val ifthenelse: PackratParser[Exp] = positioned {
    (IFF() ~! LEFT_PAREN() ~! expr() ~! RIGHT_PAREN() ~! expr() ~! EELSE() ~! expr()) ^^ {
      case _ ~ _ ~ exp1 ~ _ ~ exp2 ~ _ ~ exp3 => IfThenElseExp(exp1, exp2, exp3)
    }
  }

  private lazy val wwhile: PackratParser[Exp] = positioned {
    (WWHILE() ~! LEFT_PAREN() ~! expr() ~! RIGHT_PAREN() ~! expr()) ^^ {
      case _ ~ _ ~ exp1 ~ _ ~ exp2 => WhileExp(exp1, exp2)
    }
  }

  private lazy val blockel: PackratParser[AstNode] = valdecl | vardecl | defdecl | classdecl | expr()

  private lazy val blockelmseq: PackratParser[List[AstNode]] = repsep(blockel, SEMICOLON())

  type BlockTupleType = Tuple5[List[ValDecl], List[VarDecl], List[DefDecl], List[ClassDecl], List[Exp]]

  private def validBlock[T](l: List[T]): Option[BlockTupleType] = {
    val matchers = List[Function[T, Boolean]](
      { case _: ValDecl => true
      case _ => false
      },
      { case _: VarDecl => true
      case _ => false
      },
      { case _: DefDecl => true
      case _ => false
      },
      { case _: ClassDecl => true
      case _ => false
      },
      { case _: Exp => true
      case _ => false
      })
    val (remaining, splits) = matchers.foldLeft((l, List[List[T]]())) {
      case ((list: List[T], outcome: List[List[T]]), matcher) =>
        val sublist = list.takeWhile(elm => matcher(elm))
        (list.drop(sublist.size), sublist :: outcome)
    }
    val items = splits.reverse
    if (remaining.isEmpty) Some((
      items.head.map(_.asInstanceOf[ValDecl]),
      items(1).map(_.asInstanceOf[VarDecl]),
      items(2).map(_.asInstanceOf[DefDecl]),
      items(3).map(_.asInstanceOf[ClassDecl]),
      items(4).map(_.asInstanceOf[Exp]))
    )
    else None
  }

  private lazy val block: PackratParser[BlockExp] = positioned {
    ((LEFT_BRACE() ~! blockelmseq ~! RIGHT_BRACE()) ^^ {
      case _ ~ l ~ _ => validBlock(l).map(BlockExp.tupled)
    } filter(_.isDefined)) ^^ {_.get}
  }

  private lazy val assignment: PackratParser[Exp] = positioned {
    (identifier ~ EQ() ~! expr()) ^^ {
      case id ~ _ ~ exp => AssignmentExp(id.str, exp)
    }
  }

  private lazy val call: PackratParser[Exp] =
    (expr(-2) ~ rep1(appl)) ^^ { case target ~ applications =>
      applications.tail.foldLeft(CallExp(target, applications.head).setPos(target.pos)) {
        case (curr, acc) => CallExp(curr, acc).setPos(curr.pos) } }

  private lazy val calllookup: PackratParser[Exp] =
    (identifier ~ rep1(appl)) ^^ { case target ~ applications =>
      applications.tail.foldLeft(CallExp(VarExp(target.str).setPos(target.pos), applications.head).setPos(target.pos)) {
        case (curr, acc) => CallExp(curr, acc).setPos(curr.pos) } }

  private lazy val appl: PackratParser[List[Exp]] =
    (LEFT_PAREN() ~ repsep(expr(), COMMA()) ~ RIGHT_PAREN()) ^^ { case _ ~ apps ~ _ => apps }

  private lazy val lambda: PackratParser[Exp] = positioned {
    LEFT_PAREN() ~ repsep(identifier ~ opttypeannotation, COMMA()) ~ RIGHT_PAREN() ~ ARROW() ~! expr() ^^ {
      case _ ~ identifiers ~ _ ~ _ ~ body =>
        LambdaExp(identifiers.map(p => FunParam(p._1.str, p._2).setPos(p._1.pos)), body)
    } |
      identifier ~ ARROW() ~! expr() ^^ {
        case id ~ _ ~ body => LambdaExp(List(FunParam(id.str, None).setPos(id.pos)), body)
      }
  }

  private def replaceVarTarget(callExp: CallExp, newTarget: VarExp => Exp): CallExp = {
    callExp match {
      case CallExp(id: VarExp, a) => CallExp(newTarget(id), a)
      case CallExp(e: CallExp, a) => CallExp(replaceVarTarget(e, newTarget), a)
      case _ => ???
    }
  }.setPos(callExp.pos)

  private lazy val lookup: PackratParser[Exp] = positioned {
    (calllookup | expr(-3)) ~ DOT() ~ rep1sep(calllookup | identifier, DOT()) ^^ {
      case e ~ _ ~ ids => ids.foldLeft(e: Exp) { case (acc, curr) =>
        curr match {
          case c: CallExp => replaceVarTarget(c, id => LookupExp(acc, id.x).setPos(c.pos))
          case id: IDENTIFIER => LookupExp(acc, id.str).setPos(id.pos)
        }
      }
    }
  }

  private lazy val newobj: PackratParser[Exp] = positioned {
    NEW() ~! identifier ~! appl ^^ { case _ ~ name ~ args => NewObjExp(name.str, args)}
  }

  private lazy val valdecl: PackratParser[Decl] = positioned {
    (VVAL() ~! identifier ~! opttypeannotation ~! EQ() ~! expr()) ^^ {
      case _ ~ id ~ t ~ _ ~ exp => ValDecl(id.str, t, exp) }
  }

  private lazy val vardecl: PackratParser[Decl] = positioned {
    (VVAR() ~! identifier ~! opttypeannotation ~! EQ() ~! expr()) ^^ {
      case _ ~ id ~ t ~ _ ~ exp => VarDecl(id.str, t, exp)
    }
  }

  private lazy val defdecl: PackratParser[Decl] = positioned {
    (DDEF() ~! identifier ~! LEFT_PAREN() ~! repsep(identifier ~! opttypeannotation, COMMA()) ~! RIGHT_PAREN() ~! opttypeannotation ~! EQ() ~! expr()) ^^ {
      case _ ~ id ~ _ ~ identifiers ~ _ ~ retType ~ _ ~ exp =>
        DefDecl(id.str, identifiers.map(p => FunParam(p._1.str, p._2).setPos(p._1.pos)), retType, exp)
    }
  }

  private lazy val classdecl: PackratParser[Decl] = positioned {
    CLASS() ~! identifier ~! LEFT_PAREN() ~! repsep(identifier ~! opttypeannotation, COMMA()) ~ RIGHT_PAREN() ~! block ^^ {
      case _ ~ name ~ _ ~ params ~ _ ~ body =>
        ClassDecl(name.str, params.map(p => FunParam(p._1.str, p._2).setPos(p._1.pos)), body)
    }
  }

  private lazy val opttypeannotation: PackratParser[Option[Type]] =
    opt { (COLON() ~! typeannotation) ^^ { case _ ~ ta => ta }  }

  private def binopexp(antiPrecedence: Int): PackratParser[Exp] =
    expr(antiPrecedence - 1) * {
      binop(antiPrecedence) ^^ { op => { (left: Exp, right: Exp) => BinOpExp(left, op, right).setPos(left.pos) } }
    }

  private lazy val literal: PackratParser[Literal] = positioned {
    strliteral ^^ { lit => StringLit(lit.str) } |
      boolliteral ^^ { lit => BoolLit(lit.b) } |
      intliteral ^^ { lit => IntLit(lit.i) } |
      floatliteral ^^ { lit => FloatLit(lit.v) } |
      nullliteral ^^ { lit => NullLit() }
  }

  private lazy val unopexp: PackratParser[Exp] = positioned {
    (unop ~ expr(-1)) ^^ { case op ~ exp => UnOpExp(op, exp) }
  }

  private lazy val typeannotation: PackratParser[Type] = positioned {
    (LEFT_PAREN() ~ repsep(typeannotation, COMMA()) ~ RIGHT_PAREN() ~ ARROW() ~! typeannotation) ^^ {
      case _ ~ t1 ~ _ ~ _ ~ t2 => FunType(t1, t2)
    } |
      (simpletypeannotation ~ ARROW() ~! typeannotation) ^^ {
        case t1 ~ _ ~ t2 => FunType(List(t1), t2)
      } |
      (LEFT_PAREN() ~ typeannotation ~ COMMA() ~ rep1sep(typeannotation, COMMA()) ~ RIGHT_PAREN()) ^^ {
        case _ ~ t ~ _ ~ ts ~ _ => TupleType(t :: ts)
      } |
      (LEFT_PAREN() ~ typeannotation ~ RIGHT_PAREN()) ^^ {
        case _ ~ t ~ _ => t
      } |
      (LEFT_PAREN() ~ RIGHT_PAREN()) ^^ {
        _ => TupleType(Nil) } |
      simpletypeannotation
  }

  private lazy val simpletypeannotation: PackratParser[Type] = positioned {
    simpletype ^^ {
      t => t.str match {
        case "Int" => IntType()
        case "String" => StringType()
        case "Boolean" => BoolType()
        case "Float" => FloatType()
        case "Unit" => TupleType(Nil)
        case "Null" => NullType()
      }
    } |
      identifier ^^ (id => ClassNameType(id.str).setPos(id.pos))
  }

  private def binop(antiPrecedence: Int): PackratParser[BinOp] = positioned {
    antiPrecedence match {
      case 0 =>
        mult | div | modulo
      case 1 =>
        plus | minus
      case 2 =>
        lt | lteq
      case 3 =>
        equalequal
      case 4 =>
        and
      case 5 =>
        or
      case 6 =>
        max
    }
  }

  private lazy val unop: PackratParser[UnOp] = positioned {
    neg | not
  }

  private lazy val simpletype: PackratParser[SIMPLE_TYPE] = accept("simple type", { case t: SIMPLE_TYPE => t })

  private lazy val strliteral: PackratParser[STRING] = accept("string literal", { case lit: STRING => lit })

  private lazy val intliteral: PackratParser[INT] = accept("int literal", { case lit: INT => lit })

  private lazy val boolliteral: PackratParser[BOOL] = accept("boolean literal", { case lit: BOOL => lit })

  private lazy val floatliteral: PackratParser[FLOAT] = accept("float literal", { case lit: FLOAT => lit })

  private lazy val nullliteral: PackratParser[NULL] = accept("null literal", { case lit: NULL => lit })

  private lazy val identifier: PackratParser[IDENTIFIER] = accept("identifier", { case id: IDENTIFIER => id })

  private lazy val plus: PackratParser[BinOp] = OP("+") ^^ { _ => PlusBinOp() }

  private lazy val minus: PackratParser[BinOp] = OP("-") ^^ { _ => MinusBinOp() }

  private lazy val mult: PackratParser[BinOp] = OP("*") ^^ { _ => MultBinOp() }

  private lazy val div: PackratParser[BinOp] = OP("/") ^^ { _ => DivBinOp() }

  private lazy val equalequal: PackratParser[BinOp] = OP("==") ^^ { _ => EqualBinOp() }

  private lazy val and: PackratParser[BinOp] = OP("&") ^^ { _ => AndBinOp() }

  private lazy val or: PackratParser[BinOp] = OP("|") ^^ { _ => OrBinOp() }

  private lazy val max: PackratParser[BinOp] = OP("max") ^^ { _ => MaxBinOp() }

  private lazy val lt: PackratParser[BinOp] = OP("<") ^^ { _ => LessThanBinOp() }

  private lazy val modulo: PackratParser[BinOp] = OP("%") ^^ { _ => ModuloBinOp() }

  private lazy val lteq: PackratParser[BinOp] = OP("<=") ^^ { _ => LessThanOrEqualBinOp() }

  private lazy val neg: PackratParser[UnOp] = OP("-") ^^ { _ => NegUnOp() }

  private lazy val not: PackratParser[UnOp] = OP("!") ^^ { _ => NotUnOp() }

  private def parseTokens(tokens: Seq[MiniScalaToken]): Exp =
    prog(new MiniScalaTokenReader(tokens)) match {
      case NoSuccess(_, next) =>
        throw new SyntaxError(next.pos)
      case Success(result, _) =>
        result
    }

  def parse(code: String): Exp =
    parseTokens(Lexer(code))

  def readFile(path: String): String = {
    try {
      val f = Source.fromFile(new File(path))
      try {
        f.mkString
      } finally {
        f.close()
      }
    } catch {
      case e: IOException =>
        throw new MiniScalaError(s"Unable to read file ${e.getMessage}", NoPosition)
    }
  }

  class SyntaxError(pos: Position) extends MiniScalaError(s"Syntax error", pos)

  class MiniScalaTokenReader(tokens: Seq[MiniScalaToken]) extends Reader[MiniScalaToken] {

    override def first: MiniScalaToken = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = tokens.headOption.map(_.pos).getOrElse(NoPosition)

    override def rest: Reader[MiniScalaToken] = new MiniScalaTokenReader(tokens.tail)
  }
}
