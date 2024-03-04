package miniscala

import miniscala.Ast._
import miniscala.Unparser.unparse

import scala.util.parsing.input.Position

/**
  * Type checker for MiniScala.
  */
object TypeChecker {

  type TypeEnv = Map[Id, Type]

  type ClassTypeEnv = Map[Id, StaticClassType]

  case class RefType(thetype: Type) extends Type

  case class StaticClassType(srcpos: Position, params: List[FunParam], membertypes: TypeEnv, ctenv: ClassTypeEnv, classes: List[ClassDecl]) extends Type

  val unitType: Type = TupleType(Nil)

  def typeCheck(e: Exp, tenv: TypeEnv, ctenv: ClassTypeEnv): Type = e match {
    case IntLit(_) => IntType()
    case BoolLit(_) => BoolType()
    case FloatLit(_) => FloatType()
    case StringLit(_) => StringType()
    case NullLit() => NullType()
    case VarExp(x) => tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)) match {
      case RefType(thetype) => thetype
      case t: Type => t
    }
    case BinOpExp(leftexp, op, rightexp) =>
      val lefttype = typeCheck(leftexp, tenv, ctenv)
      val righttype = typeCheck(rightexp, tenv, ctenv)
      op match {
        case PlusBinOp() =>
          (lefttype, righttype) match {
            case (IntType(), IntType()) => IntType()
            case (FloatType(), FloatType()) => FloatType()
            case (IntType(), FloatType()) => FloatType()
            case (FloatType(), IntType()) => FloatType()
            case (StringType(), StringType()) => StringType()
            case (StringType(), IntType()) => StringType()
            case (StringType(), FloatType()) => StringType()
            case (IntType(), StringType()) => StringType()
            case (FloatType(), StringType()) => StringType()
            case _ => throw new TypeError(s"Type mismatch at '+', unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
          }
        case MultBinOp() => (lefttype, righttype) match {
          case (StringType(), IntType()) => StringType()
          case (IntType(), IntType()) => IntType()
          case (FloatType(), FloatType()) => FloatType()
          case (IntType(), FloatType()) => FloatType()
          case (FloatType(), IntType()) => FloatType()
        }
        case MinusBinOp() | DivBinOp() | ModuloBinOp() | MaxBinOp() => (lefttype, righttype) match {
          case (IntType(), IntType()) => IntType()
          case (FloatType(), FloatType()) => FloatType()
          case (IntType(), FloatType()) => FloatType()
          case (FloatType(), IntType()) => FloatType()
          case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
        }
        case EqualBinOp() => BoolType()
        case LessThanBinOp() | LessThanOrEqualBinOp() => (lefttype, righttype) match {
          case (IntType(), IntType()) => BoolType()
          case (FloatType(), FloatType()) => BoolType()
          case (IntType(), FloatType()) => BoolType()
          case (FloatType(), IntType()) => BoolType()
          case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
        }
        case AndBinOp() | OrBinOp() => (lefttype, righttype) match {
          case (BoolType(), BoolType()) => BoolType()
          case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(lefttype)} and ${unparse(righttype)}", op)
        }
      }
    case UnOpExp(op, exp) =>
      val expType = typeCheck(exp, tenv, ctenv)
      (op, expType) match {
      case (NegUnOp(), IntType()) => IntType()
      case (NegUnOp(), FloatType()) => FloatType()
      case (NotUnOp(), BoolType()) => BoolType()
      case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(expType)}", op)
    }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val condType = typeCheck(condexp, tenv, ctenv)
      condType match {
      case BoolType() =>
        val thenType = typeCheck(thenexp, tenv, ctenv)
        val elseType = typeCheck(elseexp, tenv, ctenv)
        if (thenType == elseType) thenType
        else throw new TypeError(s"Type mismatch, unexpected types ${unparse(thenType)} and ${unparse(elseType)}", e)
      case _ => throw new TypeError(s"Type mismatch, unexpected types ${unparse(condType)}", e)
    }
    case BlockExp(vals, vars, defs, classes, exps) =>
      var tenv1 = tenv
      for (d <- vals) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> ot.getOrElse(t))
      }
      for (d <- vars) {
        val t = typeCheck(d.exp, tenv1, ctenv)
        val ot = getType(d.opttype, ctenv)
        checkSubtype(t, ot, d)
        tenv1 = tenv1 + (d.x -> RefType(d.opttype.getOrElse(t)))
      }
      var tenv2 = tenv1
      for (d <- defs) {
        val FunType(paramtypes, restype) = getType(getFunType(d), ctenv)
        tenv2 = tenv2 + (d.fun -> FunType(paramtypes, restype))
        for ((p, t) <- d.params.zip(paramtypes)) {
          tenv2 += (p.x -> t)
        }
      }
      for (d <- defs) {
        val t = typeCheck(d.body, tenv2, ctenv)
        val ot = getType(d.optrestype, ctenv)
        checkSubtype(t, ot, d)
      }
      val ctenv1 = classes.foldLeft(ctenv)((acc, c) => acc + (c.klass -> getClassType(c, ctenv, classes)))
      classes.foreach(c => {
        val tenv3 = c.params.foldLeft(tenv2)((acc, p) =>
          acc + (p.x -> getType(p.opttype, ctenv1).getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", c))))
        typeCheck(c.body, tenv3, ctenv1)
      })
      var resType = unitType
      for (e <- exps) {
        resType = typeCheck(e, tenv2, ctenv1)
      }
      resType
    case TupleExp(exps) => TupleType(exps.map(x => typeCheck(x, tenv, ctenv)))
    case MatchExp(exp, cases) =>
      val exptype = typeCheck(exp, tenv, ctenv)
      exptype match {
        case TupleType(ts) =>
          for (c <- cases) {
            if (ts.length == c.pattern.length) {
              var tenv1 =  tenv
              for ((x, v) <- c.pattern.zip(ts)) {
                tenv1 += (x -> v)
              }
              return typeCheck(c.exp, tenv1, ctenv)
            }
          }
          throw new TypeError(s"No case matches type ${unparse(exptype)}", e)
        case _ => throw new TypeError(s"Tuple expected at match, found ${unparse(exptype)}", e)
      }
    case CallExp(funexp, args) =>
      val funTypes = typeCheck(funexp, tenv, ctenv)
      funTypes match {
        case FunType(paramtypes, restype) =>
          if (args.length != paramtypes.length) throw new TypeError(s"Expected ${paramtypes.length} argument(s), but received ${args.length}", e)
          for ((a, t) <- args.zip(paramtypes)) {
            val typ = typeCheck(a, tenv, ctenv)
            checkSubtype(typ, Some(t), a)
          }
          restype
        case _ => throw new TypeError(s"$funTypes is not a function type", e)
      }
    case LambdaExp(params, body) =>
      var tenv1 = tenv
      for (p <- params) {
        tenv1 += (p.x -> getType(p.opttype, ctenv).getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p)))
      }
      val bodyType = typeCheck(body, tenv1, ctenv)
      FunType(params.map(p => getType(p.opttype, ctenv).getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))), bodyType)
    case AssignmentExp(x, exp) =>
      tenv.getOrElse(x, throw new TypeError(s"Unknown identifier '$x'", e)) match {
        case RefType(thetype) => checkSubtype(typeCheck(exp, tenv, ctenv), Some(thetype), exp)
          unitType
        case _ => throw new TypeError(s"'$x' is not a mutable variable.", e)
      }
    case WhileExp(cond, body) =>
      checkSubtype(typeCheck(cond, tenv, ctenv), Some(BoolType()), cond)
      typeCheck(body, tenv, ctenv)
      unitType
    case NewObjExp(klass, args) =>
      val classType = ctenv.getOrElse(klass, throw new TypeError(s"No class called $klass", e))
      if (args.length != classType.params.length) throw new TypeError(s"Expected ${classType.params.length} argument(s), but received ${args.length}", e)
      val ctenv1 = rebindClasses(classType.ctenv, classType.classes)
      val paramTypes = classType.params.map(x => getType(x.opttype, ctenv1))
      args.zip(paramTypes).foreach(p => checkSubtype(typeCheck(p._1, tenv, ctenv), p._2, e))
      classType
    case LookupExp(objexp, member) =>
      typeCheck(objexp, tenv, ctenv) match {
        case StaticClassType(_, _, membertypes, ctenv, classes) =>
          val ctenv1 = rebindClasses(ctenv, classes)
          getType(membertypes.getOrElse(member, throw new TypeError(s"No such member: $member", e)), ctenv1)
        case _ => throw new TypeError(s"The expression $objexp is not a class type", e)
      }
  }

  def rebindClasses(ctenv: ClassTypeEnv, classes: List[ClassDecl]): ClassTypeEnv = {
    classes.foldLeft(ctenv)((acc, c) => acc + (c.klass -> getClassType(c, ctenv, classes)))
  }

  /**
   * Checks whether `t1` is a subtype of `t2`.
   */
  def subtype(t1: Type, t2: Type): Boolean = {
    if (t1 == t2) return true
    (t1, t2) match {
      case (IntType(), FloatType()) => true
      case (NullType(), StaticClassType(_, _, _, _, _)) => true
      case (TupleType(list1), TupleType(list2)) =>
        if (list1.length != list2.length) return false
        list1.zip(list2).forall(t => subtype(t._1, t._2))
      case (FunType(params1, res1), FunType(params2, res2)) =>
        if (params1.length != params2.length) return false
        params1.zip(params2).forall(p => subtype(p._2, p._1)) && subtype(res1, res2)
      case (_, _) => false
    }
  }

  /**
   * Checks whether `t1` is a subtype of `t2`, generates type error otherwise.
   */
  def checkSubtype(t1: Type, t2: Type, n: AstNode): Unit =
    if (!subtype(t1, t2)) throw new TypeError(s"Type mismatch: type ${unparse(t1)} is not subtype of ${unparse(t2)}", n)

  /**
   * Checks whether `t1` is a subtype of `ot2` (if present), generates type error otherwise.
   */
  def checkSubtype(t: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) => checkSubtype(t, t2, n)
    case None => // do nothing
  }

  /**
    * Returns the type described by the type annotation `t`.
    * Class names are converted to proper types according to the class type environment `ctenv`.
    */
  def getType(t: Type, ctenv: ClassTypeEnv): Type = t match {
    case ClassNameType(klass) => ctenv.getOrElse(klass, throw new TypeError(s"Unknown class '$klass'", t))
    case ClassNameType(klass) => ctenv.getOrElse(klass, throw new TypeError(s"Unknown class '$klass'", t))
    case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
    case TupleType(ts) => TupleType(ts.map(tt => getType(tt, ctenv)))
    case FunType(paramtypes, restype) => FunType(paramtypes.map(tt => getType(tt, ctenv)), getType(restype, ctenv))
    case _ => throw new RuntimeException(s"Unexpected type $t") // this case is unreachable...
  }

  /**
    * Returns the type described by the optional type annotation `ot` (if present).
    */
  def getType(ot: Option[Type], ctenv: ClassTypeEnv): Option[Type] = ot.map(t => getType(t, ctenv))

  /**
    * Returns the function type for the function declaration `d`.
    */
  def getFunType(d: DefDecl): FunType =
    FunType(d.params.map(p => p.opttype.getOrElse(throw new TypeError(s"Type annotation missing at parameter ${p.x}", p))),
      d.optrestype.getOrElse(throw new TypeError(s"Type annotation missing at function result ${d.fun}", d)))

  /**
    * Returns the class type for the class declaration `d`.
    */
  def getClassType(d: ClassDecl, ctenv: ClassTypeEnv, classes: List[ClassDecl]): StaticClassType = {
    var membertypes: TypeEnv = Map()
    for (m <- d.body.vals)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.vars)
      membertypes = membertypes + (m.x -> m.opttype.getOrElse(throw new TypeError(s"Type annotation missing at field ${m.x}", m)))
    for (m <- d.body.defs)
      membertypes = membertypes + (m.fun -> getFunType(m))
    StaticClassType(d.pos, d.params, membertypes, ctenv, classes)
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new TypeError(s"Type mismatch: expected type ${unparse(t2)}, found type ${unparse(t1)}", n)
    case None => // do nothing
  }

  /**
    * Builds an initial type environment, with a type for each free variable in the program.
    */
  def makeInitialTypeEnv(program: Exp): TypeEnv = {
    var tenv: TypeEnv = Map()
    for (x <- Vars.freeVars(program))
      tenv = tenv + (x -> IntType())
    tenv
  }

  /**
    * Exception thrown in case of MiniScala type errors.
    */
  class TypeError(msg: String, node: AstNode) extends MiniScalaError(s"Type error: $msg", node.pos)
}
