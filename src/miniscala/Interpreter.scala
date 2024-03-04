package miniscala

import miniscala.Ast._
import miniscala.Unparser._

import scala.io.StdIn
import scala.util.parsing.input.Position

/**
  * Interpreter for MiniScala.
  */
object Interpreter {
  sealed abstract class Val
  case class IntVal(v: Int) extends Val
  case class BoolVal(v: Boolean) extends Val
  case class FloatVal(v: Float) extends Val
  case class StringVal(v: String) extends Val
  case class TupleVal(vs: List[Val]) extends Val
  case class ClosureVal(params: List[FunParam], optrestype: Option[Type], body: Exp, env: Env, defs: List[DefDecl], cenv: ClassEnv) extends Val

  case class RefVal(loc: Loc, opttype: Option[Type]) extends Val
  case class ObjRefVal(loc: Loc, opttype: Option[Type]) extends Val
  case class ObjectVal(members: Env) extends Val

  val unitVal: Val = TupleVal(Nil)

  case class Constructor(params: List[FunParam], body: BlockExp, env: Env, cenv: ClassEnv, classes: List[ClassDecl], srcpos: Position)

  case class DynamicClassType(srcpos: Position) extends Type

  type Env = Map[Id, Val]

  type ClassEnv = Map[Id, Constructor]

  type Sto = Map[Loc, Val]

  type Loc = Int

  def nextLoc(sto: Sto): Loc = sto.size

  def eval(e: Exp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Sto) = e match {
    case IntLit(c) => (IntVal(c), sto)
    case BoolLit(c) => (BoolVal(c), sto)
    case FloatLit(c) => (FloatVal(c), sto)
    case StringLit(c) => (StringVal(c), sto)
    case NullLit() => (ObjRefVal(-1, None), sto)
    case VarExp(x) => (getValue(env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)), sto), sto)
    case BinOpExp(leftexp, op, rightexp) =>
      val (leftval, sto1) = eval(leftexp, env, cenv, sto)
      val (rightval, sto2) = eval(rightexp, env, cenv, sto1)
      op match {
        case PlusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 + v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 + v2), sto2)
            case (StringVal(v1), StringVal(v2)) => (StringVal(v1 + v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (StringVal(v1), FloatVal(v2)) => (StringVal(v1 + v2.toString), sto2)
            case (IntVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case (FloatVal(v1), StringVal(v2)) => (StringVal(v1.toString + v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '+', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MinusBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 - v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 - v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 - v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case MultBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 * v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 * v2), sto2)
            case (StringVal(v1), IntVal(v2)) => (StringVal(v1 * v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '*', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case DivBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 / v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 / v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 / v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '/', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case ModuloBinOp() =>
          if (rightval == IntVal(0) || rightval == FloatVal(0.0f))
            throw new InterpreterError(s"Division by zero", op)
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => (IntVal(v1 % v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => (FloatVal(v1 % v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => (FloatVal(v1 % v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at '%', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case EqualBinOp() => (leftval, rightval) match {
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 == v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 == v2), sto2)
          case _ => (BoolVal(leftval == rightval), sto2)
        }
        case LessThanBinOp() => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 < v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 < v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '<', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
        case LessThanOrEqualBinOp() => (leftval, rightval) match {
          case (IntVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (FloatVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (IntVal(v1), FloatVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case (FloatVal(v1), IntVal(v2)) => (BoolVal(v1 <= v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '<=', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
        case MaxBinOp() =>
          (leftval, rightval) match {
            case (IntVal(v1), IntVal(v2)) => if (v1 > v2) (IntVal(v1), sto2) else (IntVal(v2), sto2)
            case (FloatVal(v1), FloatVal(v2)) => if (v1 > v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case (IntVal(v1), FloatVal(v2)) => if (v1 > v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case (FloatVal(v1), IntVal(v2)) => if (v1 > v2) (FloatVal(v1), sto2) else (FloatVal(v2), sto2)
            case _ => throw new InterpreterError(s"Type mismatch at 'max', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
          }
        case AndBinOp() => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 & v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '&', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
        case OrBinOp() => (leftval, rightval) match {
          case (BoolVal(v1), BoolVal(v2)) => (BoolVal(v1 | v2), sto2)
          case _ => throw new InterpreterError(s"Type mismatch at '|', unexpected values ${valueToString(leftval)} and ${valueToString(rightval)}", op)
        }
      }
    case UnOpExp(op, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      op match {
        case NegUnOp() =>
          expval match {
            case IntVal(v) => (IntVal(-v), sto1)
            case FloatVal(v) => (FloatVal(-v), sto1)
            case _ => throw new InterpreterError(s"Type mismatch at '-', unexpected value ${valueToString(expval)}", op)
          }
        case NotUnOp() => expval match {
          case BoolVal(v1) => (BoolVal(!v1), sto)
          case _ => throw new InterpreterError(s"Type mismatch at '!', unexpected value ${valueToString(expval)}", op)
        }
      }
    case IfThenElseExp(condexp, thenexp, elseexp) =>
      val (expval, sto1) = eval(condexp, env, cenv, sto)
      expval match {
        case BoolVal(v1) => if (v1) eval(thenexp, env, cenv, sto1) else eval(elseexp, env, cenv, sto1)
        case _ => throw new InterpreterError(s"Type mismatch at 'if-else', unexpected value ${valueToString(expval)}", e)
      }
    case b: BlockExp =>
      val (res, _, sto1) = evalBlock(b, env, cenv, sto)
      (res, sto1)
    case TupleExp(exps) =>
      var (vals, sto1) = (List[Val](), sto)
      for (exp <- exps) {
        val (v, sto2) = eval(exp, env, cenv, sto1)
        vals = v :: vals
        sto1 = sto2
      }
      (TupleVal(vals.reverse), sto1)
    case MatchExp(exp, cases) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      expval match {
        case TupleVal(vs) =>
          for (c <- cases) {
            if (vs.length == c.pattern.length) {
              var env1 = env
              for ((x, v) <- c.pattern.zip(vs))
                env1 += (x -> v)
              return eval(c.exp, env1, cenv, sto1)
            }
          }
          throw new InterpreterError(s"No case matches value ${valueToString(expval)}", e)
        case _ => throw new InterpreterError(s"Tuple expected at match, found ${valueToString(expval)}", e)
      }
    case CallExp(funexp, args) =>
      trace("Evaluates the function expression which should be an identifier.")
      val (exp, sto1) = eval(funexp, env, cenv, sto)
      exp match {
        case ClosureVal(params, optrestype, body, closureEnv, defs, closureCenv) =>
          trace("Invokes the evalArgs function.")
          val (env1, sto2) = evalArgs(args, params, env, sto1, cenv, closureEnv, closureCenv, e)
          var env2 = env1
          trace("rebinds function declarations.")
          for (d <- defs) {
            env2 = env2 + (d.fun -> ClosureVal(d.params, d.optrestype, d.body, closureEnv, defs, cenv))
          }
          trace("Evaluates the functions body in the updated environment and store.")
          val (result, sto3) = eval(body, env2, cenv, sto2)
          trace("Type checks the result value.")
          val ot = getType(optrestype, cenv)
          checkValueType(result, ot, e)
          trace(s"Returns the resulting value and an updated store.")
          (result, sto3)
        case _ => throw new InterpreterError(s"'${valueToString(exp)}' is not an identifier", e)
      }
    case LambdaExp(params, body) => (ClosureVal(params, None, body, env, List[DefDecl](), cenv), sto)
    case AssignmentExp(x, exp) =>
      val (expval, sto1) = eval(exp, env, cenv, sto)
      env.getOrElse(x, throw new InterpreterError(s"Unknown identifier '$x'", e)) match {
        case RefVal(loc, opttype) =>
          checkValueType(expval, getType(opttype, cenv), exp)
          val sto2 = sto1 + (loc -> expval)
          (unitVal, sto2)
        case _ => throw new InterpreterError(s"'$x' is not a mutable variable", exp)
      }
    case WhileExp(cond, body) =>
      val (expval, sto1) = eval(cond, env, cenv, sto)
      expval match {
        case BoolVal(true) =>
          val (_, sto2) = eval(body, env, cenv, sto1)
          eval(WhileExp(cond, body), env, cenv, sto2)
        case BoolVal(false) => (unitVal, sto1)
        case _ => throw new InterpreterError(s"'${valueToString(expval)}' is not a boolean", cond)
      }
    case NewObjExp(klass, args) =>
      trace("Looks up the class and returns the constructor.")
      val c = cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class name '$klass'", e))
      trace("Invokes the rebindClasses function.")
      val declcenv1 = rebindClasses(c.env, c.cenv, c.classes)
      trace(s"Evaluates and type checks each argument and gets an updated environment and store.")
      val (declenv1, sto1) = evalArgs(args, c.params, env, sto, cenv, c.env, declcenv1, e)
      trace("Evaluates the body of the constructor.")
      val (_, env1, sto2) = evalBlock(c.body, declenv1, declcenv1, sto1)
      trace("Finds an unused location in the store.")
      val newloc = nextLoc(sto2)
      trace("Creates a new object environment which only has the fields and method names that are declared in the body of the constructor.")
      val objenv = (c.body.defs.map(d => (d.fun -> env1(d.fun))) ++ c.body.vars.map(d => (d.x -> env1(d.x))) ++ c.body.vals.map(d => (d.x -> env1(d.x)))).toMap
      trace("Updates the store such that the new location maps to the object environment.")
      val sto3 = sto2 + (newloc -> ObjectVal(objenv))
      trace("Returns a tuple with the location and, if specified, the type of the value that the location maps to, and an updated store.")
      (ObjRefVal(newloc, Some(DynamicClassType(c.srcpos))), sto3)
    case LookupExp(objexp, member) =>
      trace("Evaluates the object expression and returns a tuple with the location and, if specified, the type of the value that the location maps to, and an updated store.")
      val (objval, sto1) = eval(objexp, env, cenv, sto)
      trace("Checks that the location is an object location.")
      objval match {
        case ObjRefVal(loc, _) =>
          if (loc == -1) throw new InterpreterError("Null pointer exception", objexp)
          trace("Looks up the location in the store.")
          sto1(loc) match {
            case ObjectVal(members) =>
              trace("Looks up member in the found object environment and return the value and an updated store.")
              (getValue(members.getOrElse(member, throw new InterpreterError(s"No such member: $member", e)), sto1), sto1)
            case _ => throw new RuntimeException(s"Expected an object value") // (unreachable case)
          }
        case _ => throw new InterpreterError(s"Base value of lookup is not a reference to an object: ${valueToString(objval)}", e)
      }
  }

  /**
   * Evaluates the given block.
   * Returns the resulting value, the updated environment after evaluating all declarations, and the latest store.
   */
  def evalBlock(b: BlockExp, env: Env, cenv: ClassEnv, sto: Sto): (Val, Env, Sto) = {
    trace("Evaluates and type checks each val declaration and returns a updated environment and store.")
    var env1 = env
    var sto1 = sto
    for (d <- b.vals) {
      val (v, sto2) = eval(d.exp, env1, cenv, sto1)
      val ot = getType(d.opttype, cenv)
      checkValueType(v, ot, d)
      env1 = env1 + (d.x -> v)
      sto1 = sto2
    }
    trace("Evaluates and type checks each var declaration and returns a updated environment and store.")
    for (d <- b.vars) {
      val (expval, sto2) = eval(d.exp, env1, cenv, sto1)
      val ot = getType(d.opttype, cenv)
      checkValueType(expval, ot, d)
      val location = nextLoc(sto2)
      env1 = env1 + (d.x -> RefVal(location, d.opttype))
      sto1 = sto2 + (location -> expval)
    }
    trace("Evaluates each function declaration and updates the environment.")
    var env2 = env1
    for (d <- b.defs) {
      val ot = getType(d.optrestype, cenv)
      env2 = env2 + (d.fun -> ClosureVal(d.params, ot, d.body, env2, b.defs, cenv))
    }
    trace("Evaluates each class declaration and updates the class environment.")
    var cenv1 = cenv
    for (d <- b.classes)
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env2, cenv, b.classes, d.pos))
    var res: Val = unitVal
    trace("Evaluates each expression in the block and returns the last result in a tuple with the updated environment and store.")
    for (exp <- b.exps) {
      val (res1, sto2) = eval(exp, env2, cenv1, sto1)
      res = res1
      sto1 = sto2
    }
    (res, env2, sto1)
  }

  /**
   * Evaluates the arguments `args` in environment `env` with store `sto`,
   * extends the environment `declenv` with the new bindings, and
   * returns the extended environment and the latest store.
   */
  def evalArgs(args: List[Exp], params: List[FunParam], env: Env, sto: Sto, cenv: ClassEnv, declenv: Env, declcenv: ClassEnv, e: Exp): (Env, Sto) = {
    trace("Checks that the number of arguments and parameters are the same.")
    if (args.length != params.length) throw new InterpreterError("Wrong number of arguments at call/new", e)
    var (env1, sto1) = (declenv, sto)
    trace(s"Evaluates and type checks each argument to get an updated environment and store.")
    for ((p, arg) <- params.zip(args) ) {
      val (argval, sto2) = eval(arg, env, cenv, sto1)
      checkValueType(argval, getType(p.opttype, declcenv), arg)
      env1 = env1 + (p.x -> argval)
      sto1 = sto2
    }
    trace("Returns the updated environment and store.")
    (env1, sto1)
  }

  /**
   * Resolves reference values by looking up the referenced value in the store.
   */
  def getValue(v: Val, sto: Sto): Val = v match {
    case RefVal(loc, _) =>
      trace("Looks up the location in the store and returns the value at that location.")
      sto(loc)
    case _ =>
      trace(s"Returns the value ${valueToString(v)}.")
      v
  }

  /**
   * Rebinds `classes` in `cenv` to support recursive class declarations.
   */
  def rebindClasses(env: Env, cenv: ClassEnv, classes: List[ClassDecl]): ClassEnv = {
    var cenv1 = cenv
    trace("Rebinds the classes to allow recursion.")
    for (d <- classes)
      cenv1 = cenv1 + (d.klass -> Constructor(d.params, d.body, env, cenv, classes, d.pos))
    cenv1
  }

  /**
    * Returns the type described by the type annotation `ot` (if present).
    * Class names are converted to proper types according to the class environment `cenv`.
    */
  def getType(ot: Option[Type], cenv: ClassEnv): Option[Type] = ot.map(t => {
    def getType(t: Type): Type = t match {
      case ClassNameType(klass) => DynamicClassType(cenv.getOrElse(klass, throw new InterpreterError(s"Unknown class '$klass'", t)).srcpos)
      case IntType() | BoolType() | FloatType() | StringType() | NullType() => t
      case TupleType(ts) => TupleType(ts.map(getType))
      case FunType(paramtypes, restype) => FunType(paramtypes.map(getType), getType(restype))
      case _ => throw new RuntimeException(s"Unexpected type $t") // (unreachable case)
    }
    getType(t)
  })

  /**
    * Checks whether value `v` has type `ot` (if present), generates runtime type error otherwise.
    */
  def checkValueType(v: Val, ot: Option[Type], n: AstNode): Unit = ot match {
    case Some(t) =>
      (v, t) match {
        case (IntVal(_), IntType()) |
             (BoolVal(_), BoolType()) |
             (FloatVal(_), FloatType()) |
             (IntVal(_), FloatType()) |
             (StringVal(_), StringType()) => // do nothing
        case (TupleVal(vs), TupleType(ts)) if vs.length == ts.length =>
          for ((vi, ti) <- vs.zip(ts))
            checkValueType(vi, Some(ti), n)
        case (ClosureVal(cparams, optcrestype, _, _, _, cenv), FunType(paramtypes, restype)) if cparams.length == paramtypes.length =>
          for ((p, t) <- cparams.zip(paramtypes))
            checkTypesEqual(t, getType(p.opttype, cenv), n)
          checkTypesEqual(restype, getType(optcrestype, cenv), n)
        case (ObjRefVal(-1, _), _: DynamicClassType) => // do nothing
        case (ObjRefVal(_, Some(vd: DynamicClassType)), td: DynamicClassType) =>
           if (vd != td) {
            throw new InterpreterError(s"Type mismatch: object of type ${unparse(vd)} does not match type ${unparse(td)}", n)
          }
        case _ =>
          throw new InterpreterError(s"Type mismatch: value ${valueToString(v)} does not match type ${unparse(t)}", n)
      }
    case None => // do nothing
  }

  /**
    * Checks that the types `t1` and `ot2` are equal (if present), throws type error exception otherwise.
    */
  def checkTypesEqual(t1: Type, ot2: Option[Type], n: AstNode): Unit = ot2 match {
    case Some(t2) =>
      if (t1 != t2)
        throw new InterpreterError(s"Type mismatch: type ${unparse(t1)} does not match type ${unparse(t2)}", n)
    case None => // do nothing
  }

  /**
    * Converts a value to its string representation (for error messages).
    */
  def valueToString(v: Val): String = v match {
    case IntVal(c) => c.toString
    case FloatVal(c) => c.toString
    case BoolVal(c) => c.toString
    case StringVal(c) => c
    case TupleVal(vs) => vs.map(valueToString).mkString("(", ",", ")")
    case ClosureVal(params, _, exp, _, _, _) => // the resulting string ignores the result type annotation and the declaration environment
      s"<(${params.map(unparse).mkString(",")}), ${unparse(exp)}>"
    case ObjRefVal(-1, _) => "null"
    case ObjRefVal(loc, _) => s"object#$loc" // the resulting string ignores the type annotation
    case _ => throw new RuntimeException(s"Unexpected value $v") // (unreachable case)
  }

  /**
    * Builds an initial environment, with a value for each free variable in the program.
    */
  def makeInitialEnv(program: Exp): Env = {
    var env = Map[Id, Val]()
    for (x <- Vars.freeVars(program)) {
      print(s"Please provide an integer value for the variable $x: ")
      env = env + (x -> IntVal(StdIn.readInt()))
    }
    env
  }

  /**
   * Simplifies an expression if possible
   */
  def simplify(exp: Exp): Exp = exp match {
    case BinOpExp(lexp, op, rexp) =>
      val leftexp = simplify(lexp)
      val rightexp = simplify(rexp)
      op match {
      case PlusBinOp() =>
        if (leftexp == IntLit(0)) return rightexp
        if (rightexp == IntLit(0)) return leftexp
      case MinusBinOp() =>
        if (leftexp == IntLit(0)) return UnOpExp(NegUnOp(), rightexp)
        if (rightexp == IntLit(0)) return leftexp
        if (leftexp == rightexp) return IntLit(0)
      case MultBinOp() =>
        if (leftexp == IntLit(1)) return rightexp
        if (rightexp == IntLit(1)) return leftexp
        if (leftexp == IntLit(0) || rightexp == IntLit(0)) return IntLit(0)
      case DivBinOp() => if (rightexp == IntLit(1)) return leftexp
    }
      BinOpExp(leftexp, op, rightexp)
    case _ => exp
  }

  /**
    * Prints message if option -trace is used.
    */
  def trace(msg: String): Unit =
    if (Options.trace)
      println(msg)

  /**
    * Exception thrown in case of MiniScala runtime errors.
    */
  class InterpreterError(msg: String, node: AstNode) extends MiniScalaError(s"Runtime error: $msg", node.pos)
}
