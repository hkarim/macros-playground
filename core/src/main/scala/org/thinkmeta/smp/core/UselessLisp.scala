package org.thinkmeta.smp.core


import scala.util.parsing.combinator._
import language.higherKinds


/**
 * @author Hossam Karim
 */
trait Operand
trait Operator extends Operand

trait Literal extends Operand
trait Body extends Operand
case class Name(value: String) extends Operand
case class StringLiteral(value: String) extends Literal
case class IntegerLiteral(value: Integer) extends Literal
case class DoubleLiteral(value: Double) extends Literal
case class TypeName(value: String) extends Operand
case class ValueDef(name: Name, typeName: Option[TypeName], value: Operand) extends Body
case class Arg(name: Name, typeName: TypeName) extends Operand
case class Args(args: List[Arg]) extends Operand
case class FunApp(name: Name, actualArgs: List[Operand]) extends Operator
case class FunDef
(name: Name, typeName: Option[TypeName], formalArgs: Option[Args], body: List[Operand])
  extends Body
case class LambdaArg(name: Name, typeName: Option[TypeName]) extends Operand
case class LambdaDef(args: List[LambdaArg], body: Operand) extends Operand
case class CaseExpr(lhs: Operand, guard: Option[Operand], rhs: Operand) extends Operand
//case class Cases(defs: List[CaseDef]) extends Operand
case class Using(path: List[Name], wildcard: Boolean) extends Body
case class Module(name: Name, blocks: List[Body]) extends Body

object UselessLisp extends JavaTokenParsers {

  def path =
    ident ~ rep1("." ~> ident) ^^ {
      case n ~ ns ⇒ Name((n::ns).mkString("."))
    }

  def name =
      symbol    |
      path      |
      ident ^^ {
        case "if"         ⇒ Name("if$")
        case "do"         ⇒ Name("do$")
        case "chain"      ⇒ Name("__chain__")
        case "new"        ⇒ Name("__new__")
        case "match"      ⇒ Name("__match__")
        case s            ⇒ Name(s)
      }

  def symbol =
    "+" ^^^ Name("plus")            |
      "-" ^^^ Name("minus")         |
      "*" ^^^ Name("times")         |
      "." ^^^ Name("__chain__")


  def literal =
    wholeNumber ^^ {
      case i ⇒ IntegerLiteral(i.toInt)
    } |
      floatingPointNumber ^^ {
        case f ⇒ DoubleLiteral(f.toDouble)
      } |
      stringLiteral ^^ {
        case s ⇒ StringLiteral(s.stripPrefix("\"").stripSuffix("\""))
      }

  def any = "(" ~> operand <~ ")"

  def operand: Parser[Operand] =
    name | literal | valuedef | lambdadef | casedef | funapp | any

  def typename = ident ^^ {
    case s ⇒ TypeName(s)
  }

  def arg = name ~ ":" ~ typename ^^ {
    case name ~ _ ~ typeName ⇒ Arg(name,typeName)
  }

  def args = "[" ~> rep1(arg) <~ "]" ^^ {
    case argSeq ⇒ Args(argSeq)
  }

  def valuedef =
    "(" ~ "val" ~> ident ~ opt(":" ~> typename) ~ operand <~ ")" ^^ {
      case id ~ tp ~ value ⇒ ValueDef(Name(id),tp,value)
    }


  def funapp: Parser[FunApp] =
    "(" ~> name ~ rep(operand) <~ ")" ^^ {
      case name ~ operands ⇒ FunApp(name, operands)
    }

  def fundef =
    "(" ~ "defun" ~> name ~ opt(":" ~> typename) ~ opt(args) ~ rep1(operand) <~ ")" ^^ {
      case name ~ typename ~ args ~ operand ⇒ FunDef(name,typename,args,operand)
    }


  def lambdaarg = ident ~ opt(":" ~> typename) ^^ {
    case id ~ tn ⇒ LambdaArg(Name(id), tn)
  }

  def lambdasep = "|" | "->" | "→"

  def formallambda =
    "{" ~> rep1(lambdaarg) ~ lambdasep ~ operand <~ "}" ^^ {
      case args ~ _ ~ body ⇒ LambdaDef(args,body)
    }

  def inlinelambda =
    "#" ~ "(" ~> operand <~ ")"

  def lambdadef = formallambda

  def caselhs: UselessLisp.Parser[Operand] =
    literal |
      name    |
      (name ~ rep1(operand)) ^^ {case n ~ args ⇒ FunApp(n,args)} |
      funapp
  def casedef =
    "(" ~> caselhs ~ opt("if" ~> operand) ~ lambdasep ~ operand <~ ")" ^^ {
      case lhs ~ guard ~ _ ~ rhs ⇒ CaseExpr(lhs,guard,rhs)
    }

  def using = "using" ~> name ~ rep("::" ~> name) ~ opt("::" ~> "_") ^^ {
    case n ~ Nil ~ wildcard ⇒ Using(List(n), wildcard.nonEmpty)
    case n ~ ns ~ wildcard ⇒ Using(n :: ns, wildcard.nonEmpty)
  }

  def body = using | valuedef | fundef | module

  def module: Parser[Module] =
    "module" ~ name ~ "{" ~ rep(body) ~ "}" ^^ {
      case _ ~ name ~  _  ~ defs ~ _ ⇒ Module(name,defs)
    }

  def eval[T](expression:String)(p: Parser[T]) =
    parse(p,expression) match {
      case Success(result,_) ⇒ result
      case f@Failure(msg, _)   ⇒ throw new IllegalArgumentException(f.toString())
      case Error(msg, _)     ⇒ throw new IllegalArgumentException(msg)
    }

  def astModule(expression: String) = eval(expression)(module)
  def astFunApp(expression: String) = eval(expression)(funapp)
  def astFunDef(expression: String) = eval(expression)(fundef)
  def astLambdaDef(expression: String) = eval(expression)(lambdadef)
  def astName(expression: String) = eval(expression)(name)
  def astValueDef(expression: String) = eval(expression)(valuedef)

}

object StdLib {

  def if$[T](condition: ⇒ Boolean, then$: ⇒ T, else$: ⇒ T): T =
    if(condition) then$ else else$

  def loop[T](from:Int,to:Int,apply: Int ⇒ T) =
    for (i ← from to to) apply(i)

  def gt[A: Numeric](lhs:A, rhs: A) = implicitly[Numeric[A]].gt(lhs,rhs)
  def ge[A: Numeric](lhs:A, rhs: A) = implicitly[Numeric[A]].gteq(lhs,rhs)
  def lt[A: Numeric](lhs:A, rhs: A) = implicitly[Numeric[A]].lt(lhs,rhs)
  def le[A: Numeric](lhs:A, rhs: A) = implicitly[Numeric[A]].lteq(lhs,rhs)
  def eq[A: Numeric](lhs:A, rhs: A) = implicitly[Numeric[A]].compare(lhs,rhs) == 0
  def ne[A: Numeric](lhs:A, rhs: A) = !eq(lhs,rhs)


  def plus[A: Numeric](as: A*) =
    as.reduce(implicitly[Numeric[A]].plus)
  def minus[A: Numeric](as: A*) =
    as.reduce(implicitly[Numeric[A]].minus)
  def times[A: Numeric](as: A*) =
    as.reduce(implicitly[Numeric[A]].times)


  def do$(block: Any*): Unit = block.foreach(b ⇒ b)

  /*
    (val l (List 1 2 3 4))
    // l.map(_+1).reduce(_+_)

    (chain l
      (.map [x| (+ x 1) )
      (.reduce [x y| (+ x y)]))
    (chain l (.map [x|(+ x 1)]) (.reduce [x y| (+ x y)]) )
   */


  object Lists {
    import scalaz._
    import std.list._
    def map[A,B](f: A ⇒ B, m: List[A]): List[B] =
      Traverse[List].map(m)(f)
  }


}

object ASTShow extends App {
  import UselessLisp._

  def showExpression(expression: String) = {
    val result = astFunApp(expression)
    s"$expression → $result"
  }
  def showName(expression: String) = {
    val result = astName(expression)
    s"$expression → $result"
  }
  def showLambda(expression: String) = {
    val result = astLambdaDef(expression)
    s"$expression → $result"
  }
  def showDefinition(expression: String) = {
    val result = astFunDef(expression)
    s"$expression → $result"
  }
  def showModule(expression: String) = {
    val result = astModule(expression)
    s"$expression → $result"
  }





}
