package org.thinkmeta.smp.core
import language.experimental.macros
import reflect.macros.Context


/**
 * @author Hossam Karim
 */
object HostedLisp {

  trait Target
  case object TermTreesTarget extends Target
  case object TypeTreesTarget extends Target
  case object CasesTarget extends Target
  case object PatternTarget extends Target

  def operand(c: Context)(e: Operand): c.Tree =
    e match {
      //casts are here to satisfy the IDE, the compiler doesn't need the casts
      case x: Name ⇒ name(c)(x).asInstanceOf[c.Tree]
      case x: IntegerLiteral ⇒ integerLiteral(c)(x).asInstanceOf[c.Tree]
      case x: DoubleLiteral ⇒ doubleLiteral(c)(x).asInstanceOf[c.Tree]
      case x: StringLiteral ⇒ stringLiteral(c)(x).asInstanceOf[c.Tree]
      case x: ValueDef ⇒ valdef(c)(x).asInstanceOf[c.Tree]
      case x: LambdaDef ⇒ lambdadef(c)(x).asInstanceOf[c.Tree]
      case x: LambdaArg ⇒ lambdaarg(c)(x).asInstanceOf[c.Tree]
      case x: CaseExpr ⇒ casedef(c)(x).asInstanceOf[c.Tree]
      case x: FunApp ⇒ funapp(c)(x).asInstanceOf[c.Tree]
    }

  def name(c: Context)(e: Name): c.Tree = {
    import c.universe._
    if(e.value.contains("."))
      e.value.split('.').toList match {
        case x :: y :: Nil ⇒
          Select(Ident(newTermName(x)), newTermName(y))
        case x :: y :: ys  ⇒
          ys.foldLeft(Select(Ident(newTermName(x)), newTermName(y))) {
            (acc,next) ⇒ Select(acc,newTermName(next))
          }
      }
    else
      q"${c.universe.newTermName(e.value)}"
  }


  def integerLiteral(c: Context)(e: IntegerLiteral): c.Tree = {
    import c.universe._
    q"${c.literal(e.value)}"
  }

  def doubleLiteral(c: Context)(e: DoubleLiteral): c.Tree = {
    import c.universe._
    q"${c.literal(e.value)}"
  }

  def stringLiteral(c: Context)(e: StringLiteral): c.Tree = {
    import c.universe._
    q"${c.literal(e.value)}"
  }

  def typeName(c: Context)(e: TypeName): c.Tree = {
    import c.universe._
    val t = e.value match {
      case "int"    ⇒ newTypeName("Int")
      case "double" ⇒ newTypeName("Double")
      case "string" ⇒ newTypeName("String")
      case tn       ⇒ newTypeName(tn)
    }
    q"$t"
  }

  def valdef(c: Context)(e: ValueDef): c.universe.ValDef = {
    import c.universe._
    val valName = newTermName(e.name.value)
    val valValue = operand(c)(e.value)
    e.typeName.map(t ⇒ typeName(c)(t)) match {
      case Some(tn) ⇒ q"val $valName:$tn = $valValue"
      case _        ⇒ q"val $valName = $valValue"
    }
  }
  def arg(c: Context)(e: Arg): c.universe.ValDef = {
    import c.universe._
    val argName = newTermName(e.name.value)
    val argType = typeName(c)(e.typeName)
    q"val $argName:$argType"
  }

  def args(c: Context)(e: Args): List[c.universe.ValDef] =
    e.args.map(a ⇒ arg(c)(a)).asInstanceOf[List[c.universe.ValDef]] // cast for the IDE only


  def lambdaarg(c: Context)(e: LambdaArg): c.universe.ValDef = {
    import c.universe._
    val argName = newTermName(e.name.value)
    e.typeName.map(typeName(c)(_)) match {
      case Some(tn) ⇒ q"val $argName:$tn"
      case _        ⇒ ValDef(NoMods, argName,TypeTree(), EmptyTree)
    }
  }

  def lambdadef(c: Context)(e: LambdaDef): c.Tree = {
    import c.universe._
    val args = e.args.map(lambdaarg(c)(_))
    val body = operand(c)(e.body)
    q"(..$args) ⇒ ($body)"
  }

  def casedef(c: Context)(e: CaseExpr): c.Tree = {
    import c.universe._

    def q2pq(q: c.Tree): c.Tree = q match {
      case Apply( n,args )   => Apply( n, args map(q ⇒ q2pq(q)) )
      case Ident( term )     => Bind(term, Ident(nme.WILDCARD))
      case x                 => x
    }

    val lhs = q2pq(operand(c)(e.lhs))
    val rhs = operand(c)(e.rhs)
    val guard = e.guard.map(operand(c)(_))
    guard match {
      case Some(op) ⇒ cq"$lhs if $op ⇒ $rhs"
      case None ⇒ cq"$lhs ⇒ $rhs"
    }
  }

  def selectorForNew(c: Context)(e: Name): c.Tree = {
    import c.universe._
    if(e.value.contains(".")) /*a.b.c...*/{
      val list = e.value.split('.').toList
      val init = list.init
      val last = list.last
      val selected = init match {
        case x :: y :: Nil ⇒
          Select(Ident(newTermName(x)), newTermName(y))
        case x :: y :: ys  ⇒
          ys.foldLeft(Select(Ident(newTermName(x)), newTermName(y))) {
            (acc,next) ⇒ Select(acc,newTermName(next))
          }
        case _ ⇒ throw new IllegalArgumentException(s"Can't get a selector for $e")
      }
      Select(selected, newTypeName(last))
    } else /* just a */{
      Ident(newTypeName(e.value))
    }
  }

  // A pure hack to support the dot syntax
  def chain(c: Context)(e: FunApp): c.Tree = {
    import c.universe._
    // (chain (List 1 2 3)
    //        (map [x| (+ x 1)])
    //        (reduce [x y| (+ x y)]))
    // →
    // {
    //   val $1 = List(1,2,3)
    //   val $2 = $1.map(_+1)
    //   val $3 = $2.reduce(_+_)
    //   $3
    // }
    val length = e.actualArgs.length
    if(length < 2)
      throw new IllegalArgumentException("chain takes at least 2 parameters")

    def compose(arg: Operand, prevValueDef: Option[ValueDef]) = {
      val prev = prevValueDef.map(_.name.value)
      val currentName = c.fresh()

      // transform the ast to prefix the name of current arg with 'prev.'
      val currentValueAst = (arg,prev) match {
        case (FunApp(Name(n),args), Some(p)) ⇒ FunApp(Name(p+"."+n),args)
        case (Name(n), Some(p))              ⇒ Name(p+"."+n)
        case x@(_, Some(p))                  ⇒
          throw new UnsupportedOperationException(s"Unsupported: $x")// probably literals
        case (x, _)                          ⇒ x
      }
      ValueDef(Name(currentName), None, currentValueAst)
    }


    val valsReversed =
      e.actualArgs.tail.foldLeft(List(compose(e.actualArgs.head, None))) {
        (acc,next) ⇒
          compose(next,Some(acc.head)) :: acc
      }
    val lastValue = newTermName(valsReversed.head.name.value)
    val vals = valsReversed.reverse.map(valdef(c)(_))
    q"""
    {
      ..$vals
      $lastValue
    }
    """
  }

  def new$(c: Context)(e: FunApp): c.Tree = {
    import c.universe._
    e.actualArgs match {
      case (n@Name(_)) :: Nil ⇒
        val clsname = selectorForNew(c)(n)
        q"new $clsname"
      case (n@Name(_)) :: ops ⇒
        val clsname = selectorForNew(c)(n)
        val ctrargs = ops.map(op ⇒ operand(c)(op))
        q"new $clsname(..$ctrargs)"
      case _ ⇒
        throw new IllegalArgumentException(s"Can't handle expression $e")
    }
  }

  def matchcase(c: Context)(e: FunApp): c.Tree = {
    import c.universe._
    e.actualArgs match {
      case head :: tail ⇒
        val headOp = operand(c)(head)
        val tailOp =
          tail
            .asInstanceOf[List[CaseExpr]]
            .map(casedef(c)(_))
            .collect { case x: CaseDef ⇒ x }
        Match(headOp, tailOp)

      case _ ⇒
        throw new IllegalArgumentException("Malformed match expression")
    }

  }

  // The compiler once said: Quasiquotes can only be used with literal strings
  /*
  def termTree(c: Context)(pattern: String*)(trees: c.Tree*): c.Tree = {
    import c.universe._
    StringContext(pattern:_*).q(trees:_*)
  }
  */

  def funapp(c: Context)(e: FunApp): c.Tree = {
    import c.universe._
    e.name.value match {
      case "__chain__"    ⇒ chain(c)(e)
      case "__new__"      ⇒ new$(c)(e)
      case "__match__"    ⇒ matchcase(c)(e)
      case _            ⇒
        val fname = name(c)(e.name)
        val args = e.actualArgs.map(op ⇒ operand(c)(op))
        q"$fname(..$args)"
    }
  }


  def fundef(c: Context)(e: FunDef): c.Tree = {
    import c.universe._
    val fname = newTermName(e.name.value)
    val tpe = e.typeName.map(tn ⇒ typeName(c)(tn))
    val fargs = e.formalArgs match {
      case Some(largs) ⇒ args(c)(largs)
      case None        ⇒ Nil
    }
    val fbody = e.body.map(operand(c)(_))

    tpe match {
      case Some(t) ⇒
        q"""
          def $fname(..$fargs):$t = {
            ..$fbody
          }
        """
      case None ⇒
        q"""
          def $fname(..$fargs) = {
            ..$fbody
          }
        """
    }

  }

  def using(c: Context)(e: Using): c.Tree = {
    import c.universe._
    val path = e.path.map(_.value) //++ (if(e.wildcard) List("_") else Nil)
    path match {
      case x :: Nil if e.wildcard ⇒
        val tx = newTermName(x)
        Import(
          Ident(tx),
          List(ImportSelector(nme.WILDCARD, 0, null, -1)))
      case x :: y :: Nil if e.wildcard ⇒
        val tx = newTermName(x)
        val ty = newTermName(y)
        Import(
          Select(Ident(tx), ty),
          List(ImportSelector(nme.WILDCARD, 11, null, -1)))
      case x :: y :: Nil ⇒
        val tx = newTermName(x)
        val ty = newTermName(y)
        Import(
          Ident(tx),
          List(ImportSelector(ty, 0, ty, 0)))
      case x :: y :: ys if e.wildcard ⇒
        val tx = newTermName(x)
        val ty = newTermName(y)
        Import(
          ys.foldLeft(Select(Ident(tx), ty))( (acc,next) ⇒
            Select(acc,newTermName(next))),
          List(ImportSelector(nme.WILDCARD, 0, null, -1)))
      case l@(_::_) ⇒
        val (x::y::ys) = l.init
        val tx = newTermName(x)
        val ty = newTermName(y)
        val tz = newTermName(l.last)
        Import(
          ys.foldLeft(Select(Ident(tx), ty))( (acc,next) ⇒
            Select(acc,newTermName(next))),
          List(ImportSelector(tz, 0, tz, 0)))
    }

  }

  def moduledef(c: Context)(e: Module): c.Tree = {
    import c.universe._
    def mkModule(module: Module, topLevel: Boolean = false): c.Tree = {
      val functions = module.blocks.collect {
        case f: FunDef ⇒ fundef(c)(f)
      }
      val values = module.blocks.collect {
        case v: ValueDef ⇒ valdef(c)(v)
      }
      val mname = newTermName(module.name.value)
      val innerModules: List[Module] =
        module.blocks.collect {case m: Module ⇒ m}
      val imports = module.blocks.collect {case u: Using ⇒ u}.map(using(c)(_))
      val inner = innerModules.map(m ⇒ mkModule(m))
      val objectTree =
        q"""
          object $mname {
            import org.thinkmeta.smp.core.StdLib._
            ..$imports
            ..$values
            ..$functions
            ..$inner
          }
        """
      if(topLevel)
        q"""
          $objectTree
          $mname
        """
      else
        objectTree
    }
    mkModule(e, topLevel = true)
  }

  // hack to get around the compiler error:
  // reflective toolbox has failed: cannot operate on trees that are already typed
  // Context.eval(expression) did not work!
  def extractString(c: Context)(e: c.Expr[String]): String = {
    import c.universe._
    val definition: List[String] =
      e.tree.collect { case Literal(Constant(s: String)) ⇒ s }
    if(definition.headOption.isEmpty)
      throw new IllegalArgumentException("Empty Definition")
    definition.head
  }

  def functionm(c: Context)(e: c.Expr[String]): c.Expr[AnyRef] = {
    import c.universe._
    val code = extractString(c)(e)
    val funast = UselessLisp.astFunDef(code)
    val compiled = fundef(c)(funast)
    val objectName = newTermName(c.fresh())
    val expr =
      q"""
        import org.thinkmeta.smp.core.StdLib._
        object $objectName{ $compiled }
        $objectName
      """
    c.Expr(expr)
  }

  def modulem(c: Context)(e: c.Expr[String]): c.Expr[AnyRef] = {
    import c.universe._
    val code = extractString(c)(e)
    val module = UselessLisp.astModule(code)
    val expr = moduledef(c)(module)
    c.Expr(expr)
  }

  def module(e: String) = macro modulem
  def function(e: String) = macro functionm





}
