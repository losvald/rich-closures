import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.whitebox.{Context => WContext}

object SerializationUtil {
  trait RichClosure {
    type Function
    val f: Function
    val freeRefVals: List[Any]
  }

  // case class RichClosure1[-T, +R](
  //   val f: ???, // TODO(med-prio) how to let f have type <: Function1[T, R]?
  //   val freeRefVals: List[Any])
  trait RichClosure1[-T, +R]
      extends (T => R) with RichClosure {
    type Function <: Function1[T, R]
    override def apply(x: T): R = f.apply(x)
  }

  def mkFreeRefs(ts: Any*): List[String] = macro mkFreeRefsImpl
  def mkFreeRefsImpl(c: WContext)(ts: c.Tree*) = {
    import c.universe._
    val fullNames = ts.map(_ match {
      // case ident: Ref => ident.symbol.fullName // this is too specific
      case ref @ RefTree(qual, _) if qual.isTerm => ref.symbol.fullName
      case Block(_, Function(_, Apply(ref @ RefTree(_, _), _))) if ref.isTerm =>
        ref.symbol.fullName
      case t =>
        // val t = t.find { _ match { case Ident(
        c.abort(c.enclosingPosition, "not (selection of) ident.: " + showRaw(t))
    })
    q"""List(..$fullNames)"""
  }

  def mkFreeRefsUnsafe(fullNames: String*): List[String] = fullNames.toList

  // Macro for extracting unbound names of a rich closure
  def freeRefNames(rc: RichClosure): List[String] = macro freeRefNamesImpl
  def freeRefNamesImpl(c: WContext)(rc: c.Tree) = {
    import c.universe._
    // TODO(hi-prio) if we store trees / symbols from blackbox.Universe#Context,
    // would it be possible to extract anything from them in a whitebox context?
    // q"""$rc.freeSyms.map(_.fullName)"""
    q"$rc.freeRefVals"
  }

  def fun0[R](body: => R): () => R = macro fun0Impl[R]
  def fun0Impl[R: c.WeakTypeTag](c: Context)(body: c.Tree) = {
    import c.universe._
    q"""() => $body"""
  }

  // Macro for extracting free identifiers from an 1-argument function
  // TODO(lo-prio) add wrappers for n-argument functions (macro is agnostic)
  def fun1[T, R](f: T => R): RichClosure1[T, R] = macro fun1Impl[T, R]
  def fun1Impl[T: c.WeakTypeTag, R: c.WeakTypeTag](c: Context {
    // TODO(lo-prio) not sure why I need this, inspired by workaround from:
    //   https://issues.scala-lang.org/browse/SI-8356
    type PrefixType = SerializationUtil.type
  })(f: c.Expr[T => R]) = {
    import c.universe._
    import org.kiama.output.PrettyPrinter._
    val pos = c.enclosingPosition
    debugln(s"\n@L${pos.line} fun1 in ${pos.source.path}:\n" +
      pretty(any(f.tree)) + "\n" +
      "raw: " + showRaw(f))

    lazy val freeSyms = findFreeRefs(c)(f.tree)
    lazy val freeRefs = freeSyms.map(_.fullName)
    lazy val freeRefsBF = FreeVarsBruteForceFinder.locals(c)(f.tree).map {
      _._1.symbol.fullName
    }

    val (tpeT, tpeR) = (c.weakTypeOf[T], c.weakTypeOf[R])
    c.Expr[c.prefix.value.RichClosure1[T, R]](q"""
      new RichClosure1[$tpeT, $tpeR] {
        override type Function = Function1[$tpeT, $tpeR]
        override val f = $f
        override val freeRefVals = $freeRefs
      }""")
  }

  def findFreeRefs(c: Context)(
    tree: c.universe.Tree
  ): List[c.universe.TermSymbol] = {
    import c.universe._
    // def isFree(sym: TermSymbol) = sym.isInstanceOf[FreeTermSymbolApi]

    val defSymSet = Set(findDefs(c)(tree): _*)
    debugln(s"bound: ${defSymSet}")
    val freeSyms = tree.collect {
      // TODO(lo-prio) see if some cases can be consolidated as follows:
      // 1) Collect (term) symbols from RefTree other than SelectFromTypeTree
      // 2) case ref @ RefTree(qual, _) if ref.isTerm && ... => ...
      case ident @ Ident(name)
          if ident.isTerm && !ident.symbol.isModule &&
          name != termNames.WILDCARD =>
        debugln(s"ID'FIER: ${showRaw(ident)} #${ident##}")
        ident.symbol.asTerm
      case select @ Select(qual, TermName(name))
          if select.isTerm && select.symbol.asTerm.isStable &&
          select.symbol.asTerm.isStatic && // TODO(med-prio) too conservative?
          !select.symbol.isModule =>
        val sym = select.symbol.asTerm
        debugln(s"SEL: ${showRaw(select)} name=$name meth=${sym.isMethod}")
        sym
      case select @ Select(ref @ (This(_) | Super(_)), _)
          if select.isTerm && select.symbol.isStatic &&
          !select.symbol.asTerm.isStable =>
        val sym = select.symbol.asTerm
        debugln(s"unstable static: ${showRaw(select)}")
        select.symbol.asTerm // TODO(hi-prio) catch ref's symbol instead?
    } filter { s =>
      // Note: we cannot filter out if s.isMethod == true because member val
      // have explicit setters or getters
      !(s.isPackage /* || TODO(high-prio) add more isXYZ? */) &&
      !defSymSet.contains(s) // exclude bound symbols
    } toSet // groupBy { s => s } map { _._1 } toList

    freeSyms.toList
  }

  private def findDefs(c: Context)(tree: c.universe.Tree) = {
    import c.universe._
    tree match {
      case Function(_, _) | Block(_, Function(_, _)) =>
      case _ => c.abort(c.enclosingPosition, "unexpected AST: " + showRaw(tree))
    }

    tree.collect {
      case defn @ (DefDef(_, _, _, _, _, _) | ValDef(_, _, _, _)) =>
        defn.symbol.asTerm
      case bind @ Bind(name, body) => bind.symbol.asTerm
    }
  }

  // private def findValDefs((implicit c: Context)

  private val isDebugEnabled = System.getProperty(
    "cv.debug", "true").toBoolean // TODO(low-prio) change default to false
  def debugln(msg: => String): Unit =
    if (isDebugEnabled) println(msg)

  object TestOnly {
    implicit class DebuggableRichClosure(rc: RichClosure) {
      val freeRefs = rc.freeRefVals.asInstanceOf[List[String]].sorted
    }
  }
}
