import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.macros.whitebox.{Context => WContext}

object SerializationUtil {
  trait RichClosure {
    type Function
    val f: Function
    val freeRefs: List[Any]
  }

  // case class RichClosure1[-T, +R](
  //   val f: ???, // TODO(med-prio) how to let f have type <: Function1[T, R]?
  //   val freeRefs: List[Any])
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
      case t =>
        // val t = t.find { _ match { case Ident(
        c.abort(c.enclosingPosition, "not (selection of) ident.: " + showRaw(t))
    })
    q"""List(..$fullNames)"""
  }

  // Macro for extracting unbound names of a rich closure
  def freeRefNames(rc: RichClosure): List[String] = macro freeRefNamesImpl
  def freeRefNamesImpl(c: WContext)(rc: c.Tree) = {
    import c.universe._
    // TODO(hi-prio) if we store trees / symbols from blackbox.Universe#Context,
    // would it be possible to extract anything from them in a whitebox context?
    // q"""$rc.freeSyms.map(_.fullName)"""
    q"$rc.freeRefs"
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

    def vparams2symbols(params: List[ValOrDefDef]) = params.map(_.symbol.asTerm)
    val boundSyms = f.tree match {
      case Function(vparams, body) =>
        debugln("FUN1: " + f)
        debugln("vparams: " + vparams)
        debugln("vparams symbol names: " + vparams.map(p => (p.symbol, p.name)))
        vparams2symbols(vparams)
      case Block(trees, Function(vparams, body)) =>
        // TODO(med-prio) consider trees in non-local var search
        debugln("FUN1: " + f)
        vparams2symbols(vparams)
      case _ =>
        c.abort(c.enclosingPosition, "unexpected AST: " + showRaw(f))
    }
    val freeSyms = findFreeRefs(c)(f.tree, boundSyms)
    val freeRefs = freeSyms.map(_.fullName)

    val (tpeT, tpeR) = (c.weakTypeOf[T], c.weakTypeOf[R])
    c.Expr[c.prefix.value.RichClosure1[T, R]](q"""
      new RichClosure1[$tpeT, $tpeR] {
        override type Function = Function1[$tpeT, $tpeR]
        override val f = $f
        override val freeRefs = $freeRefs
      }""")
  }

  def findFreeRefs(c: Context)(tree: c.universe.Tree,
    freeSyms: List[c.universe.TermSymbol]):
      List[c.universe.TermSymbol] = {
    import c.universe._
    var idents = List[TermSymbol]()
    var freeSymsSet = Set(freeSyms.toSeq: _*)
    tree.collect {
      case ref @ RefTree(qual, _) if qual.isTerm => ref.symbol.asTerm
      // case ident @ Ident(name) if ident.isTerm => ident.symbol.asTerm
    // } ++ tree.collect { case @Select(_, termName @ TermName(_)) => name
    } filter { s =>
      !(s.isMethod || s.isPackage /* TODO(high-prio) ... */) &&
      !freeSymsSet.contains(s)
    }
    // new Traverser {
    //   override def traverse(tree: Tree): Unit = tree match {
    //     case ident @ Ident => TODO
    // }
  }

  // private def findValDefs((implicit c: Context)

  private val isDebugEnabled = System.getProperty(
    "cv.debug", "true").toBoolean // TODO(low-prio) change default to false
  def debugln(msg: => String): Unit =
    if (isDebugEnabled) println(msg)
}
