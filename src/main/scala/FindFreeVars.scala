/**
 * Copyright (C) 2013 Carnegie Mellon University
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import reflect.macros.blackbox.Context
import language.experimental.macros

object FreeVarsBruteForceFinder {
  def extractParentValDefs(c: Context)(
    tree: c.universe.Tree, targetSymbol: c.Symbol
  ): (List[(String, c.universe.TreeApi)], Boolean) = {
    import c.universe._
    var gDefs = List[(String, TreeApi)]()
    var gFound = false

    /**
     * A traverser which extracts all ValDef nodes from the AST,
     * which are ancestors of the node which hast the symbol targetSymbol.
     */
    class C extends Traverser {
      //Traverse each child tree, remember wheter we already found
      //our target symbol.
      def traverseChildTrees(trees: List[Tree], include: Boolean): Boolean = {
        trees.foldLeft(false) { (found, subtree) =>
          found | traverseChildTree(subtree, include)
        }
      }

      //Traverse a single child tree.
      //If the child tree, contains our target, we remember all
      //ValDefs from the child tree and mark this node as ancestor too.
      def traverseChildTree(tree: Tree, include: Boolean): Boolean = {
        val (defs, found) = extractParentValDefs(c)(tree, targetSymbol)
        if (found || include) { gDefs = defs ::: gDefs }
        if (found) { gFound = true }
        found
      }

      //Traverse the current tree.
      //Check whether we found the target. If so, stop traversion.
      //If not, extract all relevant child trees.
      override def traverse(tree: Tree): Unit = {
        if (targetSymbol == tree.symbol) { gFound = true }

        tree match {
          case expr @ ValDef(_, name, _, subtree) =>
            //We fund a val def.
            gDefs = (name.toString(), expr) :: gDefs
            super.traverse(subtree)
          case expr @ Bind(name, _) =>
            //We found a bind from a case/match. This is also important to
            //remember.
            gDefs = (name.toString(), expr) :: gDefs
          case Block(trees, tree) => traverseChildTrees(tree :: trees, false)
          case Function(params, subtree) =>
            //Special case: If our target is in the subtree
            //of a function call, we also have to include the
            //params of our function in the case.
            traverseChildTrees(params, traverseChildTree(subtree, false))
          case CaseDef(valdef, _, matchexpr) =>
            //Special case: Pattern matching. Handle it similar as function.
            traverseChildTree(valdef, traverseChildTree(matchexpr, false))
          case _ => super.traverse(tree)
        }
      }
    }
    (new C).traverse(tree)
    (gDefs, gFound)
  }

  def extractIdentTerms(c: Context)(tree: c.universe.Tree) = {
    import c.universe._
    var idents = List[(Tree, String)]()

    class C extends Traverser {
      override def traverse(tree: Tree): Unit = tree match {
        case ident @ Ident(name) if !ident.symbol.isMethod =>
          idents = (tree, name.toString) :: idents
        case _ => super.traverse(tree)
      }
    }
    (new C).traverse(tree)
    idents
  }

  /**
    * Finds free variables within an anonymous function, which
    * are bound from an outer scope.
    *
    * Static or class variables are not found.
    */
  def locals(c: Context)(func: c.Tree) = {
    import c.universe._

    //Extract all Idents from our function
    val identTerms = extractIdentTerms(c)(func)

    //Only keep each symbol once, also filter out packes and so on.
    val filteredTerms = identTerms.filter(x => {
      !x._1.symbol.isPackage &&
      !x._1.symbol.isMethod &&
      !x._1.symbol.isModule &&
      !x._1.symbol.isClass &&
      !x._1.symbol.isType &&
      x._2 != "_" //Exclude blank.
    })

    //Check if all instances of term are really free
    filteredTerms.filter((x) => {
      //For each ident, look for a parent ValDef in our own function.
      //If we define this val ourself, drop it.
      val defs = extractParentValDefs(c)(func, x._1.symbol)._1
      defs.find(y => x._2 == y._1).isEmpty
    }).groupBy(x => x._2).map(x => x._2.head).toList
  }
}
