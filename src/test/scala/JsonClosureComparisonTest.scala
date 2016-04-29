/*
 * Copyright (C) 2015 Leo Osvald (leo.osvald@gmail.com)
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

import org.json4s._, JsonDSL._
import native.{Serialization => JsonSer, _}
import native.JsonMethods.{pretty => jsonPretty, _}

import org.kiama.output.PrettyPrinter._

import scala.pickling.Defaults._, scala.pickling.json._
import scala.reflect.runtime.universe._

import utest._

object JsonClosureComparisonTest extends TestSuite { self =>
  override def utestTruncateLength = 2000

  def typeName[T : TypeTag] = typeOf[T].typeSymbol.name.toString

  val noDiff = Diff(JNothing, JNothing, JNothing)

  // Prettify JSON diffs (order-agnostic) in asserts using kiama library ;)
  implicit def diff2PrettyStr(diff: Diff): String =
    if (diff == noDiff) "" else pretty(any(diff))

  case class IntStr(i: Int, s: String)
  case class Nested(is: IntStr)

  class IntAndSelfFieldClass {
    val x = 42
    val self = this
  }

  def tests = this {
    import SerializationUtil._

    def toStr(jv: JValue) = compact(render(jv))
    def toPrettyStr(jv: JValue) = jsonPretty(render(jv))

    "primitives" - {
      val one = 1

      "singleton" - {
        val freeVals = fun1 { (s: String) => List(one, s) } freeRefVals

        assert(freeVals.size == 1) // sanity check
        val actual = "\\s+".r replaceAllIn(freeVals.head.pickle.value, "")
        assert(actual == """{"$type":"scala.Int","value":1}""")
      }

      "int and string" - {
        // TODO(med-prio): use fun0 after it is properly implemented
        val two = "two"
        val rc1RefVals = fun1 { (a0: Unit) => List(one, two) } freeRefVals
        val serFreeVals = rc1RefVals map { _.pickle.value }
        val diff = JArray(
          ("$type" -> "scala.Int") ~ ("value" -> 1) ::
            ("$type" -> "java.lang.String") ~ ("value" -> "two") :: Nil
        ) diff JArray(serFreeVals map { parse(_) })
        val diffStr: String = diff
        assert(diffStr.isEmpty, diff == noDiff)
      }
    }

    "case class" - {
      val actual = new IntStr(42, "foo").pickle.value
      val nameOfIntStr = // TODO(lo-prio): fragile, find a better way
        IntStr.getClass.getName.dropRight(1).replaceAllLiterally("$", ".")
      val diff = (
        ("i" -> 42) ~ ("s" -> "foo") ~
          ("$type" -> nameOfIntStr) // typeName[IntStr])
      ) diff parse(actual)
      assert(diff.isEmpty, diff == noDiff)
    }

    "regular class" - {
      val (one, two) = ("one", 2)

      "non circular local" - {
        class StringInt(val s: String, val i: Int)
        val diff = (
          ("$type" -> "StringInt") ~ // FIXME(med-prio): avoid hard-coded name
            ("i" -> two) ~
            ("s" -> one) // order does not matter, it's a diff
        ) diff parse(new StringInt(one, two).pickle.value)
        assert(diff.isEmpty, diff == noDiff)
      }

      "circular ref" - {
        val circ = new IntAndSelfFieldClass
        { // FIXME(scala-pickling): it serializes *nothing* without this,
          val y = 11 // but this field is not even serialized! Double-bug?
        }
        val freeVals = fun1 { (a0: Unit) => circ.x } freeRefVals
        val expected = (
          ("$type" -> circ.getClass.getName) ~
            ("self" -> (
              ("$type" -> circ.getClass.getName) ~
                ("self" ->
                  ("$ref" -> 0) // object identity info
                ) ~
                ("x" -> 42))) ~
            ("x" -> 42))

        val actual = parse(freeVals.head.pickle.value)
        val diff = expected diff actual

        "no missing or extra" - {
          assert(diff.added == JNothing, diff.deleted == JNothing)
        }

        "no changed" - {
          val changed = diff.changed
          assert(changed == JNothing)
        }
      }
    }
  }
}
