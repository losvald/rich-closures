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

import org.json4s._
import JsonDSL._
import native.{Serialization => JsonSer, _}
import native.JsonMethods._
// import org.json4s.jackson.{Serialization => JacksonSer} // FIXME: find dep.

import utest._

object JsonClosureComparisonTest extends TestBase { self =>
  case class IntStr(i: Int, s: String)
  case class Nested(is: IntStr)

  class StringInt(val s: String, val i: Int)

  // TODO(hi-prio): use this from Jackson serialization (supported by json4s)
  // @JsonIdentityInfo(generator=ObjectIdGenerators.IntSequenceGenerator.class,
  //   property="@id")
  class IntAndSelfFieldClass {
    val x = 42
    val self = this
  }

  val noDiff = Diff(JNothing, JNothing, JNothing)

  val tests = TestSuite {
    import SerializationUtil._

    implicit def jValue2String(jv: JValue): String =
      compact(render(jv))

    def toStr(jv: JValue) = compact(render(jv))

    def jsonify(a: Any)(implicit fmts: Formats) =
      toStr(Extraction.decompose(a)(fmts))

    "primitives" - {
      implicit val fmts = JsonSer.formats(NoTypeHints)
      val one = 1
      val oneTwo = jsonify(List(one, "two"))
      assert(oneTwo == """[1,"two"]""")

      "singleton" - {
        val rc1RefVals = fun1 { (s: String) => List(one, s) } freeRefVals
        val actual = JsonSer.write(rc1RefVals)
        assert(actual == "[1]")
      }

      "int and string" - {
        // TODO(med-prio): use fun0 after it is properly implemented
        val two = "two"
        val rc1RefVals = fun1 { (a0: Unit) => List(one, two) } freeRefVals
        val actual = JsonSer.writePretty(rc1RefVals)
        assert(actual == """[
          |  1,
          |  "two"
          |]""".stripMargin)
        val d = parse(actual) diff parse("""[1, "two"]""")
        assert(d == noDiff)
      }
    }

    "case class" - {
      implicit val fmts = JsonSer.formats(
        new FullTypeHints(classOf[IntStr] :: classOf[Nested] :: Nil))

      val diff = Extraction.decompose(new IntStr(42, "foo")) diff (
          ("i" -> 42) ~ ("s" -> "foo") ~
            ("jsonClass" -> classOf[IntStr].getName))
      assert(diff == noDiff)
    }

    "regular class" - {
      implicit val fmts = JsonSer.formats(
        new FullTypeHints(classOf[StringInt] ::
          classOf[IntAndSelfFieldClass] :: Nil))

      val (one, two) = ("one", 2)

      "non circular" - {
        val diff = Extraction.decompose(new StringInt(one, two)) diff (
          ("jsonClass" -> classOf[StringInt].getName) ~
            ("i", 2) ~
            ("s", "one"))
        assert(diff == noDiff)
      }

      "circular ref" - {
        val circ = new IntAndSelfFieldClass
        // {// FIXME(json4s): Classes defined in method bodies are not supported
        //   val y = 11
        // }
        val freeVals = fun1 { (a0: Unit) => circ.x } freeRefVals
        val expectedJson = (
          ("jsonClass" -> classOf[IntAndSelfFieldClass].getName) ~
            ("self" -> (
              ("jsonClass" -> classOf[IntAndSelfFieldClass].getName) ~
                ("x" -> 42))) ~
            ("x" -> 42))

        // FIXME: decompose doesn't pick up anything
        // val diff = Extraction.decompose(freeVals head) diff (expectedJson)
        // assert(diff == noDiff)

        // FIXME: jsonify only picks up the class type but not object id!
        val actual = jsonify(freeVals.head)
        val expected = toStr(expectedJson)
        // assert(actual == expected)
      }
    }
  }
}
