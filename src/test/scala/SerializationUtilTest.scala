import utest._

object SerializationUtilTest extends TestBase {
  val gConst42 = 42

  val tests = TestSuite {
    "def macros" - {
      import SerializationUtil._

      "fun0" - {
        "no eval" - {
          var loc = 3
          fun0 {
            loc = 42
          }
          assert(loc == 3)
        }
      }

      "fun1" - {
        "no early eval" - {
          var loc = 3
          val f = fun1 { (x: Int) =>
            loc = x + x;
          }
          assert(loc == 3)

          f(21)
          assert(loc == 42)
        }

        "member & local" - {
          "from method" - {
            val y = 1
            def add42AndY(x: Int) = x + y + gConst42
            fun1(add42AndY)
            assert(add42AndY(23) == 66)
            // TODO(hi-prio) more assertions
          }
          "from anon. closure" - {
            val y = 1
            val freeIdents = fun1 { (x: Int) => x + y + gConst42 }.freeIdents
            assert(freeIdents == List("SerializationUtilTest.y"))
          }
        }

        "higher-order anon. function" - {
          var freeIdents = fun1((f: Int => String) => 42).freeIdents
          assert(freeIdents == List())
          // TODO(med-prio) figure out why the following doesn't compile
          // assert(fun1((f: Int => String) => 42).freeIdents.isEmpty)
        }
      }
    }
  }
}
