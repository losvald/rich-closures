import utest._

object Module {
  val gConst42 = 42

  object Inner {
    val gConst21 = gConst42 / 2
  }
}

object SerializationUtilTest extends TestBase {
  val gConst42 = 42
  def plus42(x: Int) = x + 42

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

        "none" - {
          "from global method" - {
            val freeRefs = fun1(plus42).freeRefs
            assert(freeRefs == mkFreeRefs())
          }

          "from higher-order method" - {
            def get43(ignored: Int) = {
              val one = 1 // try to confuse the macro
              plus42(one)
            }
            val freeRefs = fun1(get43).freeRefs
            assert(freeRefs == mkFreeRefs())
          }
        }

        "local only" - {
          val parentBar = "bar"
          "parent scope from anon. func" - {
            val actFreeRefs = fun1((s: String) => parentBar).freeRefs
            val expFreeRefs = mkFreeRefs(parentBar)
            assert(actFreeRefs == expFreeRefs)
          }

          "from anon. func" - {
            val five = 5
            val freeRefs = fun1((x: Int) => five).freeRefs
            assert(freeRefs == mkFreeRefs(five))
          }

          "from local def" - {
            val five = 5
            def f(x: Int) = five
            val freeRefs = fun1(f).freeRefs
            assert(freeRefs == mkFreeRefs(five))
          }

          // "from local val" - { // TODO(lo-prio) rewrite Ref(TermName("f"))?
          //   val six = 6
          //   val f: Int => Int = (x: Int) => six
          //   val freeRefs = fun1(f).freeRefs
          //   assert(freeRefs == mkFreeRefs(six))
          // }
        }

        "other module" - {
          "from anon. func" - {
            // Select(Select(Ref(Module), Module.Inner), TermName("gConst21"))
            val actFreeRefs = fun1(
              (x: Int) => x + Module.Inner.gConst21
            ).freeRefs
            // assert(freeRefs == List("Module"))
            val expFreeRefs = mkFreeRefs(
              Module.Inner,
              Module) // FIXME(hi-prio) false positive & missing gConst21
            assert(actFreeRefs == expFreeRefs)
          }
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
            val freeRefs = fun1 { (x: Int) => x + y + gConst42 }.freeRefs
            assert(freeRefs == List("SerializationUtilTest.y"))
          }
        }

        "higher-order anon. function" - {
          var freeRefs = fun1((f: Int => String) => 42).freeRefs
          assert(freeRefs == List())
          // TODO(med-prio) figure out why the following doesn't compile
          // assert(fun1((f: Int => String) => 42).freeRefs.isEmpty)
        }
      }
    }
  }
}
