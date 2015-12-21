import utest._

package pkg {
  object Module {
    type Str = String
    val gBar = "bar"
  }
}

package object pkgobj {
  type GlobStr = String
  val gFoo = "foo"

  object Module {
    type Str = String
    val gBaz = "baz"
  }
}

object Module {
  val gConst42 = 42

  object Inner {
    val gConst21 = gConst42 / 2
  }
}

object SerializationUtilTest extends TestBase {
  val testSuiteName = "SerializationUtilTest"
  val gConst42 = 42

  def plus42(x: Int) = x + 42

  object Applier {
    def fOfOne(f: Int => Int): Int = f(1)
  }

  val tests = TestSuite {
    import SerializationUtil._
    import TestOnly._

    "def macros" - {
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
          val noRefs = mkFreeRefs()

          "from global method" - {
            val freeRefs = fun1(plus42).freeRefs
            assert(freeRefs == noRefs)
          }

          "from higher-order method" - {
            def get43(ignored: Int) = {
              val one = 1 // try to confuse the macro
              plus42(one)
            }
            val freeRefs = fun1(get43).freeRefs
            assert(freeRefs == noRefs)
          }

          "structural refinement" - {
            "field" - {
              val freeRefs = fun1((x: { val y: Int }) => x.y).freeRefs
              assert(freeRefs == noRefs)
            }
            "instance" - {
              val freeRefs = fun1((z: { val w: Int }) => z).freeRefs
              assert(freeRefs == noRefs)
            }
            "in call" - {
              val freeRefs = fun1((x: Int) => {
                def foo(bar: { val baz: String }) = bar.baz
                new { val baz = "baz" }
              }).freeRefs
              assert(freeRefs == noRefs)
            }
          }

          "type" - {
            "ident" - {
              type LocalInt = Int
              val freeRefs = fun1((z: LocalInt) =>
                1.asInstanceOf[LocalInt]).freeRefs
              assert(freeRefs == noRefs)
            }

            "select" - {
              trait Trait {
                type LocalAny = Any
              }
              class Clazz[T <: Trait#LocalAny] {
                type LocalT = T
                type LocalString = String
              }

              var freeRefs = fun1((s: Clazz[Int]#LocalString) => {
                type ClazzStr = Clazz[String]
                type MyStr = ClazzStr#LocalT
                s.asInstanceOf[MyStr] + s.asInstanceOf[Trait#LocalAny]
              }).freeRefs
              assert(freeRefs == noRefs)

              // Verify no false positive due to selection of:
              // - Literal(Constant("foo"))
              // - path-dependant type c.type#LocalT
              freeRefs = fun1((c: Clazz[String]) =>
                "foo".asInstanceOf[c.type#LocalT]).freeRefs
              assert(freeRefs == noRefs)
            }
          }

          "package" - {
            val freeRefs = fun1((s: String) => {
              type Str = pkg.Module.type#Str
              s.asInstanceOf[pkgobj.GlobStr]
            }).freeRefs
            assert(freeRefs == noRefs)
          }

          "import" - {
            "wildcard" - {
              val freeRefs = fun1((s: String) => {
                import pkg._
                import pkgobj._
                s
              }).freeRefs
              assert(freeRefs == noRefs)
            }

            "import specific" - {
              val freeRefs = fun1((s: String) => {
                import pkgobj.gFoo
                s
              }).freeRefs
              assert(freeRefs == noRefs)
            }
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
            // "indirectly" - { // TODO(med-prio) is this supposed to work?
            //   val five = 5
            //   def f(x: Int) = five
            //   val freeRefs = fun1(f).freeRefs
            //   assert(freeRefs == mkFreeRefs(five))
            // }
          }

          // "from local val" - { // TODO(lo-prio) rewrite Ref(TermName("f"))?
          //   val six = 6
          //   val f: Int => Int = (x: Int) => six
          //   val freeRefs = fun1(f).freeRefs
          //   assert(freeRefs == mkFreeRefs(six))
          // }

          "bound in patmat" - {
            "outer" - {
              val freeRefs = Some("foo") match {
                case opt @ Some(_) => fun1((s: String) => opt).freeRefs
              }
              assert(freeRefs == mkFreeRefsUnsafe(s"$testSuiteName.opt"))
            }

            "inner" - {
              val freeRefs = ((1, 2), (10, 20)) match {
                case ((_, two), snd @ (_, _)) => fun1 {
                  (neg: Boolean) => (if (neg) -1 else 1) * (two + snd._2)
                }.freeRefs.asInstanceOf[List[String]].sorted
              }
              assert(freeRefs == mkFreeRefsUnsafe(
                s"${testSuiteName}.snd",
                s"${testSuiteName}.two"))
            }
          }

          "as param to" - {
            import Applier._

            "val" - {
              val double = (x: Int) => x * 2

              {
                val actFreeRefs = fun1((x: Int) => x + fOfOne(double)).freeRefs
                val expFreeRefs = mkFreeRefs(double)
                assert(actFreeRefs == expFreeRefs)
              }
              {
                val two = 2
                val actFreeRefs = fun1(
                  (x: Int) => fOfOne { _ + two }).freeRefs // XXX
                val expFreeRefs = mkFreeRefs(double)
              }
            }

            // "def" - { // TODO(med-prio) fails with BF finder
            //   def triple(x: Int) = x * 3
            //   val actFreeRefs = fun1((x: Int) => x + fOfOne(triple)).freeRefs
            //   val expFreeRefs = mkFreeRefs(triple _)
            //   assert(actFreeRefs == expFreeRefs)
            // }
          }

          "param of enclosing" - {
            "def" - {
              def fAnon(arg: Int) = {
                val y = 3
                fun1((x: Int) => x + y + arg)
              }

              "anon. fun" - {
                val freeRefs = fAnon(2).freeRefs
                assert(freeRefs == mkFreeRefsUnsafe(
                  s"$testSuiteName.arg",
                  s"$testSuiteName.y"))
              }

              def fAnonShadowedByVal(arg: Int) = {
                val arg = 3
                fun1((x: Int) => x + arg)
              }

              "shadowed by local val" - {
                val freeRefs = fAnonShadowedByVal(2).freeRefs
                assert(freeRefs == mkFreeRefsUnsafe(s"$testSuiteName.arg"))
              }

              "shadowed by anon. fun param" - {
                def fAnonShadowedByParam(arg: Int) = fun1((arg: Int) => arg)
                val freeRefs = fAnonShadowedByParam(0).freeRefs
                assert(freeRefs == mkFreeRefsUnsafe())
              }
            }

            "case def" - {
              val freeRefs = Some("foo") match {
                case Some(foo) => fun1((s: String) => foo).freeRefs
              }
              assert(freeRefs == mkFreeRefsUnsafe(s"$testSuiteName.foo"))
            }
          }
        }

        // "other module" - { // TODO(hi-prio) detect stable symbols in Select
        //   "from anon. func" - {
        //     //Select(Select(Ref(Module), Module.Inner), TermName("gConst21"))
        //     val actFreeRefs = fun1(
        //       (x: Int) => x + Module.Inner.gConst21
        //     ).freeRefs
        //     // assert(freeRefs == List("Module"))
        //     val expFreeRefs = mkFreeRefs(
        //       Module.Inner,
        //       Module) // FIXME(hi-prio) false positive & missing gConst21
        //     assert(actFreeRefs == expFreeRefs)
        //   }
        // }

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
