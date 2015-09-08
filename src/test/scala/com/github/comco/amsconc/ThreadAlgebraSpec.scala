package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class ThreadAlgebraSpec extends FlatSpec with Matchers {
  import ThreadAlgebra._

  "A ThreadAlgebra.Behavior" should "parse" in {
    import Behavior._

    parseBehavior("S") shouldEqual Terminal()

    parseBehavior("D") shouldEqual Deadlock()

    parseBehavior("a.b.S") shouldEqual
      ActionPrefix("a", ActionPrefix("b", Terminal()))

    parseBehavior("a . (b.D)") shouldEqual
      ActionPrefix("a", ActionPrefix("b", Deadlock()))

    parseBehavior("S < a > (D)") shouldEqual
      PostconditionalComposition("a", Terminal(), Deadlock())

    // Action prefix binds stronger than post-conditional composition.
    parseBehavior("a.a.D < b > c.S") shouldEqual
      PostconditionalComposition("b",
        ActionPrefix("a", ActionPrefix("a", Deadlock())),
        ActionPrefix("c", Terminal()))
  }

  it should "support variables in terms" in {
    import Behavior._

    // Strings inside [] are treated as variables.
    parseBehavior("a.[x] < b > ([x] < c > d.[y])") shouldEqual
      PostconditionalComposition("b",
        ActionPrefix("a", Variable("x")),
        PostconditionalComposition("c",
          Variable("x"),
          ActionPrefix("d", Variable("y"))))
    
    parseBehavior("a.[a;b;(!;$)*]") shouldEqual
      ActionPrefix("a", Variable("a;b;(!;$)*"))
  }
}
