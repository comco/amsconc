package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class ThreadAlgebraSpec extends FlatSpec with Matchers {
  import ThreadAlgebra._

  "A ThreadAlgebra Term" should "parse" in {
    import Term._

    parseTerm("S") shouldEqual Termination

    parseTerm("D") shouldEqual Deadlock

    parseTerm("a.b.S") shouldEqual
      ActionPrefix("a", ActionPrefix("b", Termination))

    parseTerm("a . (b.D)") shouldEqual
      ActionPrefix("a", ActionPrefix("b", Deadlock))

    parseTerm("S < a > (D)") shouldEqual
      PostconditionalComposition("a", Termination, Deadlock)

    // Action prefix binds stronger than post-conditional composition.
    parseTerm("a.a.D < b > c.S") shouldEqual
      PostconditionalComposition("b",
        ActionPrefix("a", ActionPrefix("a", Deadlock)),
        ActionPrefix("c", Termination))
  }

  it should "support variables in terms" in {
    import Term._

    parseTerm("a.'x < b > ('x < c > d.'y)") shouldEqual
      PostconditionalComposition("b",
        ActionPrefix("a", Variable("x")),
        PostconditionalComposition("c",
          Variable("x"),
          ActionPrefix("d", Variable("y"))))
  }
}