package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class PASpec extends FlatSpec with Matchers {
  import PA._
  import Program._
  import Instruction._
  import Program.DSL._

  "A PA.Instrucion" should
    "disallow forward jumps by with negative number of steps" in {
      an[IllegalArgumentException] should be thrownBy parseInstruction("#-1")
    }

  "A PA.Program" should "parse by #parseProgram" in {
    parseProgram("$") shouldEqual seq()
    parseProgram("a;$") shouldEqual seq("a")
    parseProgram("+a;-b;c;!;#0;$") shouldEqual seq("+a", "-b", "c", "!", "#0")
    parseProgram("+a;(b;$)*") shouldEqual seq("+a", rep("b"))
    parseProgram("$*") shouldEqual rep()
    parseProgram("(a;$*)**") shouldEqual rep(rep("a", rep()))
    parseProgram("(a;(!;$)*)*") shouldEqual rep("a", rep("!"))
  }

  it should "pretty print by #toCompactString" in {
    def expectPrettyPrinted(programString: String) {
      val program = parseProgram(programString)
      program.toCompactString shouldEqual programString
    }

    expectPrettyPrinted("$")
    expectPrettyPrinted("+a;-b;c;!;#0;$")
    expectPrettyPrinted("$*")
    expectPrettyPrinted("(a;$)***")
    expectPrettyPrinted("a;(+b;c;$)*")
  }

  it should "compute the instruction sequence by #instructions" in {
    val bound = 10

    def expectInstructionSequence(programString: String,
      instructions: Seq[Instruction]) {
      val program = parseProgram(programString)
      program.instructions.take(bound).toList shouldEqual instructions
    }

    def seq(instructions: Instruction*): Seq[Instruction] =
      Seq(instructions: _*)

    def cycleOff(off: Int, instructions: Instruction*): Seq[Instruction] =
      Stream.continually(instructions.toStream).flatten.take(bound - off)

    def cycle(instructions: Instruction*) = cycleOff(0, instructions: _*)

    expectInstructionSequence("$", seq())
    expectInstructionSequence("a;$", seq("a"))
    expectInstructionSequence("+a;-b;c;#12;!;$",
      seq("+a", "-b", "c", "#12", "!"))
    expectInstructionSequence("$*", seq())
    expectInstructionSequence("$**", seq())
    expectInstructionSequence("(a;$)*", cycle("a"))
    expectInstructionSequence("(!;!;$)*", cycle("!"))
    expectInstructionSequence("(a;b;$)*", cycle("a", "b"))
    expectInstructionSequence("#0;#1;(!;a;(b;$)*)*",
      seq("#0", "#1", "!", "a") ++ cycleOff(4, "b"))
  }

  it should "support extracting first instruction" in {
    def expectFirst(a: String, first: String, next: String) {
      parseProgram(a).extractFirst shouldEqual
        Some((parseInstruction(first), parseProgram(next)))
    }

    def expectNoFirst(a: String) {
      parseProgram(a).extractFirst shouldEqual None
    }

    expectNoFirst("$")
    expectNoFirst("$*")
    expectNoFirst("$***")

    expectFirst("-a;$", "-a", "$")
    expectFirst("!;!;$", "!", "!;$")
    expectFirst("(a;$)*", "a", "(a;$)*")
    expectFirst("a;(b;$)*", "a", "(b;$)*")
    expectFirst("(#0;-a;#5;b;$)*", "#0", "(-a;#5;b;#0;$)*")
    expectFirst("(a;$)**", "a", "(a;$)*")
  }

  it should "support cyclic reduction" in {
    def expectCyclicReduction(a: String, b: String) {
      parseProgram(a).cyclicReduction shouldEqual parseProgram(b)
    }
    expectCyclicReduction("a;b;$", "a;b;$")
    expectCyclicReduction("(a;b;b;a;$)*", "(a;b;b;a;$)*")
    expectCyclicReduction("(a;b;a;b;$)*", "(a;b;$)*")
    expectCyclicReduction("(!;!;!;$)*", "(!;$)*")
    expectCyclicReduction("(a;c;b;a;c;b;a;c;b;$)*", "(a;c;b;$)*")
  }

  it should "compute instruction sequence congruence" in {
    def expectInstructionSequenceCongruent(a: String, b: String, is: Boolean) {
      val programA = parseProgram(a)
      val programB = parseProgram(b)
      programA.isInstructionSequenceCongruentTo(programB) shouldEqual is
    }

    expectInstructionSequenceCongruent("$", "$", true)
    expectInstructionSequenceCongruent("$", "$*", true)
    expectInstructionSequenceCongruent("$", "a;$", false)
    expectInstructionSequenceCongruent("a;$", "a;$", true)
    expectInstructionSequenceCongruent("(a;$)*", "a;(a;$)*", true)
    expectInstructionSequenceCongruent("a;b;(c;a;b;$)*", "(a;b;c;$)*", true)
    expectInstructionSequenceCongruent("(a;b;a;b;$)*", "(a;b;$)*", true)
    expectInstructionSequenceCongruent("a;b;(a;(b;a;$)*)*", "(a;b;$)*", true)
  }
  
  "A PA.BehaviorExtractor" should "extract program behavior by #extract" in {
    val extractor = new BehaviorExtractor
    
    def expectExtracted(programString: String, behaviorString: String) {
      val program = parseProgram(programString)
      val behavior = ThreadAlgebra.parseBehavior(behaviorString)
      extractor.extract(program) shouldEqual behavior
    }
    
    expectExtracted("$", "D")
    expectExtracted("a;$", "a.[$]")
    expectExtracted("(a;$)*", "a.[(a;$)*]")
    expectExtracted("(b;c;b;c;$)*", "b.[(c;b;c;b;$)*]")
    expectExtracted("(c;b;c;b;$)*", "c.[(b;c;b;c;$)*]")
    expectExtracted("#0;$", "D")
    expectExtracted("#1;$", "D")
    expectExtracted("#2;$", "D")
    expectExtracted("#3;$", "D")
    expectExtracted("#0;a;$", "D")
    expectExtracted("#1;a;$", "a.[$]")
    expectExtracted("#2;a;$", "D")
    expectExtracted("#10;(a;$)*", "a.[(a;$)*]")
    expectExtracted("(#0;#1;$)*", "D")
    // Larger example with subexpressions
    expectExtracted("-c;+a;(+b;#2;c;+a;$)*", "[#2;+a;(+b;#2;c;+a;$)*] < c > [+a;(+b;#2;c;+a;$)*]")
    expectExtracted("#2;+a;(+b;#2;c;+a;$)*", "[(#2;c;+a;+b;$)*] < b > [#2;(#2;c;+a;+b;$)*]")
    expectExtracted("(#2;c;+a;+b;$)*", "[(+b;#2;c;+a;$)*] < a > [#2;(+b;#2;c;+a;$)*]")
    expectExtracted("(+b;#2;c;+a;$)*", "[(#2;c;+a;+b;$)*] < b > [#2;(#2;c;+a;+b;$)*]")
    
  }
}
