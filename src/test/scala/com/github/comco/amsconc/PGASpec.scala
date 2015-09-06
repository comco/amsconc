package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class PGASpec extends FlatSpec with Matchers {
  import PGA._

  def parseInstruction(instruction: String) =
    Parsers.parse(Parsers.instruction, instruction).get

  def parseProgram(program: String) =
    Parsers.parse(Parsers.program, program).get

  "An Instruction" should "parse" in {
    import Instruction._ 

    parseInstruction("a") shouldEqual Basic("a")

    parseInstruction("!") shouldEqual Termination

    parseInstruction("+a") shouldEqual PositiveTest("a")

    parseInstruction("-a") shouldEqual NegativeTest("a")

    parseInstruction("#0") shouldEqual Jump(0)
  }

  "A Program" should "parse" in {
    import Instruction._
    import Program._
    
    parseProgram("a") shouldEqual Primitive(Basic("a"))

    parseProgram("a;+b;-c") shouldEqual
      Concatenation(Primitive(Basic("a")), Concatenation(
        Primitive(PositiveTest("b")), Primitive(NegativeTest("c"))))

    parseProgram("(#2;a);b") shouldEqual
      Concatenation(Concatenation(Primitive(Jump(2)), Primitive(Basic("a"))),
        Primitive(Basic("b")))

    parseProgram("#0;#1*") shouldEqual
      Concatenation(Primitive(Jump(0)), Repetition(Primitive(Jump(1))))

    parseProgram("a;(+b;!;-c)*;d") shouldEqual
      Concatenation(Primitive(Basic("a")),
        Concatenation(
          Repetition(Concatenation(
            Primitive(PositiveTest("b")),
            Concatenation(
              Primitive(Termination),
              Primitive(NegativeTest("c"))))),
          Primitive(Basic("d"))))

    parseProgram("((a);(b)**)*") shouldEqual
      Repetition(Concatenation(
        Primitive(Basic("a")),
        Repetition(Repetition(Primitive(Basic("b"))))))
  }
  
  it should "suppport pretty printing" in {
    def expectCompacted(original: String, compacted: String) {
      val program = parseProgram(original)
      program.toCompactString shouldEqual compacted
      parseProgram(compacted) shouldEqual program
    }
    
    expectCompacted("a", "a")
    
    expectCompacted("((a))", "a")
    
    expectCompacted("+a;!", "+a;!")
    
    expectCompacted("a;(b;c)", "a;b;c")
    
    expectCompacted("(a;b);c", "(a;b);c")
    
    expectCompacted("((a);(b));(c);((d);(e))", "(a;b);c;d;e")
    
    expectCompacted("!;#2;#1*", "!;#2;#1*")
    
    expectCompacted("!**;!*;(a;-b)**", ("!**;!*;(a;-b)**"))
  }
  
  it should "support prepend" in {
    def expectPrepend(a: String, b: String, c: String) {
      parseProgram(a).prepend(parseProgram(b)) shouldEqual parseProgram(c)
    }
    
    expectPrepend("b", "a", "a;b")
    
    expectPrepend("c;d", "a;b", "a;b;c;d")
  }
  
  it should "support firstCanonicalForm" in {
    def expectFirstCanonicalForm(original: String, canonical: String) {
      val program = parseProgram(original)
      program.firstCanonicalFrom shouldEqual parseProgram(canonical)
    }
    
    expectFirstCanonicalForm("a", "a")
    
    expectFirstCanonicalForm("a;!", "a;!")
    
    expectFirstCanonicalForm("a;b;c", "a;b;c")
    
    expectFirstCanonicalForm("(a;b);c", "a;b;c")
    
    expectFirstCanonicalForm("a*", "a;a*")
    
    expectFirstCanonicalForm("a;b*", "(a;b);b*")
    
    expectFirstCanonicalForm("a**", "a;a*")
    
    expectFirstCanonicalForm("(a;b;c)*", "a;(b;c;a)*")
    
    expectFirstCanonicalForm("(a;b*)*", "(a;b);b*")
    
    expectFirstCanonicalForm("a;(b;c)*;(d;e*)*", "(a;b);(c;b)*")
  }
  
  it should "support minimal firstCanonicalForm" in {
    def expectMinimalFirstCanonicalForm(original: String, canonical: String) {
      val program = parseProgram(original)
      program.minimalFirstCanonicalFrom shouldEqual parseProgram(canonical)
    }
    
    expectMinimalFirstCanonicalForm("a", "a")
    
    expectMinimalFirstCanonicalForm("a;+b;-c", "a;+b;-c")
    
    expectMinimalFirstCanonicalForm("a*", "a;a*")
    
    expectMinimalFirstCanonicalForm("a;b;c;(a;b;c)*", "a;(b;c;a)*")
    
    expectMinimalFirstCanonicalForm("(a;b);b*", "a;b*")
    
    expectMinimalFirstCanonicalForm("(#0;#1)*", "#0;(#1;#0)*")
    
    expectMinimalFirstCanonicalForm("-c;+a;(+b;#2;c;+a)*;c;a*", "-c;(+a;+b;#2;c)*")
  }
}