package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class PGASpec extends FlatSpec with Matchers {
  import PGA._

  "An Instruction" should "parse" in {
    def parseInstruction(instruction: String) =
      Parsers.parse(Parsers.instruction, instruction).get

    parseInstruction("a") shouldEqual Instruction.Basic("a")

    parseInstruction("!") shouldEqual Instruction.Termination

    parseInstruction("+a") shouldEqual Instruction.PositiveTest("a")

    parseInstruction("-a") shouldEqual Instruction.NegativeTest("a")

    parseInstruction("#0") shouldEqual Instruction.Jump(0)
  }

  def parseProgram(program: String) =
    Parsers.parse(Parsers.program, program).get

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
}