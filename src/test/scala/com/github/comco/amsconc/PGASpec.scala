package com.github.comco.amsconc

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author comco
 */
class PGASpec extends FlatSpec with Matchers {
  import PGA._

  "An Instruction" should "parse" in {
    def ParseInstruction(instruction: String) =
      Parsers.parse(Parsers.instruction, instruction).get

    ParseInstruction("a") shouldEqual Instruction.Basic("a")

    ParseInstruction("!") shouldEqual Instruction.Termination

    ParseInstruction("+a") shouldEqual Instruction.PositiveTest("a")

    ParseInstruction("-a") shouldEqual Instruction.NegativeTest("a")

    ParseInstruction("#0") shouldEqual Instruction.Jump(0)
  }

  "A Program" should "parse" in {

    def ParseProgram(program: String) =
      Parsers.parse(Parsers.program, program).get

    ParseProgram("a") shouldEqual Program.Primitive(Instruction.Basic("a"))

    import Instruction._
    import Program._

    ParseProgram("a;+b;-c") shouldEqual
      Concatenation(Primitive(Basic("a")), Concatenation(
        Primitive(PositiveTest("b")), Primitive(NegativeTest("c"))))

    ParseProgram("(#2;a);b") shouldEqual
      Concatenation(Concatenation(Primitive(Jump(2)), Primitive(Basic("a"))),
        Primitive(Basic("b")))

    ParseProgram("#0;#1*") shouldEqual
      Concatenation(Primitive(Jump(0)), Repetition(Primitive(Jump(1))))

    ParseProgram("a;(+b;!;-c)*;d") shouldEqual
      Concatenation(Primitive(Basic("a")),
        Concatenation(
          Repetition(Concatenation(
            Primitive(PositiveTest("b")),
            Concatenation(
              Primitive(Termination),
              Primitive(NegativeTest("c"))))),
          Primitive(Basic("d"))))

    ParseProgram("((a);(b)**)*") shouldEqual
      Repetition(Concatenation(
        Primitive(Basic("a")),
        Repetition(Repetition(Primitive(Basic("b"))))))
  }
}

























