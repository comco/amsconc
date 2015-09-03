package com.github.comco.amsconc

import scala.util.parsing.combinator._

/**
 * TODO:
 * - equivalence
 * - normal forms
 * - thread interpretation
 * - dsl
 * @author comco
 */
object PGA {
  type Label = String

  sealed trait Instruction {
    import Instruction._
    
    // Returns a string representation of this instruction.
    lazy val toCompactString: String = this match {
      case Basic(label) => label
      case Termination => "!"
      case PositiveTest(label) => "+" + label
      case NegativeTest(label) => "-" + label
      case Jump(steps) => "#" + steps
    }
  }
  object Instruction {
    case class Basic(label: Label) extends Instruction
    case object Termination extends Instruction
    case class PositiveTest(label: Label) extends Instruction
    case class NegativeTest(label: Label) extends Instruction
    case class Jump(steps: Int) extends Instruction
  }

  sealed trait Program {
    import Program._
    
    // Returns a string representation of this program without too many
    // parenthesis.
    lazy val toCompactString: String = this match {
      case Primitive(instruction) => instruction.toCompactString
      case Concatenation(first, second) => {
        val (f, s) = (first.toCompactString, second.toCompactString)
        (first, second) match {
          case (Concatenation(_, _), _) => s"($f);$s"
          case _ => s"$f;$s"
        }
      }
      case Repetition(body) => {
        val b = body.toCompactString
        body match {
          case Primitive(_) | Repetition(_) => s"$b*"
          case _ => s"($b)*"
        }
      }
    }
  }
  object Program {
    case class Primitive(instruction: Instruction) extends Program

    case class Concatenation(first: Program, second: Program) extends Program
    object Concatenation {
      def fromList(programs: List[Program]): Program = {
        require(programs.nonEmpty,
          "Need a nonempty list of programs to concatenate.")
        programs match {
          case List(only) => only
          case head :: tail => Concatenation(head, fromList(tail))
        }
      }
    }
    case class Repetition(body: Program) extends Program
  }

  object Parsers extends JavaTokenParsers {
    def label: Parser[Label] = ident

    def basic: Parser[Instruction.Basic] = label map Instruction.Basic

    def termination: Parser[Instruction.Termination.type] =
      "!" ^^ Function.const(Instruction.Termination)

    def positiveTest: Parser[Instruction.PositiveTest] =
      ("+" ~> label) map Instruction.PositiveTest

    def negativeTest: Parser[Instruction.NegativeTest] =
      ("-" ~> label) map Instruction.NegativeTest

    def steps: Parser[Int] = wholeNumber map (_.toInt)

    def jump: Parser[Instruction.Jump] = ("#" ~> steps) map Instruction.Jump

    def instruction: Parser[Instruction] =
      basic | termination | positiveTest | negativeTest | jump

    def primitive: Parser[Program.Primitive] = instruction map Program.Primitive

    def atomic: Parser[Program] = primitive | "(" ~> program <~ ")"

    def repetition: Parser[Program] = (atomic ~ rep("*")) ^^ {
      case a ~ list => list.foldLeft(a) { (a, _) => Program.Repetition(a) }
    }

    def concatenation: Parser[Program] =
      rep1sep(repetition, ";") map Program.Concatenation.fromList

    def program: Parser[Program] = concatenation
  }
}