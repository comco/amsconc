package com.github.comco.amsconc

import com.github.comco.amsconc.ThreadAlgebra.Behavior.Deadlock
import com.github.comco.amsconc.ThreadAlgebra.Behavior.Terminal
import com.github.comco.amsconc.ThreadAlgebra.Behavior.ActionPrefix

/**
 * Definition of a simplified version of PGA.
 * @author comco
 */
object PA {
  type Label = String

  sealed trait Instruction {
    import Instruction._

    lazy val toCompactString: String = this match {
      case Termination() => "!"
      case Basic(label) => label
      case PositiveTest(label) => s"+$label"
      case NegativeTest(label) => s"-$label"
      case ForwardJump(by) => s"#$by"
    }
  }

  object Instruction {
    case class Termination() extends Instruction

    case class Basic(label: Label) extends Instruction

    case class PositiveTest(label: Label) extends Instruction

    case class NegativeTest(label: Label) extends Instruction

    case class ForwardJump(by: Int) extends Instruction {
      require(by >= 0,
        s"Need a non-negative number of steps for forward jump, but was given: $by.")
    }
  }

  sealed trait Program {
    import Program._

    // Appends 'suffix' to this program.
    def append(suffix: Program): Program = this match {
      case Empty() => suffix
      case Sequence(first, next) => Sequence(first, next append suffix)
      // Appending after repetition is not reachable.
      case Repetition(body) => this
    }

    // Returns a stream of the instructions of this program.
    lazy val instructions: Stream[Instruction] = this match {
      case Empty() => Stream.empty
      case Sequence(first, next) => first #:: next.instructions
      case Repetition(body) => body.instructions match {
        case Stream() => Stream.empty
        case bodyInstructions @ _ => bodyInstructions append instructions
      }
    }

    // Extracts the first instruction and the rest of this program, if it exists.
    lazy val extractFirst: Option[(Instruction, Program)] = this match {
      case Empty() => None
      case Sequence(first, next) => Some((first, next))
      case Repetition(body) => body match {
        case Empty() => None
        case Sequence(first, next) =>
          Some((first, Repetition(next append Sequence(first, Empty()))))
        // Iterated repetition is flattened.
        case body @ Repetition(_) => body.extractFirst
      }
    }

    // Finds the smallest possible cycle generator of a list.
    def cycleReduce[A](a: List[A]): List[A] = {
      for (init <- a.inits.toList.reverse) {
        if (init.nonEmpty &&
          a.length % init.length == 0 &&
          a == List.fill(a.length / init.length)(init).flatten.toList) {
          return init
        }
      }
      return a
    }

    // Cyclically reduces programs: rep(a,b,a,b) -> rep(a,b).
    lazy val cyclicReduction: Program = this match {
      case Repetition(body) => {
        def linear(p: Program): Boolean = p match {
          case Empty() => true
          case Sequence(_, next) => linear(next)
          case Repetition(_) => false
        }
        if (linear(body)) {
          val instructions = body.instructions.toList
          val newInstructions = cycleReduce(instructions)
          Repetition(newInstructions.foldRight[Program](Empty())(Sequence))
        } else this
      }
      case _ => this
    }

    // Computes instruction sequence congruence.
    def isInstructionSequenceCongruentTo(that: Program): Boolean = {
      if (this.cyclicReduction == that.cyclicReduction) true
      else (this.extractFirst, that.extractFirst) match {
        case (None, None) => true
        case (Some((thisFirst, thisNext)), Some((thatFirst, thatNext))) =>
          thisFirst == thatFirst &&
            thisNext.isInstructionSequenceCongruentTo(thatNext)
        case _ => false
      }
    }

    lazy val toAtomicCompactString: String = this match {
      case Empty() | Repetition(_) => toCompactString
      case _ => s"($toCompactString)"
    }

    lazy val toCompactString: String = this match {
      case Empty() => "$"
      case Sequence(first, next) =>
        s"${first.toCompactString};${next.toCompactString}"
      case Repetition(body) => s"${body.toAtomicCompactString}*"
    }
  }

  object Program {
    case class Empty() extends Program

    case class Sequence(first: Instruction, next: Program) extends Program

    case class Repetition(body: Program) extends Program

    object DSL {
      def seq(instructions: Instruction*): Program = instructions.toList match {
        case List() => Empty()
        case first :: next => Sequence(first, seq(next: _*))
      }

      def seq(first: Instruction, next: Program): Program =
        Sequence(first, next)

      def rep(body: Program) = Repetition(body)

      def rep(instructions: Instruction*) =
        Repetition(seq(instructions: _*))

      def rep(first: Instruction, next: Program) =
        Repetition(Sequence(first, next))

      implicit def ins(instruction: String): Instruction =
        parseInstruction(instruction)
    }
  }
  
  class BehaviorExtractor {
    import ThreadAlgebra._
    import ThreadAlgebra.Behavior._
    import Instruction._
    import Program._
    
    def variable(program: Program) = Variable(program.toCompactString)
    
    def skipFirst(program: Program): Program = Sequence(ForwardJump(2), program)
    
    def extract(program: Program): Behavior = {
      if (bounded.contains(program)) {
        bounded(program)
      } else if (visited.contains(program)) {
        variable(program)
      } else {
        visited += program
        val behavior = program.extractFirst match {
          case Some((first, next)) => first match {
            case Termination() => Terminal()
            case Basic(label) => {
              extract(next)
              ActionPrefix(label, variable(next))
            }
            case PositiveTest(label) => {
              extract(next)
              extract(skipFirst(next))
              PostconditionalComposition(label, variable(next), variable(skipFirst(next)))
            }
            case NegativeTest(label) => {
              extract(skipFirst(next))
              extract(next)
              PostconditionalComposition(label, variable(skipFirst(next)), variable(next))
            }
            case ForwardJump(0) => Deadlock()
            case ForwardJump(1) => extract(next)
            case ForwardJump(n) => next.extractFirst match {
              case Some((_, next2)) => extract(Sequence(ForwardJump(n - 1), next2))
              case None => Deadlock()
            }
          }
          case None => Deadlock()
        }
        bounded += ((program, behavior))
        behavior
      }
    }
    
    var visited: Set[Program] = Set.empty
    
    var bounded: Map[Program, Behavior] = Map.empty
  }

  import scala.util.parsing.combinator._
  object Parsers extends JavaTokenParsers {
    import Instruction._
    import Program._

    def label: Parser[Label] = ident

    def termination: Parser[Termination] = "!" ^^ { _ => Termination() }

    def basic: Parser[Basic] = label map Basic

    def positiveTest: Parser[PositiveTest] = ("+" ~> label) map PositiveTest

    def negativeTest: Parser[NegativeTest] = ("-" ~> label) map NegativeTest

    def forwardJumpStep: Parser[Int] = wholeNumber map (_.toInt)

    def forwardJump: Parser[ForwardJump] =
      ("#" ~> forwardJumpStep) map ForwardJump

    def instruction: Parser[Instruction] =
      termination | basic | positiveTest | negativeTest | forwardJump

    def sequence: Parser[Sequence] = (instruction ~ ";" ~ program) ^^ {
      case (i ~ _ ~ p) => Sequence(i, p)
    }

    def empty: Parser[Empty] = "$" ^^ { _ => Empty() }

    def atomic: Parser[Program] = empty | "(" ~> program <~ ")"

    def repetition: Parser[Program] = (atomic ~ rep("*")) ^^ {
      case a ~ list => list.foldLeft(a) { (b, _) => Repetition(b) }
    }

    def program: Parser[Program] = repetition | sequence
  }

  def parseInstruction(instruction: String): Instruction =
    Parsers.parse(Parsers.instruction, instruction).get

  def parseProgram(program: String): Program =
    Parsers.parse(Parsers.program, program).get
}
