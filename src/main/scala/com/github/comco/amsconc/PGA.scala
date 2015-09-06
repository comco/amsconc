package com.github.comco.amsconc

import scala.util.parsing.combinator._

/**
 * TODO:
 * - thread interpretation
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
      case Concatenation(start, after) => {
        val (s, a) = (start.toCompactString, after.toCompactString)
        (start, after) match {
          case (Concatenation(_, _), _) => s"($s);$a"
          case _ => s"$s;$a"
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

    override def toString = s"Program[${toCompactString}]"

    // Prepends a program to this program.
    def prepend(program: Program): Program = program match {
      case Primitive(_) | Repetition(_) => Concatenation(program, this)
      case Concatenation(start, after) => Concatenation(start, prepend(after))
    }

    // Computes a first canonical form of this program.
    lazy val firstCanonicalFrom: Program = this match {
      case Primitive(_) => this
      case Concatenation(start, after) => {
        val firstCanonical = start.firstCanonicalFrom
        firstCanonical match {
          case Concatenation(a, Repetition(b)) => firstCanonical
          case _ => {
            val secondCanonical = after.firstCanonicalFrom
            secondCanonical match {
              case Concatenation(a, Repetition(b)) =>
                Concatenation(a.prepend(firstCanonical), Repetition(b))
              case _ =>
                secondCanonical.prepend(firstCanonical)
            }
          }
        }
      }
      case Repetition(body) => {
        val bodyCanonical = body.firstCanonicalFrom
        bodyCanonical match {
          case Concatenation(a, Repetition(b)) => bodyCanonical
          case Concatenation(start, after) =>
            Concatenation(start, Repetition(start.prepend(after)))
          case _ => Concatenation(bodyCanonical, Repetition(bodyCanonical))
        }
      }
    }

    // Computes the minimal first canonical from of this program.
    lazy val minimalFirstCanonicalFrom: Program = {
      // Extracts the largest prefix of a that is a prefix of b*.
      // Does not let a become the empty list.
      def cycleFactor[A](a: List[A], b: List[A]): (List[A], List[A]) =
        (a, b) match {
          case ((a :: as), (b :: bs)) if (a == b && as.nonEmpty) =>
            cycleFactor(as, bs ++ List(b))
          case _ => (a, b)
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

      firstCanonicalFrom match {
        case Concatenation(a, Repetition(b)) => {
          def toList(a: Program): List[Program] = a match {
            case Concatenation(head, tail) => head :: toList(tail)
            case last @ _ => List(last)
          }
          val alr: List[Program] = toList(a).reverse
          val blr: List[Program] = cycleReduce(toList(b).reverse)
          println("blr:", blr)
          val (nalr, nblr) = cycleFactor(alr, blr)
          val ar = Concatenation.fromList(nalr.reverse)
          val br = Concatenation.fromList(nblr.reverse)
          Concatenation(ar, Repetition(br))
        }
        case finite @ _ => finite
      }
    }
    
    // Computes if this program is instruction sequence congruent to 'other'.
    def instructionSequenceCongruent(other: Program): Boolean =
      minimalFirstCanonicalFrom == other.minimalFirstCanonicalFrom
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
    import Instruction._
    import Program._

    def label: Parser[Label] = ident

    def basic: Parser[Basic] = label map Basic

    def termination: Parser[Termination.type] =
      "!" ^^ Function.const(Termination)

    def positiveTest: Parser[PositiveTest] =
      ("+" ~> label) map PositiveTest

    def negativeTest: Parser[NegativeTest] =
      ("-" ~> label) map NegativeTest

    def steps: Parser[Int] = wholeNumber map (_.toInt)

    def jump: Parser[Jump] = ("#" ~> steps) map Jump

    def instruction: Parser[Instruction] =
      basic | termination | positiveTest | negativeTest | jump

    def primitive: Parser[Primitive] = instruction map Primitive

    def atomic: Parser[Program] = primitive | "(" ~> program <~ ")"

    def repetition: Parser[Program] = (atomic ~ rep("*")) ^^ {
      case a ~ list => list.foldLeft(a) { (a, _) => Repetition(a) }
    }

    def concatenation: Parser[Program] =
      rep1sep(repetition, ";") map Concatenation.fromList

    def program: Parser[Program] = concatenation
  }
}
