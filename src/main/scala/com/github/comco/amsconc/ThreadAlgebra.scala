package com.github.comco.amsconc

/**
 * @author comco
 */
object ThreadAlgebra {
  type Label = String

  sealed trait Behavior {
    import Behavior._
    
    lazy val toAtomicCompactString: String = this match {
      case Terminal() | Deadlock() | Variable(_) | ActionPrefix(_, _) =>
        toCompactString
      case _ => "(" + toCompactString + ")"
    }
    
    lazy val toCompactString: String = this match {
      case Terminal() => "S"
      case Deadlock() => "D"
      case Variable(name) => s"[$name]"
      case PostconditionalComposition(action, ifTrue, ifFalse) =>
        val at = ifTrue.toAtomicCompactString
        val af = ifFalse.toAtomicCompactString
        s"$at < $action > $af"
      case ActionPrefix(action, following) =>
        val af = following.toAtomicCompactString
        s"$action . $af"
    }
    
    override def toString = toCompactString
  }

  object Behavior {
    case class Terminal() extends Behavior

    case class Deadlock() extends Behavior

    case class PostconditionalComposition(
      action: Label, ifTrue: Behavior, ifFalse: Behavior)
        extends Behavior
    

    case class ActionPrefix(action: Label, following: Behavior) extends Behavior

    // In order to model systems of behavior equations, we need to have
    // variable-like entities. These are looked up in a context.
    case class Variable(name: String) extends Behavior
  }

  import scala.util.parsing.combinator._
  object Parsers extends JavaTokenParsers with RegexParsers {
    import Behavior._

    def action: Parser[Label] = ident

    def terminal: Parser[Terminal] = "S" ^^ { _ => Terminal() }

    def deadlock: Parser[Deadlock] = "D" ^^ { _ => Deadlock() }

    def actionPrefix: Parser[ActionPrefix] = (action ~ "." ~ atomic) ^^ {
      case a ~ _ ~ t => ActionPrefix(a, t)
    }

    def variable: Parser[Variable] =
      ("[" ~> """[^\]]+""".r <~ "]") map Variable

    def atomic: Parser[Behavior] =
      variable | terminal | deadlock | actionPrefix | "(" ~> behavior <~ ")"

    def postconditionalComposition: Parser[PostconditionalComposition] =
      (atomic ~ "<" ~ action ~ ">" ~ atomic) ^^ {
        case ifTrue ~ _ ~ a ~ _ ~ ifFalse =>
          PostconditionalComposition(a, ifTrue, ifFalse)
      }

    def behavior: Parser[Behavior] =
      postconditionalComposition | atomic
  }
  
  def parseBehavior(behavior: String): Behavior =
    Parsers.parse(Parsers.behavior, behavior).get
}