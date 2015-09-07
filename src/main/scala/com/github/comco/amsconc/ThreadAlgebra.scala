package com.github.comco.amsconc

/**
 * @author comco
 */
object ThreadAlgebra {
  type Label = String

  sealed trait Term {
    import Term._
    
    lazy val toAtomicCompactString: String = this match {
      case Termination | Deadlock | Variable(_) | ActionPrefix(_, _) =>
        toCompactString
      case _ => "(" + toCompactString + ")"
    }
    
    lazy val toCompactString: String = this match {
      case Termination => "S"
      case Deadlock => "D"
      case Variable(name) => s"'$name"
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

  object Term {
    case object Termination extends Term

    case object Deadlock extends Term

    case class PostconditionalComposition(
      action: Label, ifTrue: Term, ifFalse: Term)
        extends Term

    case class ActionPrefix(action: Label, following: Term) extends Term

    // In order to model systems of behavior equations, we need to have
    // variable-like entities. These are looked up in a context.
    case class Variable(name: String) extends Term
  }

  import scala.util.parsing.combinator._
  object Parsers extends JavaTokenParsers with RegexParsers {
    import Term._

    def action: Parser[Label] = ident

    def termination: Parser[Termination.type] = "S" ^^ { _ => Termination }

    def deadlock: Parser[Deadlock.type] = "D" ^^ { _ => Deadlock }

    def actionPrefix: Parser[ActionPrefix] = (action ~ "." ~ atomic) ^^ {
      case a ~ _ ~ t => ActionPrefix(a, t)
    }

    def variable: Parser[Variable] =
      (("'" ~> ident) | ("'[" ~> """[^\]]+""".r <~ "]")) map Variable

    def atomic: Parser[Term] =
      variable | termination | deadlock | actionPrefix | "(" ~> term <~ ")"

    def postconditionalComposition: Parser[PostconditionalComposition] =
      (atomic ~ "<" ~ action ~ ">" ~ atomic) ^^ {
        case ifTrue ~ _ ~ a ~ _ ~ ifFalse =>
          PostconditionalComposition(a, ifTrue, ifFalse)
      }

    def term: Parser[Term] =
      postconditionalComposition | atomic
  }
  
  def parseTerm(term: String): Term = Parsers.parse(Parsers.term, term).get
}