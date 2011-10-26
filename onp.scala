
type MyStr = List[Char]

sealed abstract class Result[A]
case class Success[A](c:A, remainder:MyStr) extends Result[A]
case class Failure[A](message:String, remainder:MyStr) extends Result[A]

type Parser[A] = (MyStr) => Result[A]

def char(target:Char) : Parser[Char] = (input =>
  input match {
    case c::cs if c==target => Success(c, cs)
    case _ => Failure("Next char is not " + target, input)
  })

def oneOf(targets:List[Char], input:MyStr) : Result[Char] =
  input match {
    case Nil => Failure("Too few characters", input)
    case c::cs if targets.contains(c) => Success(c, cs)
    case _ => Failure("Next char is not any of " + targets, input )
  }

def charRange(start:Char, end:Char):List[Char] = List.range(start, end+1).map(_.toChar)
def alphaChar(input:MyStr) : Result[Char] = oneOf(charRange('a','z'), input)
def digitChar(input:MyStr) : Result[Char] = oneOf(charRange('0','9'), input)

def number(input:MyStr) : Result[Int] =
  digitChar(input) match {
    case Success(c, t) => Success(c - '0', t)
    case Failure(e, rem) => Failure(e, rem)
  }

def word(input:MyStr) : Result[Char] =
  alphaChar(input)


def thenLeft[A, B](first: Parser[A], second: Parser[B]): Parser[A] = (input =>
      first(input) match {
        case Failure(e, rem) => Failure(e, rem)
        case Success(v1, rem) => second(rem) match {
          case Failure(e2, rem2) => Failure(e2, rem2)
          case Success(_, rem2) => Success(v1, rem2)
        }
      })

def then[A, B](first: Parser[A], second: Parser[B]): Parser[(A,B)] = (input =>
      first(input) match {
        case Failure(e, rem) => Failure(e, rem)
        case Success(v1, rem) => second(rem) match {
          case Failure(e, rem2) => Failure(e, rem2)
          case Success(v2, rem2) => Success((v1, v2), rem2)
        }
      })

case class Asgnmt(k:Char, v:Int)
def pAssignment : Parser[Asgnmt] = (input =>
    then(thenLeft(alphaChar, char('=')), number)(input) match {
      case Success((k, v), rem) => Success(Asgnmt(k, v), rem)
      case Failure(e, rem) => Failure(e, rem)
    }
  )

then(then(alphaChar, digitChar), alphaChar) ("fa1l".toList)
then(then(alphaChar, digitChar), alphaChar) ("s7cs".toList)
pAssignment("a=7".toList)
pAssignment("ab7".toList)
pAssignment("a=".toList)

type Superint = Int
class Helper (left: Superint) {
  def power(right: Superint) = scala.math.pow(left, right)
}

implicit def Int2Helper (s:Superint) =
  new Helper(s)

val two2five = 2 power 5

