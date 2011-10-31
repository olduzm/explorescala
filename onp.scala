

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

def oneOf(targets:List[Char]) : Parser[Char] = (input =>
  input match {
    case Nil => Failure("Too few characters", input)
    case c::cs if targets.contains(c) => Success(c, cs)
    case _ => Failure("Next char is not any of " + targets, input )
  })

def charRange(start:Char, end:Char):List[Char] = List.range(start, end+1).map(_.toChar)
def alphaChar : Parser[Char] = (input => oneOf(charRange('a','z'))(input))
def digitChar : Parser[Char] = (input => oneOf(charRange('0','9'))(input))
def comma : Parser[Char] = char(',')

def number : Parser[Int] = (input =>
  digitChar(input) match {
    case Success(c, t) => Success(c - '0', t)
    case Failure(e, rem) => Failure(e, rem)
  })

def word : Parser[Char] = (input => alphaChar(input))

def thenRight[A, B](first: Parser[A], second: Parser[B]): Parser[B] = (input =>
    first(input) match {
      case Failure(e, rem) => Failure(e, rem)
      case Success(v1, rem2) => second(rem2)
    }
  )

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

def many[A](parser: Parser[A]) : Parser[List[A]] = (input =>
  parser(input) match {
    case Failure(e, rem) => Success(Nil, input)
    case Success(v, rem) => many(parser)(rem) match {
        case Success(vs, rem2) => Success(v::vs, rem2)
        case Failure(e, rem2) => Failure("Many failed unexpectedly! program bug!." + e, rem2)
    }
  })

def manySep1[A, B](itemParser: Parser[A], seperatorParser: Parser[B]) : Parser[List[A]] = (input =>
    {
      itemParser(input) match {
        case Failure(e, rem) => Failure(e, rem)
        case Success(v, rem) => many(thenRight(seperatorParser, itemParser))(rem) match {
            case Success(vs, rem2) => Success(v::vs, rem2)
            case Failure(e, rem2) => Failure("Many failed unexpectedly! program bug!." + e, rem2)
        }
      }
    }
  )

def pAssignmentArray = thenLeft(thenRight(char('['), manySep1(pAssignment, comma)), char(']'))

then(then(alphaChar, digitChar), alphaChar) ("fa1l".toList)
then(then(alphaChar, digitChar), alphaChar) ("s7cs".toList)
pAssignment("a=7".toList)
pAssignment("ab7".toList)
pAssignment("a=".toList)
manySep1(pAssignment, char(','))("a=5".toList)
manySep1(pAssignment, char(','))("a=5,b=8".toList)
manySep1(pAssignment, char(','))("a=5,b=8,c=9".toList)
manySep1(pAssignment, char(','))("a=5,b=8,c=9,d=1,e=3".toList)
manySep1(pAssignment, char(','))("a=5,b=8,c=9,d=1,e=3helloworld".toList)
manySep1(pAssignment, char(','))("a=5.b=8".toList)
many(digitChar)("".toList)
many(digitChar)("1".toList)
many(digitChar)("192834".toList)
many(digitChar)("192834notdigits".toList)
many(digitChar)("[a=5,b=8,c=9,d=1,e=3]XXXXX".toList)

class Helper[A] (left: Parser[A]) {
  def thenRight[B](right: Parser[B]): Parser[B] = (input =>
    left(input) match {
      case Failure(e, rem) => Failure(e, rem)
      case Success(v1, rem2) => right(rem2)
    }
  )
}

implicit def abcdef[A] (parser:Parser[A]) =
  new Helper(parser)

val implicitTest = alphaChar thenRight char('=') thenRight digitChar
implicitTest("a=7".toList)

