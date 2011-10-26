

type MyStr = List[Char]

sealed abstract class Result[A]
case class Success[A](c:A, remainder:MyStr) extends Result[A]
case class Failure[A](message:String, remainder:MyStr) extends Result[A]


def char(target:Char, input:MyStr) : Result[Char] =
  input match {
    case c::cs if c==target => Success(c, cs)
    case _ => Failure("Next char is not " + target, input)
  }

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

def then[A, B](first: (MyStr) => Result[A], second: (MyStr) => Result[B]): (MyStr) => Result[B] =
  new Function1[MyStr, Result[B]] {
    def apply(input: MyStr) = {
      first(input) match {
        case Success(c, t) => second(t)
        case Failure(e, rem) => Failure(e, rem)
      }
    }
  }


type Superint = Int
class Helper (left: Superint) {
  def power(right: Superint) = scala.math.pow(left, right)
}

implicit def Int2Helper (s:Superint) =
  new Helper(s)


