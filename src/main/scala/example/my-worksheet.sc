def readInt(str: String): Option[Int] =
  if (str matches "-?\\d+") Some(str.toInt) else None

readInt("123")

readInt("abc").getOrElse(0)

def sum(optionA: Option[Int], optionB: Option[Int]): Option[Int] =
  optionA.flatMap { a => optionB.map(b => a + b) }

sum(readInt("1"), readInt("2"))
sum(readInt("1"), readInt("b"))


sealed trait Maybe[A] {
  def flatMap[B](fn: A => Seq[B]) =
    this match {
      case Empty() => Seq()
      case Full(value) => fn(value)
    }

  def map[B](fn: A => B) =
    this match {
      case Empty() => Seq()
      case Full(value) => Seq(value, value, value)
    }
}
final case class Full[A](value: A) extends Maybe[A]
final case class Empty[A]() extends Maybe[A]

for {
  a <- Full(123)
} yield a

Full(123).flatMap { a => Seq(1).map(b => b*2) }

def addOptions(v1: Option[Int], v2: Option[Int]) =
  for {
    a <- v1
    b <- v2
  } yield a + b

addOptions(Some(1), Some(2))
def addOptions(v1: Option[Int], v2: Option[Int]) =
  v1.flatMap { a => v2.map { b => a + b } }

addOptions(Some(1), Some(2))