package example

object Hello extends Greeting with App {
  println(greeting)

  println(readInt("123"))

  def readInt(str: String): Option[Int] =
    if (str matches "-?\\d+") Some(str.toInt) else None
}

trait Greeting {
  lazy val greeting: String = "hello"
}
