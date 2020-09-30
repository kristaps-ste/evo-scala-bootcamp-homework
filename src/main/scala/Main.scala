object Main extends App {

  def gcd(a: Int, b: Int): Int = {
    (a, b) match {
      case (a, b) if a == 0 => b
      case (a, b) if b == 0 => a
      case (a, b) if a > b  => gcd(b.abs, a.abs % b.abs)
      case (a, b) if a < b  => gcd(a.abs, b.abs % a.abs)
      case (a, b) if a == b => a
    }
  }

  def lcm(a: Int, b: Int): Option[Int] = {

    (a, b) match {
      case (a, b) if (a == 0 || b == 0) => None
      case (_, _)                       => Some(a.abs / gcd(a, b) * b.abs)
    }
  }

}
