package cats
package std

import dogs.Predef._

trait StringInstances {
  implicit def stringMonoid: Monoid[String] = new Monoid[String] {
    override def neutral: String = ""
    override def combine(l: String, r: String): String = l + r
  }
}
