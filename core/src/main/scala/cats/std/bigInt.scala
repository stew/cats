package cats
package std

import dogs.Show
import dogs.Predef._

trait BigIntInstances {
  implicit val bigIntShow: Show[BigInt] =
    Show.fromToString[BigInt]
}
