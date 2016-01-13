package cats
package std

import dogs.Show
import dogs.Predef._

trait BigDecimalInstances {
  implicit val bigDecimalShow: Show[BigDecimal] =
    Show.fromToString[BigDecimal]
}
