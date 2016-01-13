package cats
package std

import dogs._
import dogs.Predef._

trait AnyValInstances
  extends IntInstances
  with    ByteInstances
  with    CharInstances
  with    LongInstances
  with    ShortInstances
  with    FloatInstances
  with    DoubleInstances
  with    BooleanInstances
  with    UnitInstances

trait IntInstances {

  implicit val intShow: Show[Int] =
    Show.fromToString[Int]

  implicit val intMonoid = Monoid.instance[Int](0, _ + _)
}

trait ByteInstances {

  implicit val byteShow: Show[Byte] =
    Show.fromToString[Byte]
}

trait CharInstances {

  implicit val charShow: Show[Char] =
    Show.fromToString[Char]

}

trait ShortInstances  {

  implicit val shortShow: Show[Short] =
    Show.fromToString[Short]
}

trait LongInstances /* missing algebra type classes */ {

  implicit val longShow: Show[Long] =
    Show.fromToString[Long]

  implicit val longMonoid: Monoid[Long] = Monoid.instance(0L, _ + _)
}

trait FloatInstances /* missing algebra type classes */ {

  implicit val floatShow: Show[Float] =
    Show.fromToString[Float]
}

trait DoubleInstances /* missing algebra type classes */ {

  implicit val doubleShow: Show[Double] =
    Show.fromToString[Double]
}

trait BooleanInstances {

  implicit val booleanShow: Show[Boolean] =
    Show.fromToString[Boolean]

  val disjunctionMonoid: Monoid[Boolean] = Monoid.instance(false, _ || _)
  val conjunctionMonoid: Monoid[Boolean] = Monoid.instance(true, _ && _)

}

trait UnitInstances /* missing algebra type classes */ {

  implicit val unitShow: Show[Unit] =
    Show.fromToString[Unit]

  implicit val unitMonoid: Monoid[Unit] = Monoid.instance((), (x,y) => ())
}
