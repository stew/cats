package cats
package std

import dogs.Show
import functor.Contravariant

trait ShowInstances {
  implicit def showContravariant: Contravariant[Show] =
    new Contravariant[Show] {
      override def contramap[A,B](fa: Show[A])(f: B=>A): Show[B] =
        fa contramap f
    }
}
