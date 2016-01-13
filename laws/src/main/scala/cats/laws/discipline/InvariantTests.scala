package cats
package laws
package discipline

import scala.{Some,None,Option}
import dogs.Predef._
import cats.functor.Invariant
import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import Prop._
import org.typelevel.discipline.Laws

trait InvariantTests[F[_]] extends Laws {
  def laws: InvariantLaws[F]

  def invariant[A: Arbitrary, B: Arbitrary, C: Arbitrary](implicit
    ArbFA: Arbitrary[F[A]],
    EqFA: Eq[F[A]],
    EqFC: Eq[F[C]]
  ): RuleSet = {

    new DefaultRuleSet(
      name = "invariant",
      parent = scala.None,
      "invariant identity" -> forAll(laws.invariantIdentity[A] _),
      "invariant composition" -> forAll(laws.invariantComposition[A, B, C] _))
  }
}

object InvariantTests {
  def apply[F[_]: Invariant]: InvariantTests[F] =
    new InvariantTests[F] { def laws: InvariantLaws[F] = InvariantLaws[F] }
}
