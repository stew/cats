package cats
package tests

import dogs._
import dogs.Predef._
import dogs.std._
import org.scalacheck.Prop.forAll
import cats.laws.discipline._
import cats.laws.discipline.eq.tuple3Eq

class IdTests extends CatsSuite {
  implicit val iso = MonoidalTests.Isomorphisms.invariant[Id]

  checkAll("Id[Int]", BimonadTests[Id].bimonad[Int, Int, Int])
  checkAll("Bimonad[Id]", SerializableTests.serializable(Bimonad[Id]))

  checkAll("Id[Int]", TraverseTests[Id].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Id]", SerializableTests.serializable(Traverse[Id]))
}
