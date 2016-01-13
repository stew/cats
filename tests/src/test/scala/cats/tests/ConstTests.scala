package cats
package tests

import dogs.Predef._
import dogs._
import dogs.tests.arbitrary.all._
import cats.functor.Contravariant
import cats.std.const._
import cats.laws.discipline._

class ConstTests extends CatsSuite {

  implicit val iso = MonoidalTests.Isomorphisms.invariant[Const[String, ?]](constTraverse)

  checkAll("Const[String, Int]", MonoidalTests[Const[String, ?]].monoidal[Int, Int, Int])
  checkAll("Monoidal[Const[String, ?]]", SerializableTests.serializable(Monoidal[Const[String, ?]]))

  checkAll("Const[String, Int]", ApplicativeTests[Const[String, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Const[String, ?]]", SerializableTests.serializable(Applicative[Const[String, ?]]))

  checkAll("Const[String, Int] with Option", TraverseTests[Const[String, ?]].traverse[Int, Int, Int, Int, Option, Option])
  checkAll("Traverse[Const[String, ?]]", SerializableTests.serializable(Traverse[Const[String, ?]]))

  // Get Apply[Const[C : Semigroup, ?]], not Applicative[Const[C : Monoid, ?]]
  {
    implicit def nonEmptyListSemigroup[A]: Semigroup[Nel[A]] = SemigroupK[Nel].algebra
    implicit val iso = MonoidalTests.Isomorphisms.invariant[Const[Nel[String], ?]](Const.constContravariant)
    checkAll("Apply[Const[Nel[String], Int]]", ApplyTests[Const[Nel[String], ?]].apply[Int, Int, Int])
    checkAll("Apply[Const[Nel[String], ?]]", SerializableTests.serializable(Apply[Const[Nel[String], ?]]))
  }

  // Algebra checks for Serializability of instances as part of the laws
  checkAll("Monoid[Const[Int, String]]", GroupLaws[Const[Int, String]].monoid)

  checkAll("Const[Nel[Int], String]", GroupLaws[Const[Nel[Int], String]].semigroup)

  // Note while Eq is a superclass of PartialOrder and PartialOrder a superclass
  // of Order, you can get different instances with different (more general) constraints.
  // For instance, you can get an Order for Const if the first type parameter has an Order,
  // but you can also get just an Eq for Const if the first type parameter has just an Eq
  checkAll("Const[Map[Int, Int], String]", OrderLaws[Const[Map[Int, Int], String]].eqv)
  checkAll("PartialOrder[Const[Set[Int], String]]", OrderLaws[Const[Set[Int], String]].partialOrder)
  checkAll("Order[Const[Int, String]]", OrderLaws[Const[Int, String]].order)

  checkAll("Const[String, Int]", ContravariantTests[Const[String, ?]].contravariant[Int, Int, Int])
  checkAll("Contravariant[Const[String, ?]]", SerializableTests.serializable(Contravariant[Const[String, ?]]))

  test("show") {

    Const(1).show should === ("Const(1)")

    forAll { const: Const[Int, String] =>
      const.show.startsWith("Const(") should === (true)
      const.show.contains(const.getConst.show)
      const.show should === (implicitly[Show[Const[Int, String]]].show(const))
      const.show should === (const.retag[Boolean].show)
    }
  }



}
