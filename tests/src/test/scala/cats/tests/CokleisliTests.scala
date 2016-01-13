package cats
package tests

import dogs._
import dogs.std._
import dogs.tests.arbitrary.all._
import dogs.Predef._
import cats.arrow.{Arrow, Split}
import cats.data.Cokleisli
import cats.functor.Profunctor
import cats.laws.discipline._
import cats.laws.discipline.arbitrary._
import cats.laws.discipline.eq._
import org.scalacheck.Arbitrary
import cats.laws.discipline.{SemigroupKTests, MonoidKTests}

class CokleisliTests extends SlowCatsSuite {
  implicit def cokleisliEq[F[_], A, B](implicit A: Arbitrary[F[A]], FB: Eq[B]): Eq[Cokleisli[F, A, B]] =
    Eq[F[A] => B].contramap(_.run)

  def cokleisliEqE[F[_], A](implicit A: Arbitrary[F[A]], FA: Eq[A]): Eq[Cokleisli[F, A, A]] =
    Eq[F[A] => A].contramap(_.run)

  implicit val iso = MonoidalTests.Isomorphisms.invariant[Cokleisli[Option, Int, ?]]

  checkAll("Cokleisli[Option, Int, Int]", MonoidalTests[Cokleisli[Option, Int, ?]].monoidal[Int, Int, Int])
  checkAll("Monoidal[Cokleisli[Option, Int, ?]", SerializableTests.serializable(Monoidal[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ApplicativeTests[Cokleisli[Option, Int, ?]].applicative[Int, Int, Int])
  checkAll("Applicative[Cokleisli[Option, Int, ?]", SerializableTests.serializable(Applicative[Cokleisli[Option, Int, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", ProfunctorTests[Cokleisli[Option, ?, ?]].profunctor[Int, Int, Int, Int, Int, Int])
  checkAll("Profunctor[Cokleisli[Option, ?, ?]", SerializableTests.serializable(Profunctor[Cokleisli[Option, ?, ?]]))

  checkAll("Cokleisli[Option, Int, Int]", SplitTests[Cokleisli[Option, ?, ?]].split[Int, Int, Int, Int, Int, Int])
  checkAll("Split[Cokleisli[Option, ?, ?]", SerializableTests.serializable(Split[Cokleisli[Option, ?, ?]]))

  {
    // Ceremony to help scalac to do the right thing, see also #267.
    type CokleisliNEL[A, B] = Cokleisli[Nel, A, B]

//    implicit def ev0[A: Arbitrary, B: Arbitrary]: Arbitrary[CokleisliNEL[A, B]] =
//      cokleisliArbitrary

//    implicit def ev1[A: Arbitrary, B: Eq]: Eq[CokleisliNEL[A, B]] =
//      cokleisliEq[Nel, A, B](arbNel, Eq[B])

    checkAll("Cokleisli[Nel, Int, Int]", ArrowTests[CokleisliNEL].arrow[Int, Int, Int, Int, Int, Int])
    checkAll("Arrow[Cokleisli[Nel, ?, ?]]", SerializableTests.serializable(Arrow[CokleisliNEL]))
  }

  {
    // More ceremony, see above
    type CokleisliNELE[A] = Cokleisli[Nel, A, A]

//    implicit def ev0[A: Arbitrary]: Arbitrary[CokleisliNELE[A]] =
//      cokleisliArbitrary[Nel, A, A]

//    implicit def ev1[A: Eq](implicit arb: Arbitrary[A]): Eq[CokleisliNELE[A]] =
//      cokleisliEqE[Nel, A](arbNel, Eq[A])

    {
      implicit val cokleisliMonoidK = Cokleisli.cokleisliMonoidK[Nel]
      checkAll("Cokleisli[Nel, Int, Int]", MonoidKTests[CokleisliNELE].monoidK[Int])
      checkAll("MonoidK[Lambda[A => Cokleisli[Nel, A, A]]]", SerializableTests.serializable(cokleisliMonoidK))
    }

    {
      implicit val cokleisliSemigroupK = Cokleisli.cokleisliSemigroupK[Nel]
      checkAll("Cokleisli[Nel, Int, Int]", SemigroupKTests[CokleisliNELE].semigroupK[Int])
      checkAll("SemigroupK[Lambda[A => Cokleisli[Nel, A, A]]]", SerializableTests.serializable(cokleisliSemigroupK))
    }

  }

}
