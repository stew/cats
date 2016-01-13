package cats
package laws
package discipline

import dogs._
import dogs.Predef._
import scala.MatchError
import scala.{Function,Nil}
import scala.collection.immutable.Seq
import cats.data._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.{arbitrary => getArbitrary}

/**
 * Arbitrary instances for cats.data
 */
object arbitrary extends ArbitraryInstances0 {

  implicit def xorTArbitrary[F[_], A, B](implicit F: Arbitrary[F[A Xor B]]): Arbitrary[XorT[F, A, B]] =
    Arbitrary(F.arbitrary.map(XorT(_)))

  implicit def kleisliArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Kleisli[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Kleisli[F, A, B](_ => fb)))

  implicit def cokleisliArbitrary[F[_], A, B](implicit B: Arbitrary[B]): Arbitrary[Cokleisli[F, A, B]] =
    Arbitrary(B.arbitrary.map(b => Cokleisli[F, A, B](_ => b)))

  implicit def optionTArbitrary[F[_], A](implicit F: Arbitrary[F[Option[A]]]): Arbitrary[OptionT[F, A]] =
    Arbitrary(F.arbitrary.map(OptionT.apply))

  implicit def evalArbitrary[A: Arbitrary]: Arbitrary[Eval[A]] =
    Arbitrary(Gen.oneOf(
      getArbitrary[A].map(Eval.now(_)),
      getArbitrary[A].map(Eval.later(_)),
      getArbitrary[A].map(Eval.always(_))))

  implicit def prodArbitrary[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Prod[F, G, A]] =
    Arbitrary(F.arbitrary.flatMap(fa => G.arbitrary.map(ga => Prod[F, G, A](fa, ga))))

  implicit def funcArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]]): Arbitrary[Func[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.func[F, A, B](_ => fb)))

  implicit def appFuncArbitrary[F[_], A, B](implicit F: Arbitrary[F[B]], FF: Applicative[F]): Arbitrary[AppFunc[F, A, B]] =
    Arbitrary(F.arbitrary.map(fb => Func.appFunc[F, A, B](_ => fb)))

  implicit def writerArbitrary[L:Arbitrary, V:Arbitrary]: Arbitrary[Writer[L, V]] =
    writerTArbitrary[Id, L, V]

  // until this is provided by scalacheck
  implicit def partialFunctionArbitrary[A, B](implicit F: Arbitrary[A => Option[B]]): Arbitrary[PartialFunction[A, B]] =
    Arbitrary(F.arbitrary.map { f => new PartialFunction[A,B] {
                                 override def isDefinedAt(a: A): Boolean = f(a).isDefined
                                 override def apply(a: A): B = f(a).cata(identity, throw new MatchError(a))
                               }})


  implicit def coproductArbitrary[F[_], G[_], A](implicit F: Arbitrary[F[A]], G: Arbitrary[G[A]]): Arbitrary[Coproduct[F, G, A]] =
    Arbitrary(Gen.oneOf(
      F.arbitrary.map(Coproduct.leftc[F, G, A]),
      G.arbitrary.map(Coproduct.rightc[F, G, A])))

  implicit def showArbitrary[A: Arbitrary]: Arbitrary[Show[A]] =
    Arbitrary(Show.fromToString[A])

  implicit def function0Arbitrary[A: Arbitrary]: Arbitrary[() => A] =
    Arbitrary(getArbitrary[A].map(() => _))
}

private[discipline] sealed trait ArbitraryInstances0 {
  implicit def writerTArbitrary[F[_], L, V](implicit F: Arbitrary[F[(L, V)]]): Arbitrary[WriterT[F, L, V]] =
    Arbitrary(F.arbitrary.map(WriterT(_)))
}
