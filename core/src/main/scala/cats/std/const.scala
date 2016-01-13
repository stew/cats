package cats
package std

import dogs.{Const,Eval}
import dogs.Predef._
import functor.Contravariant

private[std] trait ConstInstances extends ConstInstances0 {

  implicit def constContravariant[C]: Contravariant[Const[C, ?]] = new Contravariant[Const[C, ?]] {
    override def contramap[A, B](fa: Const[C, A])(f: (B) => A): Const[C, B] =
      fa.retag[B]
  }

  implicit def constTraverse[C]: Traverse[Const[C, ?]] = new Traverse[Const[C, ?]] {
    def traverse[G[_], A, B](fa: Const[C, A])(f: A => G[B])(implicit G: Applicative[G]): G[Const[C, B]] =
      G.pure(fa.retag[B])

    def foldLeft[A, B](fa: Const[C, A], b: B)(f: (B, A) => B): B = b

    def foldRight[A, B](fa: Const[C, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = lb
  }

  implicit def constMonoid[A, B](implicit A: Monoid[A]): Monoid[Const[A, B]] = new Monoid[Const[A, B]]{
    def neutral: Const[A, B] = Const(A.neutral)

    def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] =
      Const(A.combine(x.getConst, y.getConst))
  }
}

private[std] sealed trait ConstInstances0 extends ConstInstances1 {

  implicit def constSemigroup[A, B](implicit A: Semigroup[A]): Semigroup[Const[A, B]] = new Semigroup[Const[A, B]] {
    def combine(x: Const[A, B], y: Const[A, B]): Const[A, B] = Const(A.combine(x.getConst, y.getConst))
  }

/*  implicit def constPartialOrder[A: PartialOrder, B]: PartialOrder[Const[A, B]] = new PartialOrder[Const[A, B]]{
    def partialCompare(x: Const[A, B], y: Const[A, B]): Double =
      x partialCompare y
  }
 */
  implicit def constApplicative[C](implicit C: Monoid[C]): Applicative[Const[C, ?]] = new Applicative[Const[C, ?]] {
    def pure[A](x: A): Const[C, A] = Const(C.neutral)

    def ap[A, B](fa: Const[C, A])(f: Const[C, A => B]): Const[C, B] =
      Const(C.combine(f.getConst, fa.getConst))

    def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
      fa.retag[B]

    def product[A, B](fa: Const[C, A], fb: Const[C, B]): Const[C, (A, B)] =
      Const(C.combine(fa.getConst, fb.getConst))
  }
}

private[std] sealed trait ConstInstances1 {
  implicit def constEq[A: Eq, B](implicit eqa: Eq[A]): Eq[Const[A, B]] = new Eq[Const[A, B]] {
    def eqv(x: Const[A, B], y: Const[A, B]): Boolean = eqa.eqv(x.getConst, y.getConst)
  }

  implicit def constApply[C](implicit C: Semigroup[C]): Apply[Const[C, ?]] = new Apply[Const[C, ?]] {
    def ap[A, B](fa: Const[C, A])(f: Const[C, A => B]): Const[C, B] =
      Const(C.combine(fa.getConst, f.getConst))

    def product[A, B](fa: Const[C, A], fb: Const[C, B]): Const[C, (A, B)] =
      Const(C.combine(fa.getConst, fb.getConst))

    def map[A, B](fa: Const[C, A])(f: A => B): Const[C, B] =
      fa.retag[B]
  }
}
