package cats
package std

import dogs._
import dogs.Predef._
import functor.Bifunctor
import Xor._

trait XorInstances extends XorInstances1 {
  implicit def xorMonoid[A, B](implicit A: Semigroup[A], B: Monoid[B]): Monoid[A Xor B] =
    new Monoid[A Xor B] {
      override def neutral: A Xor B = Xor.Right(B.neutral)
      override def combine(x: A Xor B, y: A Xor B): A Xor B = xorSemigroup[A,B].combine(x,y)
    }

  implicit def xorBifunctor: Bifunctor[Xor] =
    new Bifunctor[Xor] {
      override def bimap[A, B, C, D](fab: A Xor B)(f: A => C, g: B => D): C Xor D = fab.bimap(f, g)
    }

  implicit def xorInstances[A]: Traverse[A Xor ?] with MonadError[Xor[A, ?], A] =
    new Traverse[A Xor ?] with MonadError[Xor[A, ?], A] {
      def traverse[F[_], B, C](fa: A Xor B)(f: B => F[C])(implicit F: Applicative[F]): F[A Xor C] = fa match {
        case Right(b) => F.map(f(b))(Right.apply)
        case Left(l) => F.pure(Left(l))
      }
      def foldLeft[B, C](fa: A Xor B, c: C)(f: (C, B) => C): C = fa.foldLeft(c)(f)
      def foldRight[B, C](fa: A Xor B, lc: Eval[C])(f: (B, Eval[C]) => Eval[C]): Eval[C] = fa.foldRight(lc)(f)
      def flatMap[B, C](fa: A Xor B)(f: B => A Xor C): A Xor C = fa.flatMap(f)
      def pure[B](b: B): A Xor B = Xor.right(b)
      def handleErrorWith[B](fea: Xor[A, B])(f: A => Xor[A, B]): Xor[A, B] =
        fea match {
          case Xor.Left(e) => f(e)
          case r @ Xor.Right(_) => r
        }
      def raiseError[B](e: A): Xor[A, B] = Xor.left(e)
      override def map[B, C](fa: A Xor B)(f: B => C): A Xor C = fa.map(f)
      override def attempt[B](fab: A Xor B): A Xor (A Xor B) = Xor.right(fab)
      override def recover[B](fab: A Xor B)(pf: PartialFunction[A, B]): A Xor B =
        fab recover pf
      override def recoverWith[B](fab: A Xor B)(pf: PartialFunction[A, A Xor B]): A Xor B =
        fab recoverWith pf
    }
}

trait XorInstances1 {

  implicit def xorSemigroup[A, B](implicit A: Semigroup[A], B: Semigroup[B]): Semigroup[A Xor B] =
    new Semigroup[A Xor B] {
      override def combine(x: A Xor B, y: A Xor B): A Xor B = (x,y) match {
        case (Left(xa), Left(ya)) => Left(A.combine(xa,ya))
        case (Right(xb), Right(yb)) => Right(B.combine(xb,yb))
        case (x, Right(_)) => x
        case (Right(_), y) => y
      }
    }
}



