package cats
package laws

import dogs.Predef._
import cats.syntax.all._

/**
 * Laws that must be obeyed by any `MonadFilter`.
 */
trait MonadFilterLaws[F[_]] extends MonadLaws[F] {
  implicit override def F: MonadFilter[F]

  def monadFilterLeftEmpty[A, B](f: A => F[B]): IsEq[F[B]] =
    F.neutral[A].flatMap(f) <-> F.neutral[B]

  def monadFilterRightEmpty[A, B](fa: F[A]): IsEq[F[B]] =
    fa.flatMap(_ => F.neutral[B]) <-> F.neutral[B]

  def monadFilterConsistency[A, B](fa: F[A], f: A => Boolean): IsEq[F[A]] =
    fa.filter(f) <-> fa.flatMap(a => if (f(a)) F.pure(a) else F.neutral)
}

object MonadFilterLaws {
  def apply[F[_]](implicit ev: MonadFilter[F]): MonadFilterLaws[F] =
    new MonadFilterLaws[F] { def F: MonadFilter[F] = ev }
}
