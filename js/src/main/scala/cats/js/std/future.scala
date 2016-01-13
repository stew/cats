package cats
package js
package std

import scala.{Option,Some,None}
import dogs.Predef._
import java.lang.IllegalStateException

import scala.concurrent.Future
import scala.concurrent.{ExecutionContext => E}
import scala.concurrent.duration.FiniteDuration

import cats.std.FutureCoflatMap

object future extends FutureInstances0

object Await {
  def result[A](f: Future[A], atMost: FiniteDuration): A = f.value match {
      case Some(v) => v.get
      case None => throw new IllegalStateException()
    }
}

private[std] sealed trait FutureInstances0 extends FutureInstances1 {
  def futureComonad(atMost: FiniteDuration)(implicit ec: E): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {
      def extract[A](x: Future[A]): A =
        Await.result(x, atMost)
    }

/*  def futureOrder[A](atMost: FiniteDuration)(implicit ec: E, oa: Order[A]): Order[Future[A]] =
    new Order[Future[A]] {
      def compare(x: Future[A], y: Future[A]): Int =
        Await.result((x zip y).map { case (x, y) => oa.compare(x, y) }, atMost)
    }
 */
}

private[std] sealed trait FutureInstances1 extends FutureInstances2 {
/*  def futurePartialOrder[A](atMost: FiniteDuration)(implicit ec: E, oa: PartialOrder[A]): PartialOrder[Future[A]] =
    new PartialOrder[Future[A]] {
      def partialCompare(x: Future[A], y: Future[A]): Double =
        Await.result((x zip y).map { case (x, y) => oa.partialCompare(x, y) }, atMost)
    }
 */
}

private[std] sealed trait FutureInstances2 {
  def futureEq[A](atMost: FiniteDuration)(implicit ec: E, eqa: Eq[A]): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map { case (x, y) => eqa.eqv(x, y) }, atMost)
    }
}
