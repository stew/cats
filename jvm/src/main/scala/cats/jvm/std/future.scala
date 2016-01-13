package cats
package jvm
package std

import dogs.Predef._

import scala.concurrent.{Await, Future}
import scala.concurrent.{ExecutionContext => E}
import scala.concurrent.duration.FiniteDuration

import cats.std.FutureCoflatMap

object future extends FutureInstances0

private[std] sealed trait FutureInstances0 extends FutureInstances1 {
  def futureComonad(atMost: FiniteDuration)(implicit ec: E): Comonad[Future] =
    new FutureCoflatMap with Comonad[Future] {
      def extract[A](x: Future[A]): A =
        Await.result(x, atMost)
    }
}
/*  def futureOrder[A](atMost: FiniteDuration)(implicit ec: E, oa: Order[A]): Order[Future[A]] =
    new Order[Future[A]] {
      def compare(x: Future[A], y: Future[A]): Int =
        Await.result((x zip y).map { case (x, y) => oa.compare(x,y) }, atMost)
    }
}
 */
/*
private[std] sealed trait FutureInstances1 extends FutureInstances2 {

  def futurePartialOrder[A](atMost: FiniteDuration)(implicit ec: E, oa: PartialOrder[A]): PartialOrder[Future[A]] =
    new PartialOrder[Future[A]] {
      def partialCompare(x: Future[A], y: Future[A]): Double =
        Await.result((x zip y).map { case (x, y) => oa.partialCompare(x,y) }, atMost)
    }
}
 */

private[std] sealed trait FutureInstances1 {
  def futureEq[A](atMost: FiniteDuration)(implicit ec: E, eqa: Eq[A]): Eq[Future[A]] =
    new Eq[Future[A]] {
      def eqv(x: Future[A], y: Future[A]): Boolean =
        Await.result((x zip y).map { case (x, y) => eqa.eqv(x,y) }, atMost)
    }
}
