package cats
package std

import dogs._
import dogs.Predef._
import scala.annotation.tailrec
import scala.collection.immutable.Vector


trait ListInstances {
  implicit val listInstance: Traverse[List] with MonadCombine[List] with CoflatMap[List] =
    new Traverse[List] with MonadCombine[List] with CoflatMap[List] {

      def neutral[A]: List[A] = El()

      def combine[A](x: List[A], y: List[A]): List[A] = x ::: y

      def pure[A](x: A): List[A] = Nel(x, El())

      override def map[A, B](fa: List[A])(f: A => B): List[B] =
        fa.map(f)

      def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
        fa.flatMap(f)

      override def map2[A, B, Z](fa: List[A], fb: List[B])(f: (A, B) => Z): List[Z] =
        fa.flatMap(a => fb.map(b => f(a, b)))

      def coflatMap[A, B](fa: List[A])(f: List[A] => B): List[B] = fa coflatMap f

      def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)

      def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
        def loop(as: List[A]): Eval[B] =
          as match {
            case El() => lb
            case Nel(h, t) => f(h, Eval.defer(loop(t)))
          }
        Eval.defer(loop(fa))
      }

      def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] = {
        val gba = G.pure(Vector.empty[B])
        val gbb = fa.foldLeft(gba)((buf, a) => G.map2(buf, f(a))(_ :+ _))
        G.map(gbb)(_.foldRight[List[B]](El())(Nel.apply))
      }

      override def exists[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.exists(p)

      override def forall[A](fa: List[A])(p: A => Boolean): Boolean =
        fa.forall(p)

      override def isEmpty[A](fa: List[A]): Boolean = fa.isEmpty

      override def toStreaming[A](fa: List[A]): Streaming[A] =
        Streaming.fromList(fa)
    }

  implicit def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    override def neutral: List[A] = El()
    override def combine(left: List[A], right: List[A]): List[A] = left ::: right

  }
}

trait NelInstances {
  implicit val nelInstance: Monad[Nel] with Comonad[Nel] with SemigroupK[Nel] =
    new Monad[Nel] with Comonad[Nel] with SemigroupK[Nel] {
      override def combine[A](x: Nel[A], y: Nel[A]): Nel[A] = x ::: y
      override def pure[A](x: A): Nel[A] = Nel(x, El())
      override def map[A, B](fa: Nel[A])(f: A => B): Nel[B] = fa map f
      override def flatMap[A, B](fa: Nel[A])(f: A => Nel[B]): Nel[B] = fa flatMap f
      override def coflatMap[A, B](fa: Nel[A])(f: Nel[A] => B): Nel[B] = fa coflatMap f
      override def extract[A](fa: Nel[A]): A = fa.head

    }

  implicit def nelSemigrooup[A]: Semigroup[Nel[A]] = Semigroup.instance[Nel[A]](_ ::: _)
}

