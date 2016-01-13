package cats

import simulacrum.typeclass

@typeclass trait Monoid[A] extends Semigroup[A] {
  def neutral: A
}

object Monoid {
  def instance[A](z: A, add: (A,A) => A): Monoid[A] = new Monoid[A] {
    override def neutral = z
    override def combine(l: A, r: A): A = add(l,r)
  }
}
