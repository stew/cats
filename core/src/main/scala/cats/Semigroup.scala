package cats

import simulacrum.{typeclass,op}

@typeclass trait Semigroup[A] {
  @op("|+|") def combine(l: A, r: A): A
}

object Semigroup {
  def instance[A](f: (A,A) => A): Semigroup[A] = new Semigroup[A] {
    override def combine(l: A, r: A): A = f(l,r)
  }
}

