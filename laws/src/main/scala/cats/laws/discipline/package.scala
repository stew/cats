package cats
package laws

import dogs.Predef._
import dogs.Eq
import org.scalacheck._
import org.scalacheck.util.Pretty
import Prop.{False, Proof, Result}
import scala.StringContext

package object discipline {
  lazy val proved = Prop(Result(status = Proof))
  lazy val falsified = Prop(Result(status = False))

  def run[A](sym: String)(lhs: A, rhs: A)(f: (A, A) => Boolean): Prop =
    if (f(lhs, rhs)) proved else falsified :| {
      val exp = Pretty.pretty(lhs, Pretty.Params(0))
      val got = Pretty.pretty(rhs, Pretty.Params(0))
      s"($exp $sym $got) failed"
    }

  implicit class CheckEqOps[A](lhs: A)(implicit ev: Eq[A], pp: A => Pretty) {
    def ?==(rhs: A): Prop = run("?==")(lhs, rhs)(ev.eqv)
  }

  implicit def isEqToProp[A: Eq](isEq: IsEq[A]): Prop =
    isEq.lhs ?== isEq.rhs
}
