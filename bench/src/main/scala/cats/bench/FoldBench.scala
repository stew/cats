package cats.bench

import dogs.Predef._
import dogs._
import cats.std.list._
import cats.std.const._
import cats.std.string._
import cats.{Foldable, Traverse}
import org.openjdk.jmh.annotations.{Benchmark, Scope, State}

@State(Scope.Benchmark)
class FoldBench {

  // STEW: TODO
  import scala.Predef.charWrapper
  val chars: List[String] = List.fromIterable(('a' to 'z').map(_.toString).toList)

  /** Benchmark fold of Foldable[List] */
  @Benchmark
  def fold(): String =
    Foldable[List].fold(chars)

  /** Benchmark fold using traverse with Const */
  @Benchmark
  def traverseConst(): String =
    Traverse[List].traverse[Const[String, ?], String, String](chars)(Const(_)).getConst

}
