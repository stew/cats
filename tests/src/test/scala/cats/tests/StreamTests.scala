package cats
package tests

import dogs._
import dogs.Predef._
import dogs.std._
import cats.laws.discipline.{CoflatMapTests, MonadCombineTests, SerializableTests, TraverseTests, MonoidalTests}
import cats.laws.discipline.eq.tuple3Eq

class StreamTests extends CatsSuite {
  checkAll("Stream[Int]", MonoidalTests[Stream].monoidal[Int, Int, Int])
  checkAll("Monoidal[Stream]", SerializableTests.serializable(Monoidal[Stream]))

  checkAll("Stream[Int]", CoflatMapTests[Stream].coflatMap[Int, Int, Int])
  checkAll("CoflatMap[Stream]", SerializableTests.serializable(CoflatMap[Stream]))

  checkAll("Stream[Int]", MonadCombineTests[Stream].monadCombine[Int, Int, Int])
  checkAll("MonadCombine[Stream]", SerializableTests.serializable(MonadCombine[Stream]))

  checkAll("Stream[Int] with Option", TraverseTests[Stream].traverse[Int, Int, Int, List[Int], Option, Option])
  checkAll("Traverse[Stream]", SerializableTests.serializable(Traverse[Stream]))
}
