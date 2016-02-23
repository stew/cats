package cats
package jvm
package tests

import cats.data.{Streaming,Xor}
import cats.tests.CatsSuite
import cats.laws.discipline._
import cats.laws.discipline.eq.tuple3Eq
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary

class TaskTests extends CatsSuite {

  test("Task should not run side-effects until .unsafePerformIO() is called") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    run should be (0)
    t.unsafePerformIO()
    run should be (1)
  }

  test("Task should only evaulate side-effects once for Task.later") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    t.unsafePerformIO()
    run should be(1)

    t.unsafePerformIO()
    run should be (1)

    val t2 = t.map(_ + 1)
    t2.unsafePerformIO()
    t2.unsafePerformIO()

    run should be (1)

    val t3 = t.flatMap(x => Task.now(x + 1))
    t3.unsafePerformIO()
    t3.unsafePerformIO()

    run should be (1)
  }

  test("Task should not run async until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    run should be (0)

    t.unsafePerformIO()
    run should be (1)
  }

  test("Task should run async run multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    run should be (0)

    t.unsafePerformIO()
    run should be (1)

    t.unsafePerformIO()
    run should be (2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.unsafePerformIO()
    run should be (3)
  }

  test("Task should not run always until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    run should be (0)

    t.unsafePerformIO()
    run should be (1)
  }

  test("Task should run always multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    run should be (0)

    t.unsafePerformIO()
    run should be (1)

    t.unsafePerformIO()
    run should be (2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.unsafePerformIO()
    run should be (3)
  }

  val ones = List(Task.now(1),
                  Task.later(1),
                  Task.always(() => 1),
                  Task.async[Int](_(1)))



  test("Task should put success on the right when catch is called") {
    val attempted = ones.map(_.catchNonFatal.unsafePerformIO())
    attempted.forall(_ == Xor.Right(1)) should be (true)
  }


  // an infinite stream of ones
  val onesStream: Streaming[Task[Int]] = Streaming.continually(Streaming.fromList(ones)).flatMap(x => x)


  def taskMap2[A,B,C](t1: Task[A], t2: Task[B])(f: (A,B) => C): Task[C] = {
    t1.flatMap(a => t2.map(b => f(a,b)))
  }

  def sequenceStreaming[A](fa: Streaming[Task[A]]): Task[Streaming[A]] = {
    fa.foldRight(Eval.later(Task.now(Streaming.empty[A])))((a, st) =>
      st.map(b => taskMap2(b,a)((x,y) => Streaming.cons(y,x)))).value
  }

  test("Task should have a stack-safe flatMap") {
    val howmany = 1000000

    sequenceStreaming(onesStream.take(howmany)).unsafePerformIO().foldLeft(0)((x, _) => x + 1) should be (howmany)

    onesStream.take(howmany).sequence.unsafePerformIO().foldLeft(0)((x, _) => x + 1) should be (howmany)
  }

  test("Task should run forked tasks on another thread") {

    implicit val pool = new java.util.concurrent.ForkJoinPool

    var time1: Long = 0
    var time2: Long = 0

    val t1: Task[Unit] = Task.later {
      Thread.sleep(2000)
      time1 = System.currentTimeMillis
    }.fork(pool)

    val t2: Task[Unit] = Task.later {
      Thread.sleep(1000)
      time2 = System.currentTimeMillis
      ()
    }.fork(pool)

    val cdl = new java.util.concurrent.CountDownLatch(2)

    t1.unsafeAsyncIO(_ => cdl.countDown)
    t2.unsafeAsyncIO(_ => cdl.countDown)

    time1 should be(0L)
    time2 should be(0L)

    cdl.await

    time2 should be > 0L
    time1 should be > time2
  }

  implicit def taskEq[A](implicit A: Eq[A]): Eq[Task[A]] =
    new Eq[Task[A]] {
      def eqv(x: Task[A], y: Task[A]): Boolean =
        A.eqv(x.unsafePerformIO(), y.unsafePerformIO())
    }

  implicit val arbitraryTask: Arbitrary[Task[Int]] = Arbitrary {
    arbitrary[Int].flatMap(i => Gen.oneOf(Task.now(i), Task.later(i), Task.always(() => i), Task.async[Int](cb => cb(i))))
  }

  implicit val arbitraryTaskF: Arbitrary[Task[Int => Int]] = Arbitrary {
    arbitrary[Int].flatMap(i => Gen.oneOf(Task.now((_:Int) => i), Task.later((_:Int) => i), Task.always(() => (_:Int) => i), Task.async[Int => Int](cb => cb(Function.const(i)))))
  }

  checkAll("Task[Int]", MonadTests[Task].monad[Int, Int, Int])
}


