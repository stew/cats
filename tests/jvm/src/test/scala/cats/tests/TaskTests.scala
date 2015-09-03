package cats
package tests

class TaskTests extends CatsSuite {
  import cats.data.Xor

  test("side-effects aren't run until .run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    assert(run == 0)
    t.run
    assert(run == 1)
  }

  test("side-effects are only evaulated once for Task.later") {
    var run: Int = 0
    val t: Task[Int] = Task.later { run += 1; run }
    t.run
    assert(run == 1)

    t.run
    assert(run == 1)

    val t2 = t.map(_ + 1)
    t2.run
    t2.run

    assert(run == 1)

    val t3 = t.flatMap(x => Task.now(x + 1))
    t3.run
    t3.run

    assert(run == 1)
  }

  test("async is not run until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    assert(run == 0)

    t.run
    assert(run == 1)
  }

  test("async is not run multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.async { cb => run += 1; cb(1) }
    assert(run == 0)

    t.run
    assert(run == 1)

    t.run
    assert(run == 2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.run
    assert(run == 3)
  }

  test("always not run until run is called") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    assert(run == 0)

    t.run
    assert(run == 1)
  }

  test("always is run multiple times if the task is rum multiple times") {
    var run: Int = 0
    val t: Task[Int] = Task.always { () => run += 1; 1 }
    assert(run == 0)

    t.run
    assert(run == 1)

    t.run
    assert(run == 2)

    val t2 = t.flatMap(x => Task.now(x + 1))
    t2.run
    assert(run == 3)
  }

  val ones = Vector(Task.now(1),
                    Task.later(1),
                    Task.always(() => 1),
                    Task.async[Int](_(1)))


  import cats.data.Streaming
  // an infinite stream of ones
  val onesStream: Streaming[Task[Int]] = Streaming.continually(Streaming.fromVector(ones)).flatten

  test("attempt puts success on the right") {
    val attempted = ones.map(_.attempt.run)
    println(attempted)
    assert(attempted.forall(_ == Xor.Right(1)))
  }

  test("flatMap is stacksafe") {
    val howmany = 1000000

    // StateT relies on a stacksafe effect monad in order to ensure
    // stack safety, so we will create an infinite stream of monadic
    // computations using StateT, and see if we can pull a ton of
    // values out of the stream without blowing our stack anywhere.
    case class State[F[_],S,A](next: S => F[(S,A)]) 

    implicit def stateMonad[F[_]: Monad,S]: Monad[State[F,S,?]] =
      new Monad[State[F,S,?]] {
        override def pure[A](a: A) = State(s => Monad[F].pure(s -> a))
        override def flatMap[A,B](fa: State[F,S,A])(f: A => State[F,S,B]) =
          State { previous =>
          for {
            n     <- fa.next(previous)
            (s,a)  = n
            res   <- f(a).next(s)
          } yield res
        }
    }

    type TaskInt[A] = State[Task,Int,A]

    def runStateS[F[_] : Bimonad,S](st: State[F,S,_], s: S): S =
      st.next(s).extract._1

    val computation  =
      Traverse[Streaming].traverse[TaskInt,Task[Int],Unit](onesStream.take(howmany)){ ta =>
        State[Task,Int,Unit](i => ta.map(a => (i+a, ())))
      }

    assert(runStateS[Task,Int](computation, 0) == howmany)
  }
}
