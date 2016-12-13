package examples.cofree

import examples.cofree.Cofree.{ Lazy, Pipe, Sink }
import examples.main
import cats.implicits._

@main
object CofreeApp {
  val source: Cofree.Pipe[Lazy, Unit, Long] =
    Cofree.fibSource.collect {
      case x if x % 2 == 0 => x
    }.run(()).apply()

  val sink = Sink.foreach[Lazy, String](println)
  val pipe: Pipe[Lazy, Unit, Unit] = source.map(_.toString).toSink(sink)

  val out: Lazy[Unit] = pipe.run()

  out()
}
