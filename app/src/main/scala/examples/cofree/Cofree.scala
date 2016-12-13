package examples.cofree

import cats.{ Applicative, FlatMap, Functor, Monad, Monoid, ~> }
import Cofree.{ Lazy, Pipe, Sink }
import cats.data.Kleisli
import cats.implicits._

final case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]]) {
  def map[B](f: A => B)(implicit F: Functor[F]): Cofree[F, B] =
    Cofree(f(head), F.map(tail)(_.map(f)))
  def zip[B](other: Cofree[F, B])(implicit F: Applicative[F]): Cofree[F, (A, B)] =
    Cofree((head, other.head), (tail |@| other.tail).map(_ zip _))

  def collect[B](f: PartialFunction[A, B])(implicit F: Monad[F]): F[Cofree[F, B]] =
    if (f.isDefinedAt(head)) {
      Cofree(f(head), tail.flatMap(_.collect(f))).pure[F]
    } else {
      tail.flatMap(_.collect(f))
    }

  def mapK[G[_]](f: F ~> G)(implicit F: Functor[F]): Cofree[G, A] =
    Cofree(head, f(tail.map(_.mapK(f))))

  def flatMap[B](f: A => Cofree[F, B])(implicit F: Functor[F]): Cofree[F, B] =
    f(head).flatMap { b =>
      Cofree(b, tail.map(_.flatMap(f)))
    }

  def take(count: Int)(implicit F: Monad[F]): F[List[A]] =
    if (count == 0) {
      List.empty[A].pure[F]
    } else {
      tail.flatMap(x => x.take(count - 1).map(t => head :: t))
    }

}

object Cofree {

  implicit class PipeOps[F[_], A, B](val self: Pipe[F, A, B]) extends AnyVal {
    def tail(in: A)(implicit F: Monad[F]): F[Pipe[F, A, B]] = self.tail.run(in)
    def run(in: A)(implicit F: Monad[F]): F[Unit] = tail(in).flatMap(_.run(in))

    def toSink(sink: Pipe[F, B, Unit])(implicit F: Monad[F]): Pipe[F, A, Unit] =
      Cofree[Kleisli[F, A, ?], Unit](
        (),
        Kleisli(in => (tail(in) |@| sink.tail(self.head)).map(_ toSink _))
      )

  }

  type Pipe[F[_], A, B] = Cofree[Kleisli[F, A, ?], B]
  type Source[F[_], A] = Pipe[F, Unit, A]
  type Sink[F[_], A] = Pipe[F, A, Unit]
  object Pipe {
    def map[F[_], A, B](f: A => B)(implicit F: Applicative[F], B: Monoid[B]): Pipe[F, A, B] = {
      def tail: Kleisli[F, A, Pipe[F, A, B]] =
        Kleisli[F, A, Pipe[F, A, B]](in => Cofree[Kleisli[F, A, ?], B](f(in), tail).pure[F])
      Cofree[Kleisli[F, A, ?], B](B.empty, tail)
    }

    def run[F[_]](f: Pipe[F, Unit, Unit])(implicit F: FlatMap[F]): F[Unit] =
      f.tail.run().flatMap(run(_))
  }

  object Sink {
    def foreach[F[_], A](f: A => Unit)(implicit F: Applicative[F]): Sink[F, A] =
      Pipe.map[F, A, Unit](f)
  }

  type Lazy[A] = () => A

  def Lazy[A](a: => A): Lazy[A] = () => a

  def fib: Cofree[Lazy, Long] = {
    def help(prev1: Long, prev2: Long): Cofree[Lazy, Long] =
      Cofree(prev1 + prev2, Lazy(help(prev2, prev1 + prev2)))
    help(0l, 1l)
  }

  val fibSource: Cofree[Kleisli[Lazy, Unit, ?], Long] = {
    def help(prev1: Long, prev2: Long): Cofree[Kleisli[Lazy, Unit, ?], Long] =
      Cofree[Kleisli[Lazy, Unit, ?], Long](
        prev1 + prev2,
        Kleisli(_ => Lazy(help(prev2, prev1 + prev2)))
      )
    help(0l, 1l)
  }

}
