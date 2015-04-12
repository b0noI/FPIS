package part4

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = es => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map(pa, unit(()))((a, _) => f(a))

  def map[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    es => UnitFuture(f(a(es).get, b(es).get))

  def fork[A](a: => Par[A]): Par[A] =
    es => es submit(new Callable[A] {
      override def call(): A = a(es) get
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def run[A](es: ExecutorService)(a: Par[A]) =
    a(es)

  private case class UnitFuture[A](value: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get: A = value

    override def get(timeout: Long, unit: TimeUnit): A = value

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

}