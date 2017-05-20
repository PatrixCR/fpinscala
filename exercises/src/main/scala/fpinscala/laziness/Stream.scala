package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def toList: List[A] = this.foldRight(List.empty[A])((a, l) => a :: l)

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
      case _ => Stream.empty[A]
    }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
      case _ => Stream.empty[A]
    }

  def forAll(p: A => Boolean): Boolean = this.foldRight(true)(
    (a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])(
    (a, s) => if (p(a)) Stream.cons(a, s) else Stream.empty[A])

  def headOption: Option[A] = this.foldRight(None:Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = this.foldRight(Stream.empty[B])((a, s) => Stream.cons(f(a), s))

  def filter(f: A => Boolean): Stream[A] = this.foldRight(Stream.empty[A])((a , s) => if (f(a)) Stream.cons(a, s) else s)

  def append[B >: A](stream: => Stream[B]): Stream[B] = this.foldRight(stream)((a, s) => Stream.cons(a, s))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this.foldRight(Stream.empty[B])((a, s) => f(a).append(s))

  def startsWith[B](s: Stream[B]): Boolean =
    this.zipAll(s).takeWhile2 { case (a, b) => b.isDefined }.forAll { case (a, b) => a == b }

  def map2[B](f: A => B): Stream[B] = Stream.unfold(this)({
    case (Cons(h, t)) => Some(f(h()), t())
    case _ => None
  })

  def take2(n: Int): Stream[A] = Stream.unfold(this, n)({
    case (Cons(h, t), nn) if nn > 0 => Some(h(), (t(), nn - 1))
    case _ => None
  })

  def takeWhile3(p: A => Boolean): Stream[A] = Stream.unfold(this)({
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  })

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C) : Stream[C] = Stream.unfold(this, s2)({
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  })

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = Stream.unfold(this, s2)({
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  })
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def fibsInternal(i: Int, j: Int): Stream[Int] = Stream.cons(i, fibsInternal(j, i + j))
    fibsInternal(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).map { case (a, s) => Stream.cons(a, unfold(s)(f)) }.getOrElse(Stream.empty[A])

  val ones2: Stream[Int] = Stream.unfold(1)(s => Some((s, s)))

  def constant2[A](a: A): Stream[A] = Stream.unfold(a)(s => Some(s, s))

  def from2(n: Int): Stream[Int] = Stream.unfold(n)(s => Some(s, s + 1))

  def fibs2(): Stream[Int] = Stream.unfold(0, 1)(s => Some(s._1, (s._2, s._1 + s._2)))
}