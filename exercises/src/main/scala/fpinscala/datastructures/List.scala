package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => throw new IllegalArgumentException("Cannot access tail of an empty list.")
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => throw new IllegalArgumentException("An empty list has no head to replace.")
  }

  @scala.annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Cons(_, t) => drop(t, n - 1)
      case Nil => throw new RuntimeException("Cannot drop from empty list.")
    }

  @scala.annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new IllegalArgumentException("An empty list has no init.")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, n) => 1 + n)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    @scala.annotation.tailrec
    def loop(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, t) => loop(t, f(acc, h))
    }
    loop(l, z)
  }

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((n, _) => 1 + n)

  def reverse[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => append(reverse(t), List(h))
  }

  def foldLeft2[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def append2[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)((a1, as2) => Cons(a1, as2))

  def flatten[A](ls: List[List[A]]): List[A] = {
    foldRight(ls, Nil: List[A])(append)
  }

  def add1(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(n, t) => Cons(n + 1, add1(t))
  }

  def toString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, ss) => Cons(d.toString, ss))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil: List[B])((x, xs) => Cons(f(x), xs))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  def add(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, add(xs, ys))
    case (Nil, Nil) => Nil
    case _ => sys.error("Lists must have same length.")
  }

  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
    case (Nil, _) => Nil
    case (_, Nil) => Nil
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def tails(l: List[A]): List[List[A]] = l match {
      case Nil => List(Nil)
      case Cons(h, t) => Cons(Cons(h, t), tails(t))
    }
    def matches(supTail: List[A]): Boolean = {
      supTail != Nil && foldLeft(zipWith(sub, supTail)((_, _)), true)((b, t) => b && (t._1 == t._2))
    }
    foldRight(tails(sup), false)(matches(_) || _)
  }

  def main(args: Array[String]): Unit = {
    val l = List(1,2,3,4)
    val l1 = List(2,3)
    val l2 = List(4)
    val l3 = List()
    val l4 = List(1,2,3,4)
    val l5 = List(1,3)
    println(hasSubsequence(l, l1))
    println(hasSubsequence(l, l2))
    println(hasSubsequence(l, l3))
    println(hasSubsequence(l, l4))
    println(!hasSubsequence(l, l5))
  }
}
