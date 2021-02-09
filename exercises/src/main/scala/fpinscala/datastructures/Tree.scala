package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A], z: A => B)(f: (B, B) => B): B = t match {
    case Leaf(v) => z(v)
    case Branch(l, r) => f(fold(l, z)(f), fold(r, z)(f))
  }

  def size2[A](t: Tree[A]): Int = fold(t, _ => 1)(1 + _ + _)

  def maximum2(t: Tree[Int]): Int = fold(t, (x: Int) => x)(_ max _)

  def depth2[A](t: Tree[A]): Int = fold(t, _ => 1)((x, y) => 1 + (x max y))

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t, (a: A) => Leaf(f(a)): Tree[B])(Branch(_, _))
}