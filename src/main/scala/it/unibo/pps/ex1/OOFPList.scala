package it.unibo.pps.ex1

import java.awt.print.Printable
import scala.annotation.tailrec

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def foldLeft[B](init: B)(op: (B, A) => B): B = this match
    case h :: t => t.foldLeft(op(init, h))(op)
    case _ => init

  def foldRight[B](init: B)(op: (A, B) => B): B = this match
    case h :: t => op(h, t.foldRight(init)(op))
    case _ => init

  def append(list: List[A]): List[A] =
    foldRight(list)(_ :: _)

  def flatMap[B](f: A => List[B]): List[B] =
    foldRight(Nil())(f(_) append _)

  def filter(predicate: A => Boolean): List[A] = flatMap(a => if predicate(a) then a :: Nil() else Nil())

  def map[B](fun: A => B): List[B] = flatMap(a => fun(a) :: Nil())

  def reduce(op: (A, A) => A): A = this match
    case Nil() => throw new IllegalStateException()
    case h :: t => t.foldLeft(h)(op)

  // Exercise: implement the following methods
  def reverse: List[A] = foldLeft(Nil[A]())((acc, h) => h :: acc)

  def zipWithValue[B](value: B): List[(A, B)] =
    map(a => (a, value))

  def length(): Int =
    foldLeft(0)((acc, _) => acc + 1)

  def indices(): List[Int] =
    foldLeft((length() - 1, Nil[Int]()))((init, _) => (init._1 - 1, init._1 :: init._2))._2

  def zipWithIndex: List[(A, Int)] =
    foldRight((length() - 1, Nil[(A, Int)]()))((h, init) => (init._1 - 1, (h, init._1) :: init._2))._2

  def partition(predicate: A => Boolean): (List[A], List[A]) =
    (filter(predicate), filter(a => !predicate(a)))

  def span(predicate: A => Boolean): (List[A], List[A]) = this match
    case h :: t => if predicate(h) then {var (list1, list2) = t.span(predicate); (h :: list1, list2)} else (Nil(), this)
    case _      => (Nil(), Nil())

  def takeRight(n: Int): List[A] =
    var list: List[A] = Nil(); var c = n; for e <- this.reverse do {if c > 0 then list = e :: list else Nil(); c = c - 1}; list

  def collect(predicate: PartialFunction[A, A]): List[A] =
    filter(a => predicate.isDefinedAt(a)).map(a => predicate(a))

// Factories
object List:

  def unzip[A, B](list: List[(A, B)]): (List[A], List[B]) = list match
    case (left, right) :: rest =>
      val (leftList, rightList) = unzip(rest)
      (left :: leftList, right :: rightList)
    case Nil() => (Nil(), Nil())

  def unzipWithFold[A, B](list: List[(A, B)]): (List[A], List[B]) =
    list.foldRight((Nil(), Nil())) {
      case ((left, right), (leftList, rightList)) => (left :: leftList, right :: rightList)
    }
  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  val reference = List(1, 2, 3, 4)

  println("zipWithValue: " + reference.zipWithValue(10)) // List((1, 10), (2, 10), (3, 10), (4, 10))
  println("length......: " + reference.length()) // 4
  println("indices.....: " + reference.indices()) // List(0, 1, 2, 3)
  println("zipWithIndex: " + reference.zipWithIndex) // List((1, 0), (2, 1), (3, 2), (4, 3))
  println("partition...: " + reference.partition(_ % 2 == 0)) // (List(2, 4), List(1, 3))
  println("span........: " + reference.span(_ % 2 != 0)) // (List(1), List(2, 3, 4))
  println("span........: " + reference.span(_ < 3)) // (List(1, 2), List(3, 4))
  println("takeRight...: " + reference.takeRight(3)) // List(2, 3, 4)
  println("collect.....: " + reference.collect { case x if x % 2 == 0 => x + 1 }) // List(3, 5)