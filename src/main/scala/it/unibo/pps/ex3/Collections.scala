package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.HashSet
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object PerformanceUtils:
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]]:
    override def compare(that: MeasurementResults[_]): Int =
      duration.toNanos.compareTo(that.duration.toNanos)

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] =
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime() - startTime, TimeUnit.NANOSECONDS)
    if msg.nonEmpty then
      var message = (msg + " " + "-".repeat(30)).take(30) + ": "
      println(message + duration.toNanos + " nanos; " + duration.toMillis + "ms")
      //println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)

@main def checkPerformance: Unit =
  import PerformanceUtils.*
  import scala.collection.mutable.HashMap

  val DEBUG = true
  val size = 100000
  val key = size / 2
  val index = size / 2
  val newKeyValue = 111

  /* Linear sequences: List, ListBuffer */
  val list = (1 to size).toList       // List (immutable)
  val listBuffer = ListBuffer[Int]()  // ListBuffer (mutable)
  listBuffer ++= (1 to size).toList

  measure("List read (immutable)") {
    list(index)
  }

  measure("ListBuffer read (mutable)") {
    listBuffer(index)
  }

  measure("List update (immutable)") {
    list.updated(index, newKeyValue)
  }

  measure("ListBuffer update (mutable)") {
    listBuffer(index) = newKeyValue
  }

  measure("List remove (immutable)") {
    val listAfter = list.take(index) ++ list.drop(index + 1)
  }

  measure("ListBuffer remove (mutable)") {
    listBuffer.remove(index)
  }

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  val vector = (1 to size).toVector     // Vector (immutable)
  var array = (1 to size).toArray       // Array (mutable)
  val arrayBuffer = ArrayBuffer[Int]()  // ArrayBuffer (mutable)
  arrayBuffer ++= (1 to size).toList

  measure("Vector read (immutable)") {
    vector(index)
  }

  measure("Array read (mutable)") {
    array(index)
  }

  measure("ArrayBuffer read (mutable)") {
    arrayBuffer(index)
  }

  measure("Vector update (immutable)") {
    vector.updated(index, newKeyValue)
  }

  measure("Array update (mutable)") {
    array(index) = newKeyValue
  }

  measure("ArrayBuffer update (mutable)") {
    arrayBuffer(index) = newKeyValue
  }

  measure("Vector remove (immutable)") {
    val vectorAfter = vector.patch(index, Nil, 1)
  }

  measure("Array remove (mutable)") {
    array.patch(index, Nil, 1)
  }

  measure("ArrayBuffer remove (mutable)") {
    arrayBuffer.remove(index)
  }

  /* Sets */
  val immSet = (1 to size).toSet
  val mutSet = HashSet.from(1 to size)

  measure("Set contains (immutable)") {
    immSet.contains(key)
  }

  measure("HashSet contains (immutable)") {
    mutSet.contains(key)
  }

  measure("Set add (immutable)") {
    immSet + (key)
  }

  measure("HashSet add (mutable") {
    val copy = mutSet.clone()
    copy += (key)
    copy
  }

  measure("Set remove (immutable") {
    immSet - (key)
  }

  measure("HashSet remove (mutable)") {
    val copy = mutSet.clone()
    copy -= (key)
    copy
  }

  /* Maps */
  val immMap = (1 to size).map(i => i -> (i * 10)).toMap
  val mutMap = HashMap.from((1 to size).map(i => i -> (i * 10)))

  measure("Map lookup (immutable)") {
    immMap.get(key)
  }

  measure("HashMap lookup (mutable)") {
    mutMap.get(key)
  }

  measure("Map update (immutable") {
    immMap.updated(key, -1)
  }

  measure("HashMap update (mutable)") {
    val copy = mutMap.clone()
    copy.update(key, -1)
    copy
  }

  measure("Map add (immutable)") {
    immMap + (size + 1 -> 999)
  }

  measure("HashMap add (mutable)") {
    val copy = mutMap.clone()
    copy += ((size + 1) -> 999)
    copy
  }

  measure("Map remove (immutable") {
    immMap - key
  }

  measure("HashMap remove (mutable)") {
    val copy = mutMap.clone()
    copy -= key
    copy
  }

  /* Comparison */
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector

  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))
