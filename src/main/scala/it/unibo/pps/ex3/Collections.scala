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

  /* Linear sequences: List, ListBuffer */
  /* List (immutable) */
  val list = (1 to size).toList

  measure("List read (immutable)") {
    val index = size / 2
    list(index)
  }

  measure("List update (immutable)") {
    val index = size / 2
    val newKeyValue = 111
    list.updated(index, newKeyValue)
  }

  measure("List remove (immutable)") {
    val index = size / 2
    val listAfter = list.take(index) ++ list.drop(index + 1)
  }

  /* ListBuffer (mutable) */
  var listBuffer = ListBuffer[Int]()
  listBuffer ++= (1 to size).toList

  measure("ListBuffer read (mutable)") {
    val index = size / 2
    listBuffer(index)
  }

  measure("ListBuffer update (mutable)") {
    val index = size / 2
    val newKeyValue = 111
    listBuffer(index) = newKeyValue
  }

  measure("ListBuffer remove (mutable)") {
    val index = size / 2
    listBuffer.remove(index)
  }

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  /* Vector (immutable) */
  val vector = (1 to size).toVector

  measure("Vector read (immutable)") {
    val index = size / 2
    vector(index)
  }

  measure("Vector update (immutable)") {
    val index = size / 2
    val newKeyValue = 111
    vector.updated(index, newKeyValue)
  }

  measure("Vector remove (immutable)") {
    val index = size / 2
    val vectorAfter = vector.patch(index, Nil, 1)
  }

  /* Array (mutable) */
  var array = (1 to size).toArray

  measure("Array read (mutable)") {
    val index = size / 2
    array(index)
  }

  measure("Array update (mutable)") {
    val index = size / 2
    val newKeyValue = 111
    array(index) = newKeyValue
  }

  measure("Array remove (mutable)") {
    val index = size / 2
    array.patch(index, Nil, 1)
  }

  /* ArrayBuffer (mutable) */
  val arrayBuffer = ArrayBuffer[Int]()
  arrayBuffer ++= (1 to size).toList

  measure("ArrayBuffer read (mutable)") {
    val index = size / 2
    arrayBuffer(index)
  }

  measure("ArrayBuffer update (mutable)") {
    val index = size / 2
    val newKeyValue = 111
    arrayBuffer(index) = newKeyValue
  }

  measure("ArrayBuffer remove (mutable)") {
    val index = size / 2
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
