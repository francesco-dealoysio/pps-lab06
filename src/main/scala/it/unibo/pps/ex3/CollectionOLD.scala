package it.unibo.pps.ex3

import java.util.concurrent.TimeUnit
import scala.collection.mutable.HashSet
import scala.concurrent.duration.FiniteDuration
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object _PerformanceUtils:
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

@main def _checkPerformance: Unit =
  import PerformanceUtils.*
  import scala.collection.mutable.HashMap

  val DEBUG = true
  val size = 10 //100000
  val key = size / 2

  /* Linear sequences: List, ListBuffer */
  /* List (immutable) */
  val list = (1 to size).toList
  //val list = (1, 2, 3, 4, 3, 3, 5, 6, 7, 8, 9).toList

  if DEBUG && false then
    val index = size / 2
    println("\nList to be read: " + list)
    println("Value read " + list(index) + " at index " + index + "\n")
  end if

  measure("List read element") {
    val index = size / 2
    list(index)
    //list(list.indexOf(key))
  }

  if DEBUG && false then
    val list = (1 to size).toList
    val index = size / 2
    val newKeyValue = 111
    println("\nBefore update: " + list)
    println("Update " + list(size / 2) + " -> " + newKeyValue + " at index " + index)
    val listAfter = list.updated(index, newKeyValue)
    println("After update: " + listAfter + "\n")
  end if

  measure("List update element") {
    val index = size / 2
    val newKeyValue = 111
    list.updated(index, newKeyValue)
  }

  if DEBUG && false then
    val list = (1 to size).toList
    val index = 2
    val keyToDelete = list(index)
    println("\nBefore delete: " + list)
    println("Delete " + keyToDelete + " at index " + index)
    val listAfter = list.take(index) ++ list.drop(index + 1)
    // val listAfter = list.slice(0, index) ::: list.slice(index + 1, list.size)
    println("After delete: " + listAfter + "\n")
  end if

  measure("List remove element") {
    val index = size / 2
    val listAfter = list.take(index) ++ list.drop(index + 1)
    //list.filter(_ != key)
  }

  /* ListBuffer (mutable) */
  var listBuffer = ListBuffer[Int]()
  listBuffer ++= (1 to size).toList

  if DEBUG && false then
    val index = size / 2
    println("\nListBuffer to be read: " + listBuffer)
    println("Value read " + listBuffer(index) + " at index " + index + "\n")
  end if

  measure("ListBuffer read element") {
    val index = size / 2
    listBuffer(index)
  }

  if DEBUG && false then
    var listBuffer = ListBuffer[Int]()
    listBuffer ++= (1 to size).toList
    val index = size / 2
    val newKeyValue = 111
    println("\nBefore update: " + listBuffer)
    println("Update " + listBuffer(size / 2) + " -> " + newKeyValue + " at index " + index)
    listBuffer(index) = newKeyValue
    println("After update: " + listBuffer + "\n")
  end if

  measure("ListBuffer update element") {
    val index = size / 2
    val newKeyValue = 111
    listBuffer(index) = newKeyValue
  }

  if DEBUG && false then
    var listBuffer = ListBuffer[Int]()
    listBuffer ++= (1 to size).toList
    val index = size / 2
    val keyToDelete = listBuffer(index)
    println("\nBefore remove: " + listBuffer)
    println("Delete " + keyToDelete + " at index " + index)
    listBuffer.remove(index)
    println("After remove: " + listBuffer + "\n")
  end if

  measure("ListBuffer remove element") {
    val index = size / 2
    listBuffer.remove(index)
  }

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  /* Vector (immutable) */
  val vector = (1 to size).toVector

  if DEBUG && false then
    val index = size / 2
    println("\nVector to be read: " + vector)
    println("Value read " + vector(index) + " at index " + index + "\n")
  end if

  measure("Vector read element") {
    val index = size / 2
    vector(index)
  }

  if DEBUG && false then
    val vector = (1 to size).toVector
    val index = size / 2
    val newKeyValue = 111
    println("\nBefore update: " + vector)
    println("Update " + vector(size / 2) + " -> " + newKeyValue + " at index " + index)
    val vectorAfter = vector.updated(index, newKeyValue)
    println("After update: " + vectorAfter + "\n")
  end if

  measure("Vector update element") {
    val index = size / 2
    val newKeyValue = 111
    vector.updated(index, newKeyValue)
  }

  if DEBUG && false then
    val vector = (1 to size).toVector
    val index = 2
    val keyToDelete = vector(index)
    println("\nBefore delete: " + vector)
    println("Delete " + keyToDelete + " at index " + index)
    val vectorAfter = vector.patch(index, Nil, 1)
    println("After delete: " + vectorAfter + "\n")
  end if

  measure("Vector remove element") {
    val index = size / 2
    val vectorAfter = vector.patch(index, Nil, 1)
  }

  /* Array (mutable) */
  var array = (1 to size).toArray

  if DEBUG && false then
    val index = size / 2
    println("\nArray to be read: " + array.toList)
    println("Value read " + array(index) + " at index " + index + "\n")
  end if

  measure("Array read element") {
    val index = size / 2
    array(index)
  }

  if DEBUG && false then
    var array = (1 to size).toArray
    val index = size / 2
    val newKeyValue = 111
    println("\nBefore update: " + listBuffer)
    println("Update " + array(index) + " -> " + newKeyValue + " at index " + index)
    array(index) = newKeyValue
    println("After update: " + array.toList + "\n")
  end if

  measure("Array update element") {
    val index = size / 2
    val newKeyValue = 111
    array(index) = newKeyValue
  }

  if DEBUG && false then
    var array = (1 to size).toArray
    val index = size / 2
    val keyToDelete = array(index)
    println("\nBefore remove: " + array.toList)
    println("Delete " + keyToDelete + " at index " + index)
    val arrayAfter = array.patch(index, Nil, 1)
    println("After remove: " + arrayAfter.toList + "\n")
  end if

  measure("Array remove element") {
    val index = size / 2
    array.patch(index, Nil, 1)
  }

  /* ArrayBuffer (mutable) */
  val arrayBuffer = ArrayBuffer[Int]()
  arrayBuffer ++= (1 to size).toList

  if DEBUG && false then
    val index = size / 2
    println("\nArrayBuffer to be read: " + arrayBuffer)
    println("Value read " + arrayBuffer(index) + " at index " + index + "\n")
  end if

  measure("ArrayBuffer read element") {
    val index = size / 2
    arrayBuffer(index)
  }

  if DEBUG && false then
    var arrayBuffer = ArrayBuffer[Int]()
    arrayBuffer ++= (1 to size).toList
    val index = size / 2
    val newKeyValue = 111
    println("\nBefore update: " + arrayBuffer)
    println("Update " + arrayBuffer(size / 2) + " -> " + newKeyValue + " at index " + index)
    arrayBuffer(index) = newKeyValue
    println("After update: " + arrayBuffer + "\n")
  end if

  measure("ArrayBuffer update element") {
    val index = size / 2
    val newKeyValue = 111
    arrayBuffer(index) = newKeyValue
  }

  if DEBUG && false then
    var arrayBuffer = ListBuffer[Int]()
    arrayBuffer ++= (1 to size).toList
    val index = size / 2
    val keyToDelete = arrayBuffer(index)
    println("\nBefore remove: " + arrayBuffer)
    println("Delete " + keyToDelete + " at index " + index)
    arrayBuffer.remove(index)
    println("After remove: " + arrayBuffer + "\n")
  end if

  measure("ListBuffer remove element") {
    val index = size / 2
    arrayBuffer.remove(index)
  }

  /* Sets */
  val immSet = (1 to size).toSet
  val mutSet = HashSet.from(1 to size)

  measure("Immutable Set contains") {
    immSet.contains(key)
  }

  measure("Mutable HashSet contains") {
    mutSet.contains(key)
  }

  measure("Immutable Set add") {
    immSet + (key)
  }

  measure("Mutable HashSet add") {
    val copy = mutSet.clone()
    copy += (key)
    copy
  }

  measure("Immutable Set remove") {
    immSet - (key)
  }

  measure("Mutable HashSet remove") {
    val copy = mutSet.clone()
    copy -= (key)
    copy
  }

  /* Maps */
  val immMap = (1 to size).map(i => i -> (i * 10)).toMap
  val mutMap = HashMap.from((1 to size).map(i => i -> (i * 10)))

  measure("Immutable Map lookup") {
    immMap.get(key)
  }

  measure("Mutable HashMap lookup") {
    mutMap.get(key)
  }

  measure("Immutable Map update") {
    immMap.updated(key, -1)
  }

  measure("Mutable HashMap update") {
    val copy = mutMap.clone()
    copy.update(key, -1)
    copy
  }

  measure("Immutable Map add") {
    immMap + (size + 1 -> 999)
  }

  measure("Mutable HashMap add") {
    val copy = mutMap.clone()
    copy += ((size + 1) -> 999)
    copy
  }

  measure("Immutable Map remove") {
    immMap - key
  }

  measure("Mutable HashMap remove") {
    val copy = mutMap.clone()
    copy -= key
    copy
  }

  /* Comparison */
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector

  assert(measure("list last")(lst.last) > measure("vec last")(vec.last))
