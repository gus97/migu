package com.gus.ai

import scala.collection.mutable.Buffer
import scala.util.Random
import com.gus.ai.CommbRule._

import scala.collection.mutable.ArrayBuffer
import java.io.PrintWriter
import java.io.File
import java.util.HashMap

object B {

  def main(args: Array[String]): Unit = {

    val writer = new PrintWriter(new File("/home/g1/gus/b1.txt"))

    var i = 1
    for (x <- 1 to 10) {
      val f = b
      if (f != null) {
        var c0 = Buffer[Int]()
        var c1 = Buffer[Int]()
        var c2 = Buffer[Int]()
        var c3 = Buffer[Int]()

        val colourBox = new HashMap[Int, ArrayBuffer[Int]]()
        for (x <- 3 to 15) {
          colourBox.put(x, ArrayBuffer(1, 2, 3, 4))
        }

        for (x <- f(0)) {
          if (colourBox.containsKey(x)) {
            val c = Random.shuffle(colourBox.get(x)).remove(0)
            colourBox.get(x) -= c
            c1 += x * 10 + c
          } else {
            c1 += x
          }
        }
        for (x <- f(1)) {
          if (colourBox.containsKey(x)) {
            val c = Random.shuffle(colourBox.get(x)).remove(0)
            colourBox.get(x) -= c
            c2 += x * 10 + c
          } else {
            c2 += x
          }
        }
        for (x <- f(2)) {
          if (colourBox.containsKey(x)) {
            val c = Random.shuffle(colourBox.get(x)).remove(0)
            colourBox.get(x) -= c
            c3 += x * 10 + c
          } else {
            c3 += x
          }
        }
        for (x <- f(3)) {
          if (colourBox.containsKey(x)) {
            val c = Random.shuffle(colourBox.get(x)).remove(0)
            colourBox.get(x) -= c
            c0 += x * 10 + c
          } else {
            c0 += x
          }
        }

        writer.println((c1.sortWith(_ > _) + "$" + c2.sortWith(_ > _) + "$" + c3.sortWith(_ > _) + "$" + c0.sortWith(_ > _)).replace("ArrayBuffer(", "").replace(")", "").replace(" ", ""))

      }
      i += 1
      if (i % 1000 == 0) {
        println(i)
      }
    }

    writer.close()
  }

  def b(): Buffer[Buffer[Int]] = {

    val ai = new AI
    val list = Buffer[Int]()
    for (x <- 3 to 14) {
      list += (x, x, x, x)
    }

    val two = Buffer(15, 15, 15, 15)

    val dx = Buffer(99, 100)

    val b1 = Buffer[Int]()
    val b2 = Buffer[Int]()
    val b3 = Buffer[Int]()

    var r = Random.nextInt(8) + 3

    var s = Random.nextInt(11 - r) + 5

    val seq = list.distinct.sortWith(_ < _)

    for (x <- r to r + s - 1) {

      b1 += x

    }

    list --= b1

    r = Random.nextInt(8) + 3
    s = Random.nextInt(11 - r) + 5

    for (x <- r to r + s - 1) {

      b2 += x

    }

    list --= b2

    r = Random.nextInt(8) + 3
    s = Random.nextInt(11 - r) + 5

    for (x <- r to r + s - 1) {

      b3 += x

    }

    list --= b3

    //发三张
    val three = list.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).keySet.toBuffer

    val sh3 = Random.shuffle(three)

    if (sh3.length > 0) {

      val t1 = sh3.remove(0)
      b1 += (t1, t1, t1)
      list -= (t1, t1, t1)
    }

    if (sh3.length > 0) {
      val t2 = sh3.remove(0)
      b2 += (t2, t2, t2)
      list -= (t2, t2, t2)
    }

    if (sh3.length > 0) {
      val t3 = sh3.remove(0)
      b3 += (t3, t3, t3)
      list -= (t3, t3, t3)
    }

    //发对子

    val db = list.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).keySet.toBuffer

    val sh2 = Random.shuffle(db)

    if (sh2.length > 0) {

      val t1 = sh2.remove(0)
      b1 += (t1, t1)
      list -= (t1, t1)
    }

    if (sh2.length > 0) {
      val t2 = sh2.remove(0)
      b2 += (t2, t2)
      list -= (t2, t2)
    }

    if (sh2.length > 0) {
      val t3 = sh2.remove(0)
      b3 += (t3, t3)
      list -= (t3, t3)
    }

    val shList = Random.shuffle(list ++= (two ++ dx))

    //补全
    for (x <- 1 to 17 - b1.size) {
      b1 += shList.remove(0)
    }
    for (x <- 1 to 17 - b2.size) {
      b2 += shList.remove(0)
    }
    for (x <- 1 to 17 - b3.size) {
      b3 += shList.remove(0)
    }

    if (commb(b1.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).size <= 6
      && commb(b2.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).size <= 6
      && commb(b3.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).size <= 6) {
      //      println("============================================================================")
      //      commb(b1.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).foreach(println)
      //      println("============================================================================")
      //      commb(b2.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).foreach(println)
      //      println("============================================================================")
      //      commb(b3.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(15)).flatMap(_._2).foreach(println)
      //      println("============================================================================")
      //
      //      println(shList)
      //      println(b1.length + b2.length + b3.length + shList.length)
      //      println("\n")

      Buffer(b1, b2, b3, shList)
    } else {
      null
    }

  }
}