package com.gus.ai

import java.io.PrintWriter
import scala.collection.mutable.Buffer
import scala.util.Random
import java.io.File

object R {
  
  def main(args: Array[String]): Unit = {

    val writer = new PrintWriter(new File("/home/g1/gus/r.txt"))
    val r = Buffer[Int]()

    for (x <- 3 to 15) {
      for (y <- 1 to 4) {
        r += x * 10 + y
      }
    }

    r += (99, 100)

    var cnt = 0
    var i = 1
    for (x <- 1 to 200000) {
      val tmp = r.clone()
      val n1 = Random.shuffle(tmp).take(17).toBuffer
      tmp --= n1
      val n2 = Random.shuffle(tmp).take(17).toBuffer
      tmp --= n2
      val n3 = Random.shuffle(tmp).take(17).toBuffer
      tmp --= n3

      val n1s = n1.map(x => if (x != 100 && x != 99) { (x / 10, 1) } else { (x, 1) }).groupBy(x => x._1).map(x => (x._1, x._2.size)).filter(x => x._2 == 4).size

      val n2s = n2.map(x => if (x != 100 && x != 99) { (x / 10, 1) } else { (x, 1) }).groupBy(x => x._1).map(x => (x._1, x._2.size)).filter(x => x._2 == 4).size

      val n3s = n3.map(x => if (x != 100 && x != 99) { (x / 10, 1) } else { (x, 1) }).groupBy(x => x._1).map(x => (x._1, x._2.size)).filter(x => x._2 == 4).size

      val kn1 = n1.sortWith(_ < _).containsSlice(Buffer(99, 100))
      val kn2 = n2.sortWith(_ < _).containsSlice(Buffer(99, 100))
      val kn3 = n3.sortWith(_ < _).containsSlice(Buffer(99, 100))
       
      if ((n1s + n2s + n3s == 0) && kn1 == false && kn2 == false && kn3 == false) {

        writer.print((n1.sortWith(_ > _) + "$" + n2.sortWith(_ > _) + "$" + n3.sortWith(_ > _) + "$" + tmp+"\n").replace("ArrayBuffer(", "").replace(")", "").replace(" ", ""))
       
        cnt += (n1++n2++n3++tmp).toSet.size
      }

      i += 1

      if (i % 1000 == 0) {
        println(i)
      }
    }
   
    writer.close
  }
}