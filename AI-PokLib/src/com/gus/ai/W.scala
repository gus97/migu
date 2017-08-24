package com.gus.ai

import com.gus.ai.CommbRule._
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.util.Random
import util.control.Breaks._
import java.io.PrintWriter
import java.io.File
import java.util.HashMap

object W {

  def main(args: Array[String]): Unit = {

    println(win)
  }
  /**
   * 必胜牌策略
   * 1、三家随机发牌15张
   * 2、找出当前15张牌力最强的一家 M
   * 3、在剩余的9张重取出C92的组合，依次补到牌力最强家M的手牌中
   * 4、在C92中找出炸弹-非冲的最大值的那副牌，最为最强牌保留
   * 5、模拟实际打牌，该最强牌P在三种不同角色中均获胜，M升级必胜牌W
   */
  def win(): Buffer[Buffer[Int]] = {

    val ai = new AI
    val list = ArrayBuffer[Int]()
    for (x <- 3 to 14) {
      list += (x, x, x, x)
    }

    list += (15, 15, 15, 15)

    list += 99
    list += 100

    val jpq = ArrayBuffer(15)

    val s1 = util.Random.shuffle(list).clone

    val p1 = s1.take(15).clone
    s1 --= p1

    val p2 = s1.take(15).clone
    s1 --= p2

    val p3 = s1.take(15).clone
    s1 --= p3

    //println(p1.sortWith(_ < _))
    //println(p2.sortWith(_ < _))
    //println(p3.sortWith(_ < _))
    //println(s1)

    //println()

    //剩余C(9,2)的组合
    val b = s1.combinations(2)

    val k1 = getKZS(p1) + 0.01
    val k2 = getKZS(p2) + 0.02
    val k3 = getKZS(p3) + 0.03

    var hp = ArrayBuffer[Int]()

    var ka = TreeMap(k1 -> p1, k2 -> p2, k3 -> p3)

    val bd = ka.toList.sortWith((x, y) => x._1 > y._1).head._2

    val b1 = ka.toList.sortWith((x, y) => x._1 > y._1)(1)._2

    val b2 = ka.toList.sortWith((x, y) => x._1 > y._1)(2)._2

    //println("*******************************************************************\n", bd.sortWith(_ < _), "\n*******************************************************************")
    //println(b1.sortWith(_ < _))
    //println(b2.sortWith(_ < _))

    var tmp = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()

    //最终选取的两张
    var x2 = ArrayBuffer[Int]()

    for (x <- b) {
      val p0 = (bd.clone ++= x)
      val op = commb(p0, jpq)
      if (tmp.size == 0) {
        x2 = x
        tmp = op
      } else {
        if (tmp.flatMap(_._2).size - 2 * (tmp.get(1).getOrElse(ArrayBuffer()).size + tmp.get(4).getOrElse(ArrayBuffer()).size)
          >= op.flatMap(_._2).size - 2 * (op.get(1).getOrElse(ArrayBuffer()).size + op.get(4).getOrElse(ArrayBuffer()).size)) {
          tmp = op
          x2 = x
        }
      }
    }

    //println("\n*******************************************************************")
    //tmp.flatMap(_._2).foreach(println)
    //println("\n选中的2张======>", x2)

    //=============================调整大逻辑！！！！！！！！！！！！！！！！！！！！！！！！！！！！！！================================================
    //找出3张
    val l3 = tmp.get(3).getOrElse(ArrayBuffer()).length
    val l2 = tmp.get(2).getOrElse(ArrayBuffer()).length
    val l1 = tmp.get(11).getOrElse(ArrayBuffer()).length
    val l12345 = tmp.get(12345).getOrElse(ArrayBuffer()).length

    //最原始的17张
    val myP = tmp.flatMap(_._2).flatMap(x => x).toBuffer.clone

    var single = ArrayBuffer[Int]()
    var double = ArrayBuffer[Int]()

    //拿出小于A的单牌
    if (l1 > 0) {
      single = tmp.get(11).get.filter(x => x.filter(y => y < 14).size > 0).flatMap(x => x).sortWith(_ < _)
    }

    //消除组牌中的单牌

    if (l3 > 0 && single.length > 0) {
      if (b1.contains(tmp.get(3).getOrElse(ArrayBuffer()).head.head)) {
        //组个炸
        myP += tmp.get(3).getOrElse(ArrayBuffer()).head.head
        //扔一个最小单
        val minS = single.remove(0)
        myP -= minS
        //换一个最小的单
        b1 -= tmp.get(3).getOrElse(ArrayBuffer()).head.head
        b1 += minS

      } else if (b2.contains(tmp.get(3).getOrElse(ArrayBuffer()).head.head)) {

        //组个炸
        myP += tmp.get(3).getOrElse(ArrayBuffer()).head.head
        //扔一个最小单
        val minS = single.remove(0)
        myP -= minS
        //换一个最小的单
        b2 -= tmp.get(3).getOrElse(ArrayBuffer()).head.head
        b2 += minS
      }
    }

    if (l2 > 0 && single.length > 0) {

      if (b1.contains(tmp.get(2).getOrElse(ArrayBuffer()).head.head)) {
        //组个炸
        myP += tmp.get(2).getOrElse(ArrayBuffer()).head.head
        //扔一个最小单
        val minS = single.remove(0)
        myP -= minS
        //换一个最小的单
        b1 -= tmp.get(2).getOrElse(ArrayBuffer()).head.head
        b1 += minS

      } else if (b2.contains(tmp.get(2).getOrElse(ArrayBuffer()).head.head)) {

        //组个炸
        myP += tmp.get(2).getOrElse(ArrayBuffer()).head.head
        //扔一个最小单
        val minS = single.remove(0)
        myP -= minS
        //换一个最小的单
        b2 -= tmp.get(2).getOrElse(ArrayBuffer()).head.head
        b2 += minS
      }
    }

    //追加对顺子的整合

    //如果另外两家牌有大小王，则把大小王和最后3张牌交换

    val myP1 = ArrayBuffer[Int]()

    for (x <- myP) {
      myP1 += x
    }

    val f = commb(myP1, jpq)

    s1 --= x2

    //println("当前7张===>", s1.sortWith(_ < _))

    //println("\n最后选定的牌：========>")
    //f.flatMap(_._2).foreach(println)

    //println("\n剩余两套牌：========>")

    val x3 = util.Random.shuffle(s1)

    //val x3 = s1.sortWith(_ < _)

    // println(b1 ++= x3.take(2).clone)
    b1 ++= x3.take(2).clone
    x3.remove(0, 2)
    //println(b2 ++= x3.take(2).clone())
    b2 ++= x3.take(2).clone()
    x3.remove(0, 2)
    val three = x3.take(3)
    //println("\n最后三张：========>")
    // println(three)

    val su = f.flatMap(_._2).flatMap(x => x).toBuffer ++ b1 ++ b2 ++ three

    //核对一下最后三家牌是不是完整的54张
    //    println("\n验证：========>")
    //    val fn = su.map((_, 1))
    //      .groupBy(_._1)
    //      .map { x => (x._1, x._2.size) }
    //
    //    fn.toList.sorted foreach {
    //      case (key, value) =>
    //        println(key + " = " + value)
    //    }

    val ab = Buffer[Buffer[Int]]()

    val cd = Buffer[Buffer[Int]]()
    ab += f.flatMap(_._2).flatMap(x => x).toBuffer
    ab += b1
    ab += b2
    ab += three

    val fs = ab
    if (fs != null) {
      var c0 = Buffer[Int]()
      var c1 = Buffer[Int]()
      var c2 = Buffer[Int]()
      var c3 = Buffer[Int]()

      val colourBox = new HashMap[Int, ArrayBuffer[Int]]()
      for (x <- 3 to 15) {
        colourBox.put(x, ArrayBuffer(1, 2, 3, 4))
      }

      for (x <- fs(0)) {
        if (colourBox.containsKey(x)) {
          val c = Random.shuffle(colourBox.get(x)).remove(0)
          colourBox.get(x) -= c
          c1 += x * 10 + c
        } else {
          c1 += x
        }
      }
      for (x <- fs(1)) {
        if (colourBox.containsKey(x)) {
          val c = Random.shuffle(colourBox.get(x)).remove(0)
          colourBox.get(x) -= c
          c2 += x * 10 + c
        } else {
          c2 += x
        }
      }
      for (x <- fs(2)) {
        if (colourBox.containsKey(x)) {
          val c = Random.shuffle(colourBox.get(x)).remove(0)
          colourBox.get(x) -= c
          c3 += x * 10 + c
        } else {
          c3 += x
        }
      }
      for (x <- fs(3)) {
        if (colourBox.containsKey(x)) {
          val c = Random.shuffle(colourBox.get(x)).remove(0)
          colourBox.get(x) -= c
          c0 += x * 10 + c
        } else {
          c0 += x
        }
      }

      cd += c1
      cd += c2
      cd += c3
      cd += c0

    }
    cd
  }
}