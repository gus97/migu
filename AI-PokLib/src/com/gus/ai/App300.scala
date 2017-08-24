package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import com.gus.ai.CommbRule._

import java.util.ArrayDeque
import java.util.concurrent.CyclicBarrier
import java.io.PrintWriter
import java.io.File
import java.util.HashMap
import scala.collection.mutable.Buffer

/**
 * 自动打牌
 */
object App300 {

  var errorPock = 0
  var exceptions = 0

  def check(ab: ArrayBuffer[Int], ai: AI): Boolean = {

    if (ab.length == 0) { return true }

    if (ai.r == ai.h) { return true }

    //去花色
    val nab = ab.map { x => if (x == 99 || x == 100) { x / 1 } else { x / 10 } }

    val px = judgeTp(nab)
    val tpx = judgeTp(ai.t)
    val pFirst = nab.head
    val tFirst = ai.t.head

    px match {
      //打出是王炸，秒一切
      case 1 => return true
      //打出的是炸弹，只要桌面牌不是王炸或者（桌面牌是炸弹，但打出炸弹比桌面大）的一切都ok
      case 4 => if (tpx == 1) { return false } else if (tpx == 4 && pFirst < tFirst) { return false } else { return true }
      //其他牌型
      case _ =>
    }

    //再看同牌型之间的比较
    if (px == tpx && pFirst > tFirst && ai.t.size == ab.size) {
      return true
    } else {
      return false
    }
  }

  def main(args: Array[String]): Unit = {

    val writer = new PrintWriter(new File("/home/g1/gus/w2-2.txt"))

    var d1Num = 0
    var n1Num = 0
    var n2Num = 0

    var r1 = 0
    var r11 = 0
    var r12 = 0
    var r1w = 0
    var r11w = 0
    var r12w = 0

    var r2 = 0
    var r21 = 0
    var r22 = 0
    var r2w = 0
    var r21w = 0
    var r22w = 0

    var r3 = 0
    var r31 = 0
    var r32 = 0
    var r3w = 0
    var r31w = 0
    var r32w = 0

    var i = 0
    //    val barrier = new CyclicBarrier(1);
    //    val t1 = System.currentTimeMillis()

    for (x <- 1 to 10) {

      var c0 = Buffer[Int]()
      var c1 = Buffer[Int]()
      var c2 = Buffer[Int]()
      var c3 = Buffer[Int]()

      // new Thread(new Runnable() {
      //def run() {
      //barrier.await()

      val f = W.win()

      val colourBox = new HashMap[Int, ArrayBuffer[Int]]()
      for (x <- 3 to 15) {
        colourBox.put(x, ArrayBuffer(1, 2, 3, 4))
      }

      for (x <- f(0)) {
        if (colourBox.containsKey(x)) {
          c1 += x * 10 + colourBox.get(x).remove(0)
        } else {
          c1 += x
        }
      }
      for (x <- f(1)) {
        if (colourBox.containsKey(x)) {
          c2 += x * 10 + colourBox.get(x).remove(0)
        } else {
          c2 += x
        }
      }
      for (x <- f(2)) {
        if (colourBox.containsKey(x)) {
          c3 += x * 10 + colourBox.get(x).remove(0)
        } else {
          c3 += x
        }
      }
      for (x <- f(3)) {
        if (colourBox.containsKey(x)) {
          c0 += x * 10 + colourBox.get(x).remove(0)
        } else {
          c0 += x
        }
      }

      for (k <- 1 to 2) {

        val cal = new AFun
        //地主
        val ai0 = new AI
        val d1 = new AFun

        //上家农民
        val ai1 = new AI
        val n1 = new NFun

        //下家农民
        val ai2 = new AI
        val n2 = new NFun

        for (x <- 3 to 15) {
          ai0.jpq += (x, x, x, x)
          ai1.jpq += (x, x, x, x)
          ai2.jpq += (x, x, x, x)
        }
        ai0.jpq += (99, 100)
        ai1.jpq += (99, 100)
        ai2.jpq += (99, 100)

        var ggpt = f(3).asInstanceOf[ArrayBuffer[Int]]
        var p1t = f(0).asInstanceOf[ArrayBuffer[Int]]
        var p2t = f(1).asInstanceOf[ArrayBuffer[Int]]
        var p3t = f(2).asInstanceOf[ArrayBuffer[Int]]

        //      println("GGP============>", ggpt)
        //      println("必胜牌：==========>", p1t.sortWith(_ < _))
        //      println("农民1 ：==========>", p2t.sortWith(_ < _))
        //      println("农民2 ：==========>", p3t.sortWith(_ < _))
        //      println("\n=============================================================================")

        val colourBox = new HashMap[Int, ArrayBuffer[Int]]()
        for (x <- 3 to 15) {
          colourBox.put(x, ArrayBuffer(1, 2, 3, 4))
        }

        var p1 = ArrayBuffer[Int]()
        var p2 = ArrayBuffer[Int]()
        var p3 = ArrayBuffer[Int]()
        var ggp = ArrayBuffer[Int]()

        for (x <- p1t) {
          if (colourBox.containsKey(x)) {
            p1 += x * 10 + colourBox.get(x).remove(0)
          } else {
            p1 += x
          }
        }

        for (x <- p2t) {
          if (colourBox.containsKey(x)) {
            p2 += x * 10 + colourBox.get(x).remove(0)
          } else {
            p2 += x
          }
        }
        for (x <- p3t) {
          if (colourBox.containsKey(x)) {
            p3 += x * 10 + colourBox.get(x).remove(0)
          } else {
            p3 += x
          }
        }
        for (x <- ggpt) {
          if (colourBox.containsKey(x)) {
            ggp += x * 10 + colourBox.get(x).remove(0)
          } else {
            ggp += x
          }
        }

        //      println(p1)
        //      println(p2)
        //      println(p3)
        //      println(ggp)

        //            println(p1.sortWith(_<_))
        //            println(p2.sortWith(_<_))
        //            println(p3.sortWith(_<_))
        //            println(ggp.sortWith(_<_))
        //            println("===================================================================================")

        //====================================================
        //      ggp = ArrayBuffer(64, 104, 114)
        //      p1 = ArrayBuffer(51, 52, 53, 61, 71, 81, 82, 101, 111, 121, 131, 132, 133, 141, 151, 152, 153)
        //      p2 = ArrayBuffer(31, 32, 33, 41, 42, 43, 62, 63, 72, 73, 83, 91, 100, 102, 112, 142, 143)
        //      p3 = ArrayBuffer(34, 44, 54, 74, 84, 92, 93, 94, 99, 103, 113, 122, 123, 124, 134, 144, 154)
        //====================================================

        var px1 = ArrayBuffer[Int]()
        var px2 = ArrayBuffer[Int]()
        var px3 = ArrayBuffer[Int]()

        if (k == 1) {
          px1 = p1
          px2 = p2
          px3 = p3
        } else if (k == 2) {
          px1 = p1
          px2 = p3
          px3 = p2
        }
        //        else if (k == 3) {
        //          px1 = p2
        //          px2 = p1
        //          px3 = p3
        //        } else if (k == 4) {
        //          px1 = p2
        //          px2 = p3
        //          px3 = p1
        //        } else if (k == 5) {
        //          px1 = p3
        //          px2 = p1
        //          px3 = p2
        //        } else if (k == 6) {
        //          px1 = p3
        //          px2 = p2
        //          px3 = p1
        //        }

        var kzs1 = getKZS(outClour(px1)) + 10
        var kzs2 = getKZS(outClour(px2))
        var kzs3 = getKZS(outClour(px3))

        //      println(p1.size, kzs1, p1)
        //      println(p2.size, kzs2, p2)
        //      println(p3.size, kzs3, p3)

        //      if (kzs1 < 1.6 && kzs2 < 1.6 && kzs3 < 1.6) {
        //        println("没有可以叫地主的角色")
        //      } else {

        val max = ArrayBuffer(kzs1, kzs2, kzs3).max
        if (kzs1 == max) {
          r1 += 1
          r22 += 1
          r31 += 1

          segmentConvert(px1 ++= ggp, ai0)
          segmentConvert(px2, ai1)
          segmentConvert(px3, ai2)

          val sp = ai0.ncPoker
          ai0.jpq --= sp
          ai0.opt = commb(sp, ai0.jpq)
          ai0.r = 0

          ai1.jpq --= ai1.ncPoker
          ai1.opt = commb(ai1.ncPoker, ai1.jpq)
          ai1.r = 1

          ai2.jpq --= ai2.ncPoker
          ai2.opt = commb(ai2.ncPoker, ai2.jpq)
          ai2.r = 2

        } else if (kzs2 == max) {

          r2 += 1
          r32 += 1
          r11 += 1

          segmentConvert(px2 ++= ggp, ai0)
          segmentConvert(px1, ai1)
          segmentConvert(px3, ai2)

          //                println(ai0.colourBox)
          //                println(ai1.colourBox)
          //                 println(ai2.colourBox)

          //        println(p1.sortWith(_ < _))
          //        println(p2.sortWith(_ < _))
          //        println(p3.sortWith(_ < _))

          val sp = ai0.ncPoker
          ai0.jpq --= sp
          ai0.opt = commb(sp, ai0.jpq)
          ai0.r = 0

          ai1.jpq --= ai1.ncPoker
          ai1.opt = commb(ai1.ncPoker, ai1.jpq)
          ai1.r = 1

          ai2.jpq --= ai2.ncPoker
          ai2.opt = commb(ai2.ncPoker, ai2.jpq)
          ai2.r = 2

        } else {

          r3 += 1
          r12 += 1
          r21 += 1
          segmentConvert(px3 ++= ggp, ai0)
          segmentConvert(px1, ai1)
          segmentConvert(px2, ai2)

          val sp = ai0.ncPoker
          ai0.jpq = ai0.jpq --= sp
          ai0.opt = commb(sp, ai0.jpq)
          ai0.r = 0

          ai1.jpq --= ai1.ncPoker
          ai1.opt = commb(ai1.ncPoker, ai1.jpq)
          ai1.r = 1

          ai2.jpq --= ai2.ncPoker
          ai2.opt = commb(ai2.ncPoker, ai2.jpq)
          ai2.r = 2
        }

        //      println("===================>当前地主手牌")
        //      ai0.opt.filter(x => x._2.size > 0).foreach(println)
        //      println("===================>当前地主记牌器", ai0.jpq)
        //      println()
        //
        //      println("===================>当前下家农民手牌")
        //      ai2.opt.filter(x => x._2.size > 0).foreach(println)
        //      println("===================>当前下家农民记牌器", ai2.jpq)
        //      println()
        //
        //      println("===================>当前上家农民手牌")
        //      ai1.opt.filter(x => x._2.size > 0).foreach(println)
        //      println("===================>当前上家农民记牌器", ai1.jpq)
        //      println()
        //      println("==============================>牌局开始")

        //角色压栈
        val s = new ArrayDeque[AI]()

        s.push(ai1)
        s.push(ai2)
        s.push(ai0)

        //      println(ai1.opt)
        //      println(ai2.opt)
        //      println(ai0.opt)

        //角色打出的牌
        var zp = ArrayBuffer[Int]()

        //打牌的角色
        var h = 0

        //      println("地主=============>>>")
        //      ai0.opt.flatMap(_._2).foreach(println)
        //       println("上家农民=============>>>")
        //      ai1.opt.flatMap(_._2).foreach(println)
        //       println("下家农民=============>>>")
        //      ai2.opt.flatMap(_._2).foreach(println)

        var j = 0

        //        println("#######################################################################")
        //        ai0.opt.flatMap(_._2).foreach(println)
        //        println("#######################################################################")
        //        ai1.opt.flatMap(_._2).foreach(println)
        //        println("#######################################################################")
        //        ai2.opt.flatMap(_._2).foreach(println)
        //        println("#######################################################################\n\n")

        while (ai0.opt.flatMap(_._2).size != 0 && ai1.opt.flatMap(_._2).size != 0 && ai2.opt.flatMap(_._2).size != 0) {

          val ai = s.pop()

          //地主主动出牌
          if (ai.r == 0 && (ai.h != 1 && ai.h != 2)) {
            //          println(1)
            //地主主动出牌
            zp = d1.activeDz(ai)

            //println("d1主动出牌==============>", colourConvertN(zp, ai))
            ai1.t = zp
            ai1.h = 0

            ai2.t = zp
            ai2.h = 0

            ai0.h = 0

            h = 0

            s.addLast(ai)

          } //地主被动出牌
          else if (ai.r == 0 && (ai.h == 1 || ai.h == 2)) {
            //           println(2)
            zp = d1.passiveDz(ai)

            //println("d1被动出牌==============>", colourConvertN(zp, ai))
            //          if (!check(colourConvertN(zp, ai), ai)) {
            //            errorPock += 0
            //          }
            if (zp.size > 0) {
              ai0.h = 0
              ai1.t = zp
              ai1.h = 0
              ai2.t = zp
              ai2.h = 0

              h = 0
            }
            s.addLast(ai)
          } //地主下家农民主动出牌
          else if (ai.r == 2 && ai.h == 2) {
            //           println(3)
            zp = n2.activeNm(ai)
            //println("n2农民主动出牌===========>", colourConvertN(zp, ai))
            ai0.t = zp
            ai0.h = 2
            ai1.t = zp
            ai1.h = 2
            ai2.h = 2

            h = 2
            s.addLast(ai)

          } //地主下家农民被动出牌
          else if (ai.r == 2 && ai.h != 2) {
            //           println(4)
            zp = n2.passiveNm(ai)

            //println("n2农民被动出牌===========>", colourConvertN(zp, ai))

            //          println(1)

            //          if (!check(colourConvertN(zp, ai), ai)) {
            //            errorPock += 0
            //          }
            if (zp.size > 0) {
              ai0.t = zp
              ai0.h = 2
              ai1.t = zp
              ai1.h = 2
              ai2.h = 2

              h = 2
            }
            s.addLast(ai)
          } //地主上家农民主动出牌
          else if (ai.r == 1 && ai.h == 1) {
            //         println(5)

            //           println(ai.r,ai.h,ai.t,ai.zdnm,"------->>>")

            zp = n1.activeNm(ai)

            //println("n1农民主动出牌===========>", colourConvertN(zp, ai))
            ai0.t = zp
            ai0.h = 1
            ai2.t = zp
            ai2.h = 1
            ai1.h = 1
            h = 1
            s.addLast(ai)
          } else if (ai.r == 1 && ai.h != 1) {
            //地主上家农民被动出牌
            //           println(6)
            zp = n1.passiveNm(ai)

            //println("n1农民被动出牌===========>", colourConvertN(zp, ai))

            //          if (!check(colourConvertN(zp, ai), ai)) {
            //            errorPock += 0
            //          }
            if (zp.size > 0) {
              ai0.t = zp
              ai0.h = 1
              ai2.t = zp
              ai2.h = 1

              ai1.h = 1
              h = 1
            }

            s.addLast(ai)
          }

          //          j += 1
          //
          //          if (j % 3 == 0) {
          //            println()
          //          }

        }

        //      i += 1
        //
        //      if (i % 100 == 0) {
        //        println("完成" + i + "盘对局")
        //        println("异常总数：", exceptions)
        //        println("出错牌次数：", errorPock)
        //        //println(i, "次! 总耗时：", (System.currentTimeMillis() - t1) / 1000 + " sec.")
        //      }

        //     println(h,"----------------->>")

        if ((k == 1 || k == 2) && h == 0) {
          d1Num += 1
          //        if (kzs1 == max) {
          //          r1w += 1
          //          writer.println("R1,R2,R3\tR1 d1")
          //        } else if (kzs2 == max) {
          //          r2w += 1
          //          writer.println("R1,R2,R3\tR2 d1")
          //        } else {
          //          r3w += 1
          //          writer.println("R1,R2,R3\tR3 d1")
          //        }

        }
        if (h == 1) {
          n1Num += 1
          n2Num += 1
          //        if (kzs2 == max) {
          //          r11w += 1
          //          writer.println("R1,R2,R3\tR1 n1")
          //        } else if (kzs2 == max) {
          //          r21w += 1
          //          writer.println("R1,R2,R3\tR2 n1")
          //        } else {
          //          r31w += 1
          //          writer.println("R1,R2,R3\tR3 n1")
          //        }

        }
        if (h == 2) {

          n2Num += 1
          n1Num += 1
          //        if (kzs1 == max) {
          //          r12w += 1
          //          writer.println("R1,R2,R3\tR1 n2")
          //        } else if (kzs2 == max) {
          //          r22w += 1
          //          writer.println("R1,R2,R3\tR2 n2")
          //        } else {
          //          r32w += 1
          //          writer.println("R1,R2,R3\tR3 n2")
          //        }

        }

      }

      //println(d1Num, n1Num, n2Num)
      if (d1Num == 2) {

        i += 1
        if (i % 1000 == 0) {
          println(i)
        }
        //        println("#############################################################################################")
        //        println(c0)
        //                c1 = c1.map { x => if (x == 99 || x == 100) { x } else { x / 10 } }
        //                c2 = c2.map { x => if (x == 99 || x == 100) { x } else { x / 10 } }
        //                c3 = c3.map { x => if (x == 99 || x == 100) { x } else { x / 10 } }
        //        println()
        //        commb(c1.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(100)).flatMap(_._2).foreach(println)
        //        println()
        //        commb(c2.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(100)).flatMap(_._2).foreach(println)
        //        println()
        //        commb(c3.asInstanceOf[ArrayBuffer[Int]], ArrayBuffer(100)).flatMap(_._2).foreach(println)
        //        println("#############################################################################################\n")

        writer.println((c1.sortWith(_>_) + "$" + c2.sortWith(_>_) + "$" + c3.sortWith(_>_)+"$"+c0.sortWith(_>_)).replace("ArrayBuffer(", "").replace(")", "").replace(" ", ""))

        c0.clear
        c1.clear
        c2.clear
        c3.clear

      }

      d1Num = 0
      n1Num = 0
      n2Num = 0
    }

    writer.close()

    //}).start()
    //}

    //writer.println("所有角色统计：" + d1Num, n1Num, n2Num)
    //    writer.println("R1玩家统计：" + r1 + ":" + r1w, r11 + ":" + r11w, r12 + ":" + r12w)
    //    writer.println("R2玩家统计：" + r2 + ":" + r2w, r21 + ":" + r21w, r22 + ":" + r22w)
    //    writer.println("R3玩家统计：" + r3 + ":" + r3w, r31 + ":" + r31w, r32 + ":" + r32w)

    //writer.close()

  }
}