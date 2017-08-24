package com.gus.ai

import com.gus.ai.CommbRule._
import com.gus.ai.NComm._
import scala.io.StdIn
import util.control.Breaks._
import scala.collection.mutable.ArrayBuffer

object TestP {

  val p = ArrayBuffer("3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A", "2", "S", "B")

  def supperTest(ai: AI): ArrayBuffer[Int] = {

    val af = new AFun
    val nf = new NFun

    ai.opt = commb(ai.handPoker, ai.jpq)

    var hit = ArrayBuffer[Int]()
    if (ai.r == 0 && ai.h == 0) {
      hit = af.activeDz(ai).sortWith(_ < _)

    } else if (ai.r == 0 && ai.h != 0) {
      hit = af.passiveDz(ai).sortWith(_ < _)

    } else if ((ai.r == 1 && ai.h == 1) || (ai.r == 2 && ai.h == 2)) {
      hit = nf.activeNm(ai).sortWith(_ < _)

    } else if ((ai.r == 1 && ai.h != 1) || (ai.r == 2 && ai.h != 2)) {
      hit = nf.passiveNm(ai).sortWith(_ < _)

    } else {
      ArrayBuffer(-1)
    }

    hit
  }

  //打出的牌转换
  def out(c: ArrayBuffer[Int]): ArrayBuffer[String] = {

    val ab = ArrayBuffer[String]()

    for (x <- c) {

      if (x == 10) {
        ab += "T"
      } else if (x == 11) {
        ab += "J"
      } else if (x == 12) {
        ab += "Q"
      } else if (x == 13) {
        ab += "K"
      } else if (x == 14) {
        ab += "A"
      } else if (x == 15) {
        ab += "2"
      } else if (x == 99) {
        ab += "S"
      } else if (x == 100) {
        ab += "B"
      } else {
        ab += x.toString()
      }
    }

    ab
  }

  def in(in: String) {

    val a = in.split(" ")

    val ab = ArrayBuffer[Int]()
    for (x <- a) {
      if (!p.contains(x)) {
        println("ERROR,包含非法牌值", x)

      }

      if (x.equals("T")) {
        ab += 10
      } else if (x.equals("J")) {
        ab += 11
      } else if (x.equals("Q")) {
        ab += 12
      } else if (x.equals("K")) {
        ab += 13
      } else if (x.equals("A")) {
        ab += 14
      } else if (x.equals("2")) {
        ab += 15
      } else if (x.equals("S")) {
        ab += 99
      } else if (x.equals("B")) {
        ab += 100
      } else {
        ab += x.toInt
      }
    }
  }

  def main(args: Array[String]): Unit = {

    try {

      while (true) {
        val ai = new AI
        breakable {
          var content = StdIn.readLine("输入当前AI角色[L：地主,PP:地主上家农民,PN:地主下家农民]: ")

          while ((!"L".equals(content)) && (!"PP".equals(content)) && (!"PN".equals(content)))
            if ((!"L".equals(content)) && (!"PP".equals(content)) && (!"PN".equals(content))) {
              println("ERROR,角色输入错误", content)
              break
            }
          var r = 0

          if (content.equals("PP")) {
            r = 1
          } else if (content.equals("PN")) {
            r = 2
          }
          //角色
          ai.r = r

          content = StdIn.readLine("输入当前AI手牌，[输入示例   7 8 9 T J Q K B S]: ")

          val a = content.split(" ")

          val ab = ArrayBuffer[Int]()

          for (x <- a) {

            if (!p.contains(x)) {
              println("ERROR,包含非法牌值", x)
              break
            }
            if (x.equals("T")) {
              ab += 10
            } else if (x.equals("J")) {
              ab += 11
            } else if (x.equals("Q")) {
              ab += 12
            } else if (x.equals("K")) {
              ab += 13
            } else if (x.equals("A")) {
              ab += 14
            } else if (x.equals("2")) {
              ab += 15
            } else if (x.equals("S")) {
              ab += 99
            } else if (x.equals("B")) {
              ab += 100
            } else {
              ab += x.toInt
            }

          }
          //手牌
          ai.handPoker = ab

          //记牌器设置
          content = StdIn.readLine("场外牌,[输入示例  3 4 5 6 7 8 9 J J Q Q A A 2]: ")

          val b = content.split(" ")

          for (x <- b) {
            if (!p.contains(x)) {
              println("ERROR,包含非法牌值", x)
              break
            }
            if (x.equals("T")) {
              ai.jpq += 10
            } else if (x.equals("J")) {
              ai.jpq += 11
            } else if (x.equals("Q")) {
              ai.jpq += 12
            } else if (x.equals("K")) {
              ai.jpq += 13
            } else if (x.equals("A")) {
              ai.jpq += 14
            } else if (x.equals("2")) {
              ai.jpq += 15
            } else if (x.equals("S")) {
              ai.jpq += 99
            } else if (x.equals("B")) {
              ai.jpq += 100
            } else {
              ai.jpq += x.toInt
            }

          }

          //主动被动
          content = StdIn.readLine("当前角色主动出牌/被动出牌,输入示例: 主动出牌 A 被动出牌 P: ")

          if ("A".equals(content)) {
            ai.h = ai.r
          } else if ("P".equals(content)) {

            content = StdIn.readLine("上一轮出牌角色 ,输入示例: 地主 L 上家农民 PP 下家农民PN: ")

            if ((!"L".equals(content)) && (!"PP".equals(content)) && (!"PN".equals("content"))) {
              println("ERROR,角色输入错误", content)
              break
            }
            var h = -1
            if (content.equals("PP")) {
              h = 1
            } else if (content.equals("PN")) {
              h = 2
            } else if (content.equals("L")) {
              h = 0
            } else {
              println("ERROR,角色输入错误", content)
              break
            }
            //上一轮出牌角色
            ai.h = h

            //上轮出牌
            content = StdIn.readLine("上一轮的出牌 ,[输入示例  3 4 5 6 7 8 9 J J Q Q A A 2]: ")

            val c = ArrayBuffer[Int]()

            for (x <- content.split(" ")) {

              if (!p.contains(x)) {
                println("ERROR,包含非法牌值", x)
                break

              }
              if (x.equals("T")) {
                c += 10
              } else if (x.equals("J")) {
                c += 11
              } else if (x.equals("Q")) {
                c += 12
              } else if (x.equals("K")) {
                c += 13
              } else if (x.equals("A")) {
                c += 14
              } else if (x.equals("2")) {
                c += 15
              } else if (x.equals("S")) {
                c += 99
              } else if (x.equals("B")) {
                c += 100
              } else {
                c += x.toInt
              }

            }
            //
            ai.t = c

          } else {
            println("ERROR,包含非法牌值", content)
            break
          }

          content = StdIn.readLine("输入场外三家牌数 ,示例[3 3 5] 顺序 [地主 上家农民 下家农民]: ")

          val num = content.split(" ")
          if (num.size != 3) {
            println("ERROR,场外三家牌数错误", content)
            break
          }

          ai.dz = num(0).toInt
          ai.n1 = num(1).toInt
          ai.n2 = num(2).toInt

          println(ai.r, ai.t, ai.h, ai.jpq)
          val x = supperTest(ai)
          if (x.size > 0) {

            println("出牌：", x.toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))
          } else {
            println("不要")
          }

          content = StdIn.readLine("""继续测试按 "1":""")

          if (content.equals("1")) {
            break
          } else {
            break
          }
        }
      }
    } catch {
      case t: Throwable => 
    }
  }
}