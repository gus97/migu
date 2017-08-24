package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import com.gus.ai.CommbRule._
import com.gus.ai.NComm._
import scala.io.StdIn
import util.control.Breaks._

object Mapp100 {

  def main(args: Array[String]): Unit = {

    run()
  }

  def cr(c: ArrayBuffer[Int]): ArrayBuffer[String] = {

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

  def run(): Unit = {

    val ai = new AI
    val af = new AFun
    
    val nf = new NFun

    for (x <- 3 to 15) {
      ai.jpq += (x, x, x, x)
    }
    ai.jpq += (99, 100)

    var sv = 0

    //ai.jpq = ArrayBuffer(4,6,9,13,8,8,11,15,3,3,6,9,9,12,13,13,14)

    //角色 n1 地主下家农民 n2 地主上家农民 d1 地主

    val ggp = ArrayBuffer[Int]()

    //手牌 
    var ab = ArrayBuffer[Int]()

    val p = ArrayBuffer("3", "4", "5", "6", "7", "8", "9", "T", "J", "Q", "K", "A", "2", "S", "B")

    println("\n------------------------------------------------------------------------------------------------------\n")

    println(s"""角色划分：地主标识 D 地主上家农民 F 地主下家农民 G\n
发牌与叫牌阶段:(所有字母输入大写)\n
第一步,给AI输入17张张牌手牌：V 3 4 5 6 7 8 9 T J J Q K A 2 2 S B (T为10,S为小王,B为大王)\n
第二步,AI会给出这副牌的叫地主上限，0为建议做农民,1为建议叫地主,2为建议抢地主,3为建议加倍\n
第三步,在确定叫牌结束后需要输入三张底牌,示例： O 2 J 6\n
第四步,根据现场实际情况,如果最后确定AI做地主上家农民，则输入PP, AI做地主下家农民当,则输入PN, AI做地主则输入L
开始打牌阶段：\n
场外所有主被动出牌者,都需要录入,示例：\n
地主打了               AA  需要给AI输入： L   A A\n
上家农民动打了    AA  需要给AI输入： PP  A A\n
下家农民动打了    AA  需要给AI输入： PN  A A\n
第五步,AI的任何出牌动作输入数字 "1" AI会根据当前自己的角色属性自动判断主动被动下的出牌 \n
任意角色被动出牌要不起时,则不需要输入\n""")
    println("------------------------------------------------------------------------------------------------------")

    while (true) {

      breakable {
        try {
          var content = StdIn.readLine()
          if (content.startsWith("V")) { //获得原始17张手牌

            if (sv == 1) {

              println("ERROR,本局已经开始!")
              break
            }

            if (content.split(" ").size != 18) { //|| ab.min < 3 || (ab -- ArrayBuffer(99, 100)).max > 15 || ab.max > 100

              println("ERROR,请检查牌数是否为17张")
              break
            } else {
              ab.clear
              val a = content.split(" ").takeRight(17)

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

              //V 3 4 5 6 7 8 9 T J J Q K A 2 2 S B

              //找出控制数
              val cnt = getKZS(ab)

              //叫地主：若总控制数>=1，则叫地主；否则不叫
              //抢地主：若总控制数>1.8，则抢地主；否则不抢
              //加倍：若总控制数>2.5,则加倍；否则不加倍
              //println("控制数：" + cnt)
              if (cnt >= 2.5) {
                println("加倍")
              } else if (cnt >= 2.0) {
                println("抢地主")
              } else if (cnt >= 1.5) {
                println("叫地主")
              } else {
                println("当农民")
              }
              ai.jpq --= ab
              //println("手牌：", ab.sortWith(_ > _))
            }
          } else if (content.startsWith("O")) {

            if (content.split(" ").size != 4) { //|| ab.min < 3 || (ab -- ArrayBuffer(99, 100)).max > 15 || ab.max > 100

              println("ERROR,请检查底牌数是否为3张")
              break
            } else {
              val a = content.split(" ").takeRight(3)

              for (x <- a) {
                if (!p.contains(x)) {
                  println("ERROR,包含非法牌值", x)
                  break
                }

                if (x.equals("T")) {
                  ggp += 10
                } else if (x.equals("J")) {
                  ggp += 11
                } else if (x.equals("Q")) {
                  ggp += 12
                } else if (x.equals("K")) {
                  ggp += 13
                } else if (x.equals("A")) {
                  ggp += 14
                } else if (x.equals("2")) {
                  ggp += 15
                } else if (x.equals("S")) {
                  ggp += 99
                } else if (x.equals("B")) {
                  ggp += 100
                } else {
                  ggp += x.toInt
                }
              }

              println("当前底牌:", cr(ggp).toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))
              sv += 1
            }
          } else if (content.equals("L")) {
            //println(ab)
            //V 3 4 5 6 7 8 9 T J J Q K A 2 2 S B
            ai.dz = 20
            ai.n1 = 17
            ai.n2 = 17
            //println(s"获得底牌：$ggp")

            //segmentConvert(ab ++= ggp, ai)
            ab ++= ggp
            println("选择当地主", cr(ab).toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))
            //ab ++= ggp
            ai.jpq --= ggp
            //println("当前手牌：", ab.sortWith(_ < _).toList)
            //var p = "开始寻找最优组牌 :"

            //            new Thread(new Runnable() {
            //              def run() {
            //                var g = ". "
            //                print(p)
            //                for (x <- 1 to 100) {
            //                  g = "."
            //                  Thread.sleep(10)
            //                  print(g)
            //                }
            //                println()

            //println("AI手牌：", ab.sortWith(_ > _))
            ai.r = 0
            //找出最优组牌
            ai.opt = commb(ab, ai.jpq)
            //ai.opt.flatMap(_._2).toList.foreach(println)
            //println("非冲锋套========>")
            //getNCFT(ai).flatMap(_._2).foreach(println)
            //println("冲锋套========>")
            //getCFT(ai).flatMap(_._2).foreach(println)
            //println("炸弹========>")
            //ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
            //              }
            //            }).start()
          } else if (content.equals("PP")) {
            ai.dz = 20
            ai.n1 = 17
            ai.n2 = 17
            ai.r = 1
            //segmentConvert(ab, ai)
            //println("AI作为地主上家农民")
            ai.opt = commb(ab, ai.jpq)
            //println("非冲锋套========>")
            //getNCFT(ai).flatMap(_._2).foreach(println)
            //println("冲锋套========>")
            //getCFT(ai).flatMap(_._2).foreach(println)
            //println("炸弹========>")
            //ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
            println("已选择当地主上家农民", cr(ab).toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))
          } else if (content.equals("PN")) {
            ai.dz = 20
            ai.n1 = 17
            ai.n2 = 17
            ai.r = 2
            //segmentConvert(ab, ai)

            ai.opt = commb(ab, ai.jpq)
            //            println("非冲锋套========>")
            //            getNCFT(ai).flatMap(_._2).foreach(println)
            //            println("冲锋套========>")
            //            getCFT(ai).flatMap(_._2).foreach(println)
            //            println("炸弹========>")
            //            ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
            println("已选择当地主下家农民", cr(ab).toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))
          } else if (content.startsWith("L") && content.size > 2) {

            val n = content.split(" ").length
            val a = content.split(" ").takeRight(n - 1)
            for (x <- a) {
              if (!p.contains(x)) {
                println("ERROR,包含非法牌值", x)
                break
              }
            }
            ai.t.clear
            for (x <- a) {
              if (x.equals("T")) {
                ai.t += 10
              } else if (x.equals("J")) {
                ai.t += 11
              } else if (x.equals("Q")) {
                ai.t += 12
              } else if (x.equals("K")) {
                ai.t += 13
              } else if (x.equals("A")) {
                ai.t += 14
              } else if (x.equals("2")) {
                ai.t += 15
              } else if (x.equals("S")) {
                ai.t += 99
              } else if (x.equals("B")) {
                ai.t += 100
              } else {
                ai.t += x.toInt
              }
            }
            ai.dz -= ai.t.length
            ai.h = 0

            //            ai.jpq --= content.split(" ")(1).split(",").map(_.toInt)
            //            if (content.split(" ")(2).equals("n1")) {
            //              ai.n1 -= content.split(" ")(1).split(",").length
            //              ai.h = 1
            //              println("地主上家农民出牌========>\t", "[" + content.split(" ")(1) + "]")
            //            } else if (content.split(" ")(2).equals("n2")) {
            //              ai.n2 -= content.split(" ")(1).split(",").length
            //              ai.h = 2
            //              println("地主下家农民出牌========>\t", "[" + content.split(" ")(1) + "]")
            //            } else if (content.split(" ")(2).equals("d1")) {
            //              ai.dz -= content.split(" ")(1).split(",").length
            //              ai.h = 0
            //              println("地主出牌========>\t", "[" + content.split(" ")(1) + "]")
            //            }
            //            ai.t.clear()
            //            for (x <- content.split(" ")(1).split(",")) {
            //              ai.t += x.toInt
            //            }

          } //V 4 5 5 5 7 8 8 T J J J K K A A 2 B
          else if (content.startsWith("PP") && content.size > 2) {
            val n = content.split(" ").length
            val a = content.split(" ").takeRight(n - 1)
            for (x <- a) {
              if (!p.contains(x)) {
                println("ERROR,包含非法牌值", x)
                break
              }
            }
            ai.t.clear
            for (x <- a) {
              if (x.equals("T")) {
                ai.t += 10
              } else if (x.equals("J")) {
                ai.t += 11
              } else if (x.equals("Q")) {
                ai.t += 12
              } else if (x.equals("K")) {
                ai.t += 13
              } else if (x.equals("A")) {
                ai.t += 14
              } else if (x.equals("2")) {
                ai.t += 15
              } else if (x.equals("S")) {
                ai.t += 99
              } else if (x.equals("B")) {
                ai.t += 100
              } else {
                ai.t += x.toInt
              }
            }
            ai.n1 -= ai.t.length
            ai.h = 1

          } else if (content.startsWith("PN") && content.size > 2) {
            val n = content.split(" ").length
            val a = content.split(" ").takeRight(n - 1)
            for (x <- a) {
              if (!p.contains(x)) {
                println("ERROR,包含非法牌值", x)
                break
              }
            }
            ai.t.clear
            for (x <- a) {
              if (x.equals("T")) {
                ai.t += 10
              } else if (x.equals("J")) {
                ai.t += 11
              } else if (x.equals("Q")) {
                ai.t += 12
              } else if (x.equals("K")) {
                ai.t += 13
              } else if (x.equals("A")) {
                ai.t += 14
              } else if (x.equals("2")) {
                ai.t += 15
              } else if (x.equals("S")) {
                ai.t += 99
              } else if (x.equals("B")) {
                ai.t += 100
              } else {
                ai.t += x.toInt
              }
            }
            ai.n2 -= ai.t.length
            ai.h = 2
          } else if (content.equals("1")) {
            if (ai.opt.flatMap(_._2).flatMap(x => x).size == 0) {
              println("牌局已结束")
            } else if (ai.r == 0 && ai.h == 0) {

              val a = cr(af.activeDz(ai: AI))
              println("出牌：", a.toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))

              for (x <- a) {
                if (x.equals("T")) {
                  ai.t += 10
                } else if (x.equals("J")) {
                  ai.t += 11
                } else if (x.equals("Q")) {
                  ai.t += 12
                } else if (x.equals("K")) {
                  ai.t += 13
                } else if (x.equals("A")) {
                  ai.t += 14
                } else if (x.equals("2")) {
                  ai.t += 15
                } else if (x.equals("S")) {
                  ai.t += 99
                } else if (x.equals("B")) {
                  ai.t += 100
                } else {
                  ai.t += x.toInt
                }
              }
              if (a.length > 0) {
                ai.dz -= a.length
                ai.h = 0
              }

            } else if (ai.r == 0 && ai.h != 0) {

              val a = cr(af.passiveDz(ai: AI))
              println("出牌：", a.toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))

              for (x <- a) {
                if (x.equals("T")) {
                  ai.t += 10
                } else if (x.equals("J")) {
                  ai.t += 11
                } else if (x.equals("Q")) {
                  ai.t += 12
                } else if (x.equals("K")) {
                  ai.t += 13
                } else if (x.equals("A")) {
                  ai.t += 14
                } else if (x.equals("2")) {
                  ai.t += 15
                } else if (x.equals("S")) {
                  ai.t += 99
                } else if (x.equals("B")) {
                  ai.t += 100
                } else {
                  ai.t += x.toInt
                }
              }
              if (a.length > 0) {
                ai.dz -= a.length
                ai.h = 0
              }

            } else if (ai.r != 0 && ai.h == ai.r) {

              val a = cr(nf.activeNm(ai: AI))

              println("出牌：", a.toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))

              for (x <- a) {
                if (x.equals("T")) {
                  ai.t += 10
                } else if (x.equals("J")) {
                  ai.t += 11
                } else if (x.equals("Q")) {
                  ai.t += 12
                } else if (x.equals("K")) {
                  ai.t += 13
                } else if (x.equals("A")) {
                  ai.t += 14
                } else if (x.equals("2")) {
                  ai.t += 15
                } else if (x.equals("S")) {
                  ai.t += 99
                } else if (x.equals("B")) {
                  ai.t += 100
                } else {
                  ai.t += x.toInt
                }
              }

              if (ai.r == 1 && a.length > 0) {
                ai.n1 -= a.length
                ai.h = 1

              } else if (ai.r == 2 && a.length > 0) {
                ai.n2 -= a.length
                ai.h = 2
              }

            } else if (ai.r != 0 && ai.h != ai.r) {

              val a = cr(nf.passiveNm(ai: AI))

              println("出牌：", a.toString().replace("ArrayBuffer", "").replace("(", "").replace(")", ""))

              for (x <- a) {
                if (x.equals("T")) {
                  ai.t += 10
                } else if (x.equals("J")) {
                  ai.t += 11
                } else if (x.equals("Q")) {
                  ai.t += 12
                } else if (x.equals("K")) {
                  ai.t += 13
                } else if (x.equals("A")) {
                  ai.t += 14
                } else if (x.equals("2")) {
                  ai.t += 15
                } else if (x.equals("S")) {
                  ai.t += 99
                } else if (x.equals("B")) {
                  ai.t += 100
                } else {
                  ai.t += x.toInt
                }
              }

              if (ai.r == 1 && a.length > 0) {
                ai.n1 -= a.length
                ai.h = 1

              } else if (ai.r == 2 && a.length > 0) {
                ai.n2 -= a.length
                ai.h = 2
              }

            }

          } else if (content.equals("99")) {
            println("当前手牌")
            println("非冲锋套========>")
            getNCFT(ai).flatMap(_._2).foreach(println)
            println("冲锋套========>")
            getCFT(ai).flatMap(_._2).foreach(println)
            println("炸弹========>")
            ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
            println("记牌器：", ai.jpq)
            println("上家农民：", ai.n1)
            println("下家农民：", ai.n2)
            println("地主：", ai.dz)
            println("ai.h：", ai.h)
            println("ai.t：", ai.t)
            println("ai.r：", ai.r)
            println("战斗标记", ai.zdnm)
          } 
          else {
            println("ERROR:不存在的命令,重新输入!")
          }
        } catch {
          case t: Throwable =>
        }
      }
    }
  }
}