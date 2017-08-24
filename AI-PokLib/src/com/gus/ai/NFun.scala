package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import com.gus.ai.CommbRule._
import com.gus.ai.NComm._
import scala.util.Try
import scala.io.StdIn

class NFun {

  /**
   * 面对地主报单，农民主动出牌
   */
  def activeFaceLandlordCallOne(ai: AI): ArrayBuffer[Int] = {

    try {
      if (ai.opt.flatMap(_._2).size == 0) {
        println("牌局结束")
        return ArrayBuffer(-1)
      }
      var hit = new ArrayBuffer[Int]()

      if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size <= 1) {

        if (ai.opt.filter(_._1 != 11).flatMap(_._2).size > 0) {

          hit = ai.opt.filter(_._1 != 11).flatMap(_._2).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else {

          if (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0) {

            hit = getCFT(ai).get(11).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

          } else {
            hit = ai.opt.flatMap(_._2).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        }

      } else {

        //1)	以“地主报单农民组牌方式”进行组牌
        ai.opt = commbDS(ai.opt.flatMap(_._2).flatMap(x => x).toBuffer, ai.jpq)

        //a)	若手牌中非冲锋套的单牌套数-2*炸弹（非火箭）套数>1，则进入e）
        if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size -
          2 * (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size) > 1) {
          //e)	若自己为地主上家农民，则进入g），否则
          //地主下家农民
          if (ai.r == 2) {

            /**
             * f)	假设场外标签最大的单牌在地主手中，并将剩余牌张放入地主上家农民手中，对其按照地主报单农民组牌方式进行组套，进入ⅰ
             * i.	若地主上家农民手牌中非冲锋单牌套数<=1,则进入ⅱ，否则打出单牌中标签最小的一套
             * ii.	将自己手牌与地主上家农民手牌中非单牌非炸弹的所有套牌类型进行比对。若找到种类相同的套牌，且该套牌的标签小于地主上家农民，则打出，否则打出单牌中标签最小的一套
             */
            val otherJpq = (ai.opt.flatMap(_._2).flatMap(x => x).toBuffer += ai.jpq.max)
            val otherNm = (ai.jpq -= ai.jpq.max)

            val otherOpt = commbDS(otherNm, otherJpq)

            val oai = new AI
            oai.opt = otherOpt
            oai.dz = 1
            otherJpq.map(oai.jpq += _)

            //若地主上家农民手牌中非冲锋单牌套数<=1,则进入ⅱ
            if (getNCFT(oai).get(11).getOrElse(ArrayBuffer()).size <= 1) {

              //将自己手牌与地主上家农民手牌中非单牌非炸弹的所有套牌类型进行比对。若找到种类相同的套牌，且该套牌的标签小于地主上家农民，则打出，否则打出单牌中标签最小的一套
              val ok = oai.opt.filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 4 && x._1 != 1).keySet
              val ik = ai.opt.filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 4 && x._1 != 1).keySet
              val jk = ok & ik
              var fg = true
              for (x <- jk if fg) {
                if (ai.opt.get(x).get.head.head < oai.opt.get(x).get.last.head) {
                  hit = ai.opt.get(x).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }

              if (hit.size > 0) {
                return hit
              } else {
                //否则打出单牌中标签最小的一套
                hit = getNCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }

            } else {
              //否则打出单牌中标签最小的一套
              hit = getNCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }

          } else {
            //g)	若手牌中有非单非炸弹套牌，则打出其中标签最小的一套，否则
            if (ai.opt.filter(x => x._1 != 1 && x._1 != 4 && x._1 != 11).flatMap(_._2).size > 0) {
              hit = ai.opt.filter(x => x._1 != 1 && x._1 != 4 && x._1 != 11).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {

              //h)	若手牌中有炸弹（非火箭），且单牌的套数>=2，则打出其中标签最小的四带二（单），
              if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.filter(x => x._1 == 11).flatMap(_._2).size >= 2) {

                val p4 = ai.opt.get(4).get.head
                val p1 = ai.opt.get(11).get.take(2)
                hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
                ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
                ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
                ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
              } else {
                //i)	打出最大的单牌
                hit = ai.opt.get(11).get.last
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        } else {
          //b)	若手中单牌套数>=2，且手中有炸弹（非火箭），则打出标签最小的四带二（单），否则
          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.filter(x => x._1 == 11).flatMap(_._2).size >= 2) {
            val p4 = ai.opt.get(4).get.head
            val p1 = ai.opt.get(11).get.take(2)
            hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
            ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
            ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
            ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
          } else {
            //c)	若手中有冲锋套，则打出长度最长标签最小的冲锋套，否则

            if (getCFT(ai).size > 0) {
              hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              //d)	若手中有炸弹或火箭，则打出其中标签最小的一套，否则打出标签最小的套牌
              if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                hit = ai.opt.get(1).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

              } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                hit = ai.opt.get(4).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                //否则打出标签最小的套牌
                hit = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }

  }

  /**
   * 能跑必管逻辑
   * i.	若手中没有大于X的炸弹，则跳出能跑必管逻辑，否则
   * ii.	若手中非冲锋单牌套数-（炸弹套数-1）*2<=1，则打出大于X标签最小的炸弹，否则
   * iii.	假设场外标签最大的单牌在地主手中，并将剩余牌张放入另一家农民手中，对其按照地主报单农民组牌方式进行组套
   * （1）	若另一家农民手牌中非冲锋单牌套数-炸弹*2<=1,则进入（2），否则跳出能跑必管逻辑
   * （2）	将自己手牌与另一家农民手牌中非单牌非炸弹的所有套牌类型进行比对。若找到种类相同的套牌，且该套牌的标签小于另一家农民，则打出大于X的炸弹中最小的一套，否则跳出能跑必管逻辑
   *
   */

  def run(ai: AI): ArrayBuffer[Int] = {

    try {

      var hit = new ArrayBuffer[Int]()
      val px = judgeTp(ai.t)

      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

        //若手中非冲锋单牌套数-（炸弹套数-1）*2<=1，则打出大于X标签最小的炸弹
        if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size -
          (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size - 1) <= 1) {

          //则打出大于X标签最小的炸弹
          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

            if (px != 4 && px != 1) {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else if (px == 4 && ai.opt.get(4).get.filter(x => x.head > ai.t.head).length > 0) {

              hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).head

              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }

          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {

            hit = ai.opt.get(1).get.head

            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } else {
          //	假设场外标签最大的单牌在地主手中，并将剩余牌张放入另一家农民手中，对其按照地主报单农民组牌方式进行组套

          val otherJpq = (ai.opt.flatMap(_._2).flatMap(x => x).toBuffer += ai.jpq.max)
          val otherNm = (ai.jpq -= ai.jpq.max)

          val otherOpt = commbDS(otherNm, otherJpq)

          val oai = new AI
          oai.opt = otherOpt
          oai.dz = 1
          otherJpq.map(oai.jpq += _)

          //若另一家农民手牌中非冲锋单牌套数-炸弹*2<=1,则进入（2）
          if (getNCFT(oai).get(11).getOrElse(ArrayBuffer()).size
            - (oai.opt.get(1).getOrElse(ArrayBuffer()).size + oai.opt.get(4).getOrElse(ArrayBuffer()).size) * 2 <= 1) {
            //将自己手牌与另一家农民手牌中非单牌非炸弹的所有套牌类型进行比对。若找到种类相同的套牌，且该套牌的标签小于另一家农民，则打出大于X的炸弹中最小的一套，否则跳出能跑必管逻辑

            val ok = oai.opt.filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 4 && x._1 != 1).keySet
            val ik = ai.opt.filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 4 && x._1 != 1).keySet

            val jk = ok & ik
            var fg = true
            for (x <- jk if fg) {

              if (ai.opt.get(x).get.head.head < oai.opt.get(x).get.last.head) {
                if (px != 4 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(4).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                } else if (px != 4 && ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(1).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                } else if (px == 4 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                } else if (px == 4 && ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(1).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            }

          } else {
            return hit
          }
        }
      } else {
        return hit
      }
      hit
    } catch {
      case t: Throwable =>
             ArrayBuffer() // TODO: handle error
    }
  }

  /**
   * 面对地主报单，农民被动出牌
   */
  def passiveFaceLandlordCallOne(ai: AI): ArrayBuffer[Int] = {

    try {
      if (ai.opt.flatMap(_._2).size == 0) {
        println("牌局结束")
        return ArrayBuffer(-1)
      }

      var hit = new ArrayBuffer[Int]()

      if (hit.size > 0) { return hit }

      //以地主报单方式组牌
      ai.opt = commbDS(ai.opt.flatMap(_._2).flatMap(x => x).toBuffer, ai.jpq)

      if (ai.opt.flatMap(_._2).size == 0) {
        println("牌局结束")
        return ArrayBuffer(-1)
      }

      val px = judgeTp(ai.t)

      //a)	若自己是地主上家
      if (ai.r == 1) {

        //i.	若X是地主打出，则ii，否则v
        if (ai.h == 0) {
          //ii.	若手中有大于X的冲锋套，则打出其中标签最小的一套
          if (getCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0
            && getCFT(ai).get(px).get.filter(x => x.size == ai.t.size
              && x.head > ai.t.head).size > 0) {
            var fg = true
            for (x <- getCFT(ai).get(px).getOrElse(ArrayBuffer()) if fg) {

              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          } else {

            //iii.	进入能跑必管逻辑，若最终跳出
            val mhit = run(ai)

            if (mhit.size != 0) {
              return mhit
            } else {
              //iv.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌

              hit = getBDTX(px, ai)

              return hit
            }
          }
        } else {
          //对家农民打出
          if (px == 11) {
            //vii.	若X是场外最大的单牌
            if (ai.t.head >= ai.jpq.max) {
              return hit
            } else {
              //viii.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌
              hit = ArrayBuffer(ai.opt.flatMap(_._2).flatMap(x => x).filter(_ > ai.t.head).toBuffer.sortWith(_ > _).head)
              val ab = ArrayBuffer[Int]()

              for (x <- ai.opt.flatMap(_._2).flatMap(x => x)) {
                ab += x
              }
              ab -= hit.head
              ai.opt = commb(ab, ai.jpq)
              return hit
            }
          }
        }
      } else {
        //i.	若X是地主打出，则iii，否则
        if (ai.h == 0) {

          //iii.	若手中有大于X的冲锋套，则打出其中标签最小的一套，否则
          if (getCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0
            && getCFT(ai).get(px).get.filter(x => x.size == ai.t.size
              && x.head > ai.t.head).size > 0) {
            var fg = true
            for (x <- getCFT(ai).get(px).getOrElse(ArrayBuffer()) if fg) {

              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          } else {
            //iv.	进入能跑必管逻辑，若最终跳出，则v

            val mhit = run(ai)

            if (mhit.size != 0) {
              return mhit
            } else {
              //iv.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌

              hit = getBDTX(px, ai)

              return hit
            }
          }
        }
      }

      hit
    } catch {
      case t: Throwable =>
           ArrayBuffer()
    }
  }

  /**
   * 面对对地主报双，农民主动出牌
   */

  def activeFaceLandlordCallTwo(ai: AI): ArrayBuffer[Int] = {
    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完")
      return hit
    }

    try {
      //a)	若手中有非单非对非炸弹非火箭的套牌，则打出其中长度最长标签最小的套牌，否则
      if (ai.opt.filter(x => x._1 != 1 && x._1 != 4 && x._1 != 11 && x._1 != 2).flatMap(_._2).size > 0) {
        hit = ai.opt.filter(x => x._1 != 1 && x._1 != 4 && x._1 != 11 && x._1 != 2).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      } else {

        //b)	若手中有炸弹（非火箭），且单牌套数>=2且非冲锋套的单牌套数>=1，则打出标签最小的四带二（单）
        if ((ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) && getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size >= 2) {
          val p4 = ai.opt.get(4).get.head
          val p1 = ai.opt.get(11).get.take(2)
          hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
          ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
          ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
          ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
        } else {

          //c)	若手中有炸弹（非火箭），且对牌套数>=2且非冲锋套的对牌套数>=1，则打出标签最小的四带二（对）
          if ((ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) && getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size >= 2) {
            val p4 = ai.opt.get(4).get.head
            val p1 = ai.opt.get(2).get.take(2)
            hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
            ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
            ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
            ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
          } else {

            //d)	若手中有全场唯一最大的单牌或炸弹（火箭），且手中有其余非冲锋套的单牌，则打出非冲锋套的单牌中标签最大的一套，否则

            val hasMaxS = ai.opt.get(11).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(11).get.last.head > ai.jpq.max
            val hasRock = ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
            var hasMaxB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 < ai.opt.get(4).get.last.head

            if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0) {
              hasMaxB = true
            }

            val hasNCFTS = getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0
            if (hasNCFTS && (hasMaxS || hasRock || hasMaxB)) {
              hit = getNCFT(ai).get(11).get.toBuffer.sortWith((x, y) => x.head > y.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {

              //e)	若场外存在单王
              val w1 = ai.jpq.contains(99)
              val w2 = ai.jpq.contains(100)
              val w3 = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100))
              if ((w1 || w2) && !w3) {
                //f)	若手中有对牌，则打出其中标签最大的一套，否则打出手牌中标签最大的一套
                if (ai.opt.get(2).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(2).get.last
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else {
                  hit = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                }
              } else {

                //g)	若手中非冲锋套非炸弹（火箭）的套数<=1，则h）否则i）
                if (getNCNum(ai) <= 1) {
                  //h)	若手中有冲锋套，则打出其中标签最小的一套，否则打出手牌中标签最大的一套
                  if (getCNum(ai) > 0) {
                    hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  } else {

                    hit = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  }
                } else {

                  //i)	若手中有全场唯一最大的单牌或炸弹（火箭），且手中有其余非冲锋套非炸弹（火箭）的套牌，则不考虑冲锋套、炸弹外的其余牌张组牌，打出其中标签最大的一张，否则
                  if ((ai.opt.get(11).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(11).get.last.head > ai.jpq.max)
                    || (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 || (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
                      && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 < ai.opt.get(4).get.last.head))
                      && getNCNum(ai) > 0) {

                    if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0) {
                      hit = getNCFT(ai).get(11).get.last
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else if (getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size > 0) {

                      //拆最大非冲锋套对子
                      val d = getNCFT(ai).get(2).get.last
                      ai.opt.map { x => if (x._2.contains(d)) x._2 -= d }
                      hit = ArrayBuffer(d.head)
                      ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(ArrayBuffer()) += hit)

                    }
                  } else {
                    //j)	若手中有单牌，则打出其中标签最大的一套，否则
                    if (ai.opt.get(11).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(11).get.last
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else {

                      if (getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size > 0) {
                        val d = getNCFT(ai).get(2).get.last
                        ai.opt.map { x => if (x._2.contains(d)) x._2 -= d }
                        hit = ArrayBuffer(d.head)
                        ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(ArrayBuffer()) += hit)
                      } else {

                        //k)	打出手牌中标签最大的一套
                        hit = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  /**
   * 面对对地主报双，农民动出牌
   */
  def passiveFaceLandlordCallTwo(ai: AI): ArrayBuffer[Int] = {
    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌局结束")
      return ArrayBuffer(-1)
    }
    try {
      var hit = new ArrayBuffer[Int]()
      val px = judgeTp(ai.t)
      //a)	若X是地主打出，则b)，否则e）
      val outHasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100))
      val hasB = ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0

      if (ai.h == 0) {

        //b)	若手牌中大于X的同类套牌（非炸弹），则打出其中标签最大的一套，否则
        if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(px).get.filter(x => x.size == ai.t.size
            && x.head > ai.t.head).size > 0 && px != 4) {
          var fg = true
          for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head > y.head) if fg) {

            if (x.head > ai.t.head && x.size == ai.t.size) {
              hit = x
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              fg = false
            }
          }
        } else {

          //c)	若手牌中有炸弹，且场外没有火箭，则打出其中标签最小的一套炸弹，否则
          if (!outHasRock && hasB && px != 1) {
            if (ai.opt.get(4).get.filter(x => x.size == ai.t.size && x.head > ai.t.head).size > 0) {
              hit = ai.opt.get(4).get.filter(x => x.size == ai.t.size && x.head > ai.t.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              return hit
            }

          } else {
            //d)	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌Y，将所有Y记为集合M，
            //若在集合M中能够找出一个Y，满足打出Y后非冲锋套非炸弹（火箭）套数相比打出Y前未增加，则打出该Y（若能找出多个Y满足条件，则打出其中标签最大的Y），否则过牌
            if (getNCNum(ai) <= 1) {
              if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {

                hit = ai.opt.get(1).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            } else {
              hit = getBDTX(px, ai)
            }
          }
        }

      } else {
        //e)	若手牌中有大于X的同类套牌（非炸弹），且非冲锋套非炸弹（火箭）的套牌套数<=1，则打出其中标签最小的一套，否则
        if (getNCNum(ai) <= 1 && (px != 4 && ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(px).get.filter(x => x.size == ai.t.size
            && x.head > ai.t.head).size > 0)) {
          var fg = true
          for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

            if (x.head > ai.t.head && x.size == ai.t.size) {
              hit = x
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              fg = false
            }
          }
        } else {

          //f)	若手牌中有大于X的炸弹，且非冲锋套非炸弹（火箭）的套牌套数<=1，则打出其中标签最小的一套，否则
          if (px == 4 && getNCNum(ai) <= 1 && !outHasRock && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
            && ai.opt.get(px).get.filter(x => x.size == ai.t.size && x.head > ai.t.head).size > 0) {
            var fg = true
            for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            hit = ai.opt.get(1).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.flatMap(_._2).size == 1 && ai.opt.flatMap(_._2).head.head > ai.t.head) {
            hit = ai.opt.get(4).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else if (px != 4 && px != 1 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size - getNCNum(ai) >= 0) {
            hit = ai.opt.get(4).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit

          } else {

            //g)	若自己是地主上家，则h)，否则过牌
            if (ai.r == 1) {

              //h)	若手牌中有大于X的同类套牌Y，且X为单或对，且Y标签在（T，2）间，则打出其中标签最大的一套，否则过牌
              if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
                && ai.opt.get(px).get.filter(x => x.size == ai.t.size
                  && x.head > ai.t.head).size > 0) {
                var fg = true
                for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head > y.head) if x.head >= 10 && x.head <= 15 && fg) {

                  if (x.head > ai.t.head && x.size == ai.t.size) {
                    hit = x
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    fg = false
                  }
                }
              }
            } else {

              hit
            }
          }
        }
      }

      hit
    } catch {
      case t: Throwable =>
        ArrayBuffer()
    }
  }

  /**
   * 1.	地主下家农民面对上家农民报单
   */
  def activeDownFaceUpCallOne(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完")
      return hit
    }

    try {
      //a)	若AI套牌套数=1，则打出仅有的一套牌，否则b
      if (ai.opt.flatMap(_._2).size == 1) {
        hit = ai.opt.flatMap(_._2).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      } else {

        //找出场外最大炸弹标签
        val offSitBoomNum = ai.jpq.map((_, 1)).groupBy(_._1).map { x => (x._1, x._2.size) }.filter(x => x._2 == 4).size
        var offSitBoomV = 0
        //AI手中最大炸弹标签
        var aiMaxBoomV = -1

        if (offSitBoomNum > 0) {
          offSitBoomV = ai.jpq.map((_, 1)).groupBy(_._1).map { x => (x._1, x._2.size) }
            .filter(x => x._2 == 4)
            .keySet
            .toBuffer
            .sortWith(_ < _).last
        }

        if (ai.opt.filter(_._1 == 4).flatMap(_._2).size > 0) {
          aiMaxBoomV = ai.opt.filter(_._1 == 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).last.head
        }

        //b)	若手中没有火箭或场上最大炸弹，则d)
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0 && aiMaxBoomV < offSitBoomV) {
          //d)	不考虑组牌，打出手牌中最小的一张牌（gus:查看记牌器最小的牌x，如果手中组牌有现成的单y比x还小，打出y）
          //找出记牌器最小的牌
          val offSitMinOne = ai.jpq.min
          //找出当前最优组牌的最小单牌
          if (ai.opt.get(11).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(11).get.head.head < offSitMinOne) {
            hit = ai.opt.get(11).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            ai.zdnm = 1
          } else {

            //如果AI仅有一个场外最大炸弹和唯一一套非炸弹的套牌，则打出最大炸弹和唯一套牌
            if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && ai.opt.filter(x => x._1 != 1).flatMap(_._2).size == 1) {
              hit = ai.opt.get(1).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

            } else if (!ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 &&
              ai.opt.get(4).getOrElse(ArrayBuffer()).head.head > offSitBoomV) {
              hit = ai.opt.get(4).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

            } else {
              //拆牌打出最小
              hit = ArrayBuffer(ai.opt.flatMap(_._2).flatMap(x => x).min)
              val reAb = ArrayBuffer[Int]()
              for (x <- ai.opt.flatMap(_._2.flatMap(x => x))) {
                reAb += x
              }
              ai.opt = commb(reAb --= hit, ai.jpq)
              ai.zdnm = 1
            }
          }
        } else {
          //c)	除最大炸弹外的手牌不考虑组牌，若手牌中非最大炸弹的最小的一张牌在全场最小，则打出火箭或标签最大的炸弹，否则
          if (ai.opt.flatMap(_._2).flatMap(x => x).min < ai.jpq.min) {
            if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
              hit = ai.opt.get(1).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              hit = ai.opt.get(4).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else {
            //如果AI仅有一个场外最大炸弹和唯一一套非炸弹的套牌，则打出最大炸弹和唯一套牌
            if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && ai.opt.filter(x => x._1 != 1).flatMap(_._2).size == 1) {
              hit = ai.opt.get(1).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

            } else if (!ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 &&
              ai.opt.get(4).getOrElse(ArrayBuffer()).head.head > offSitBoomV) {
              hit = ai.opt.get(4).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

            } else {
              //拆牌打出最小
              hit = ArrayBuffer(ai.opt.flatMap(_._2).flatMap(x => x).min)
              val reAb = ArrayBuffer[Int]()
              for (x <- ai.opt.flatMap(_._2.flatMap(x => x))) {
                reAb += x
              }
              ai.opt = commb(reAb --= hit, ai.jpq)
              ai.zdnm = 1
            }
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
       
      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  /**
   * 地主下家农民面对地主上家农民报单被动牌打牌
   */
  def passiveDownFaceUpCallOne(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()
    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完")
      return hit
    }
    try {
      val outHasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100))
      val hasB = ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
      //a)	若手中有火箭或场上最大炸弹，且除最大炸弹外的手牌不考虑组牌，手牌中非最大炸弹的最小的一张牌在全场最小且唯一最小，则打出火箭或标签最大的炸弹(确保手中炸弹最大，确保打完炸后最小手牌比记牌器还小)
      val hasRock = ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
      var hasMaxB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 < ai.opt.get(4).get.last.head
      var hasMinS = (ai.opt.flatMap(_._2).toBuffer -- ai.opt.filter(_._1 == 4).flatMap(_._2).toBuffer).flatMap(x => x).min < ai.jpq.min
      val px = judgeTp(ai.t)
      //
      if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0) {

        hasMaxB = true

      }
      //仅有一套牌
      if (getCNum(ai) + getNCNum(ai) == 1) {

        hasMinS = true
      }

      if ((hasRock || hasMaxB) && hasMinS) {

        if (hasMaxB) {
          hit = ai.opt.get(4).get.head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else if (hasRock) {
          hit = ai.opt.get(1).get.head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        }
      } else {

        if (ai.h != 0) {
          return hit
        } else {
          if (px == 1 && ai.t.head <= ai.jpq.distinct.sortWith(_ < _).take(2).last && ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.min > ai.t.head) {
            return hit
          } else {

            if (px == 11) {
              if (ai.jpq.min > ai.t.head) {
                return hit
              }
            }

            //d)	若手牌中有大于X的同类套牌，则打出其中标签最大的一套，否则
            if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
              && ai.opt.get(px).get.filter(x => x.size == ai.t.size
                && x.head > ai.t.head).size > 0 && px != 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              //e)	若手牌中有大于X的炸弹或火箭，则打出其中标签最小的一套，否则不出

              if (px == 4) {
                if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(4).get.filter(x => x.size == ai.t.size && x.head > ai.t.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  ai.zdnm = 1
                } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(1).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  ai.zdnm = 1
                }
              }
            }
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
       ArrayBuffer()
    }
  }

  //8.	地主上家战斗农民
  def activeUpF(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完1")
      return hit
    }

    try {
      //a)	若非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则b) 否则f)
      if (getNCNum(ai)
        - (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size
          + getCFT(ai).get(2).getOrElse(ArrayBuffer()).size
          + getCFT(ai).get(4).getOrElse(ArrayBuffer()).size)
          + getCFT(ai).get(1).getOrElse(ArrayBuffer()).size <= 1) {

        //如果场外无炸无火箭，打完冲锋套再打非冲锋套
        if (getNCNum(ai) == 1 && getCNum(ai) > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0 && !ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100))) {
          val c = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          return c
        }

        //b)	若手中有非单非对非冲锋套的套牌，则打出其中标签最小的一套，否则
        if (getNCFT(ai).filter(x => x._1 != 11 && x._1 != 2).flatMap(_._2).size > 0) {

          //hit = getNCFT(ai).filter(x => x._1 != 11 && x._1 != 2).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head

          hit = getNCFT(ai).filter(x => x._1 != 11 && x._1 != 2).maxBy(x => x._2.size)._2.head

          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else {

          //c)	若手中非冲锋套非炸弹非火箭的套牌套数<=1，且手中有冲锋套，则打出其中标签最小的一套，否则
          if (getNCNum(ai) <= 1 && getCNum(ai) > 0) {
            hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {
            val hasB = ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
            val hasR = ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
            //d)	若手中非冲锋套非炸弹非火箭的套牌套数=1，且手中有炸弹（火箭），则打出非冲锋套非炸弹非火箭的套牌中标签最小的一套，否则
            if (getNCNum(ai) == 1 && (hasB || hasR)) {
              hit = getNCFT(ai).flatMap(_._2).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              //e)	打出手牌中套数最多的套牌种类中标签最小的一套
              if (getNCNum(ai) > 0) {

                //hit = getNCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head

                hit = getNCFT(ai).maxBy(x => x._2.size)._2.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                //特殊处理炸弹
                hit = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        }
      } else {

        //f)	若手中有在JQKA范围内的单牌，则打出其中标签最小的一套，否则
        if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0 && getNCFT(ai).get(11).get.filter(x => x.head >= 9 && x.head <= 13).size > 0) {
          hit = getNCFT(ai).get(11).get.filter(x => x.head >= 9 && x.head <= 13).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else {
          //g)	打出手牌中套数最多的套牌种类（非炸弹、非火箭、非冲锋套）中标签最小的一套
          //val px = judgeTp(getNCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head)
          //if (getNCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0) {

          //hit = getNCFT(ai).get(px).get.sortWith((x, y) => x.head < y.head).head
          hit = getNCFT(ai).maxBy(x => x._2.size)._2.head

          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          //}
        }
      }

      hit
    } catch {
      case t: Throwable =>
       
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  //上家战斗被动出牌
  def passiveUpF(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完2")
      return hit
    }
    val px = judgeTp(ai.t)

    try {
      //a)	若非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则b) 
      if (getNCNum(ai)
        - (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size
          + getCFT(ai).get(2).getOrElse(ArrayBuffer()).size
          + ai.opt.get(4).getOrElse(ArrayBuffer()).size
          + ai.opt.get(1).getOrElse(ArrayBuffer()).size) < 1) {

        //b)	若手牌中有大于X的同类套牌，则打出其中标签最小的一套，否则

        if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(px).get.filter(x => x.size == ai.t.size
            && x.head > ai.t.head).size > 0 && px != 4) {
          var fg = true
          for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

            if (x.head > ai.t.head && x.size == ai.t.size) {
              hit = x
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              fg = false
            }
          }
        } else {

          var hasMaxB =
            ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 &&
              ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 &&
              ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.last.head

          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size == 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0) {
            hasMaxB = true
          }

          //外面存在王炸，比你炸弹还大的炸弹，自己手中没有炸弹的时候，都强行拆牌去打，套数最多增加一套
          if ((ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.dz >= 2) ||
            (hasMaxB && ai.dz >= 4) ||
            (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0 &&
              ai.opt.get(4).getOrElse(ArrayBuffer()).size == 0)) {

            val bd = getBDTX(px, ai)

            if (bd.size > 0) {
              return bd
            }
          }

          //c)	若手牌中有火箭或炸弹（gus：如果场外打出的不是炸弹，直接炸，否则打出AI手牌大于场外炸弹的最小炸弹），则打出其中标签最小的一套，否则不出
          if (px != 4 && px != 1 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {

            if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
              hit = ai.opt.get(1).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else {
            //场外打出的是炸弹
            if (px == 4 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {
              if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(4).get.sortWith((x, y) => x.head < y.head).last.head > ai.t.head) {
                hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                hit = ai.opt.get(1).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        }
      } else {
        //d)	若X为单牌，且手牌中有大于X，且在JQKA范围内的单牌，则打出其中标签最小的一套，否则
        if (px == 11 && ai.opt.get(11).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(11).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).size > 0) {

          hit = ai.opt.get(11).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

        } else {
          //e)	若X为对牌，且手牌中有大于X，且在JQKA范围内的对牌，则打出其中标签最小的一套，否则
          if (px == 2 && ai.opt.get(2).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(2).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).size > 0) {
            hit = ai.opt.get(2).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {
            //f)	若X为地主打出，则h），否则
            if (ai.h == 0) {

              //h)	若X是标签为A的单牌，且手牌中有2，则打出标签为2的单牌，否则
              if (px == 11 && ai.t.head == 14 && ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.contains(15)) {

                hit = ArrayBuffer(15)
                val ab = ArrayBuffer[Int]()
                ai.opt.flatMap(_._2).flatMap(x => x).map(ab += _)
                ai.opt = commb(ab --= hit, ai.jpq)

              } else {
                //i)	若X是标签为A的对牌，且手牌中2的张数>=2张，则打出标签为2的对牌，否则
                if (px == 2 && ai.t.head == 14 && ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.sortWith(_ < _).containsSlice(ArrayBuffer(15, 15))) {
                  hit = ArrayBuffer(15, 15)
                  val ab = ArrayBuffer[Int]()
                  ai.opt.flatMap(_._2).flatMap(x => x).map(ab += _)
                  ai.opt = commb(ab --= hit, ai.jpq)
                } else {
                  //j)	若X是标签为2的单牌，且手牌中有小王，则打出小王，否则
                  if (px == 11 && ai.t.head == 15 && ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.contains(99)) {
                    hit = ArrayBuffer(99)
                    val ab = ArrayBuffer[Int]()
                    ai.opt.flatMap(_._2).flatMap(x => x).map(ab += _)
                    ai.opt = commb(ab --= hit, ai.jpq)
                  } else {
                    //k)	若X是小王，且手牌中有大王，则打出大王，否则
                    if (px == 11 && ai.t.head == 99 && ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.contains(100)) {
                      hit = ArrayBuffer(100)
                      val ab = ArrayBuffer[Int]()
                      ai.opt.flatMap(_._2).flatMap(x => x).map(ab += _)
                      ai.opt = commb(ab --= hit, ai.jpq)
                    } else {

                      //l)	若手牌中有大于X的同类套牌（非炸弹），则打出其中标签最小的一套，否则
                      if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
                        && ai.opt.get(px).get.filter(x => x.size == ai.t.size
                          && x.head > ai.t.head).size > 0 && px != 4) {
                        var fg = true
                        for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                          if (x.head > ai.t.head && x.size == ai.t.size) {
                            hit = x
                            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                            fg = false
                          }
                        }
                      } else {

                        //m
                        val ncn = getNCNum(ai) //非冲锋套

                        val cn = getCNum(ai) //冲锋套

                        val zn = ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size

                        if (ncn - cn - zn <= 1) {
                          if (px != 4 && px != 1 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {
                            if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                              hit = ai.opt.get(4).get.head
                              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                            } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                              hit = ai.opt.get(1).get.head
                              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                            }
                          } else {
                            //场外打出的是炸弹
                            if (px == 4 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {
                              if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(4).get.sortWith((x, y) => x.head < y.head).last.head > ai.t.head) {
                                hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
                                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                              } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                                hit = ai.opt.get(1).get.head
                                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                              }
                            }
                          }
                        } else {
                          //n)	遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出，否则
                          hit = getBDTX(px, ai)
                        }
                      }
                    }
                  }
                }
              }
            } else {
              //g)	若手牌中有>X的同类型非冲锋套牌，则打出其中标签最小的一套，否则过牌
              if (getNCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0
                && getNCFT(ai).get(px).get.filter(x => x.size == ai.t.size
                  && x.head > ai.t.head).size > 0 && px != 4) {
                var fg = true
                for (x <- getNCFT(ai).get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                  if (x.head > ai.t.head && x.size == ai.t.size) {
                    hit = x
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    fg = false
                  }
                }
              }
            }
          }
        }
      }

      hit
    } catch {
      case t: Throwable =>
     ArrayBuffer()
    }

  }

  /**
   * 地主上家垃圾农民主动出牌
   */
  def activeUpR(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完3")
      return hit
    }

    try {
      //a)	按现有组套，若手牌中有在JQKA范围内的单牌，则打出其中标签最小的一套，否则
      if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0 && getNCFT(ai).get(11).get.filter(x => x.head >= 9 && x.head <= 13 && x.head > ai.t.head).size > 0) {
        hit = getNCFT(ai).get(11).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      } else {
        //b)	打出手牌中长度最长的套牌中标签最小的一套
        val px = judgeTp(getNCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head)
        if (getNCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0) {
          hit = getNCFT(ai).get(px).get.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        }
      }

      hit
    } catch {
      case t: Throwable =>
       
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  /**
   * 地主上家垃圾农民被动出牌
   */
  def passiveUpR(ai: AI): ArrayBuffer[Int] = {

    try {
      var hit = new ArrayBuffer[Int]()

      if (ai.opt.flatMap(_._2).size == 0) {
        println("牌已出完4")
        return hit
      }
      val px = judgeTp(ai.t)

      //a)	若手牌中有大于X的同类套牌，则b)，否则c)
      if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
        && ai.opt.get(px).get.filter(x => x.size == ai.t.size
          && x.head > ai.t.head).size > 0) {
        //b)	若X为单牌，且手牌中有在JQKA范围内的单牌，则打出其中标签最小的一套，否则打出大于X的同类套牌标签最小的一套
        if (px == 11) {

          if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0 && getNCFT(ai).get(11).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).size > 0) {
            hit = getNCFT(ai).get(11).get.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {

            if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0 && getNCFT(ai).get(11).get.filter(x => x.head <= 10 && x.head > ai.t.head).size > 0) {
              hit = getNCFT(ai).get(11).get.filter(x => x.head <= 10 && x.head > ai.t.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          }
        } else {

          if (getNCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0
            && getNCFT(ai).get(px).get.filter(x => x.size == ai.t.size
              && x.head > ai.t.head).size > 0 && px != 4) {

            var fg = true
            for (x <- getNCFT(ai).get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          }
        }
      } else {

        //c)	若X为农民打出，则过牌，否则
        if (ai.h != 0) {

          return hit
        } else {
          //d)	不考虑组牌，若手牌中存在大于X的同类套牌A，则打出：非冲锋套非炸弹（火箭）的套牌套数【与原有非冲锋套非炸弹（火箭）的套牌套数相比】增加量最小且标签最小的一套，否则不出
         
          hit = getBDTX(px, ai)
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        }
      }
      hit

    } catch {
      case t: Throwable =>
       
        ArrayBuffer() // TODO: handle error
    }
  }

  //2.	地主上家农民面对地主下家农民报单 主动

  def activeUpFaceDownCallOne(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完")
      return hit
    }

    try {
      //a)	若自己的非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则进入地主上家战斗农民逻辑
      if (getNCNum(ai)
        - (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size
          + getCFT(ai).get(2).getOrElse(ArrayBuffer()).size
          + getCFT(ai).get(4).getOrElse(ArrayBuffer()).size)
          + getCFT(ai).get(1).getOrElse(ArrayBuffer()).size <= 1) {

        hit = activeUpF(ai)
      } else {

        //b)	若手牌中有非冲锋套的单牌，则打出其中标签最小的一套，否则
        //特别的：当地主上家农民打出手牌中（非火箭/炸弹标签）最小的一张且地主无出牌动作时，若报单的地主下家农民无出牌动作，则将地主下家农民标记为未报单农民，并将自己标记为战斗农民（直至此局结束）
        if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0) {
          hit = getNCFT(ai).get(11).get.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          ai.zdnm = 1
        } else {
          if (ai.zdnm == 1) {
            hit = activeUpF(ai)
          } else {
            hit = activeUpR(ai)
            //bug
            //hit = passiveUpR(ai)
          }
        }
      }

      hit
    } catch {
      case t: Throwable =>
     
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  def passiveUpFaceDownCallOne(ai: AI): ArrayBuffer[Int] = {

    var hit = new ArrayBuffer[Int]()

    if (ai.opt.flatMap(_._2).size == 0) {
      println("牌已出完")
      return hit
    }
    val px = judgeTp(ai.t)

    try {
      //a)	若手中仅剩一套牌，且大于X，则打出该套牌，否则
      if (ai.opt.flatMap(_._2).size == 1) {
        if (px == 4) {
          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(4).get.head.head > ai.t.head) {
            hit = ai.opt.get(4).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } else {
          if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
            && ai.opt.get(px).get.filter(x => x.size == ai.t.size
              && x.head > ai.t.head).size > 0 && px != 4) {
            var fg = true
            for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head > y.head) if fg) {

              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          }
        }
      } else {
        //b)	若X是农民打出，则不出，否则
        if (ai.h != 0) {
          return hit
        } else {

          //c)	若非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则进入地主上家战斗农民逻辑

          if (getNCNum(ai)
            - (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size
              + getCFT(ai).get(2).getOrElse(ArrayBuffer()).size
              + getCFT(ai).get(4).getOrElse(ArrayBuffer()).size)
              + getCFT(ai).get(1).getOrElse(ArrayBuffer()).size <= 1) {

            hit = passiveUpF(ai)
          } else {

            //d)	若手牌中有大于X的同类套牌，则打出其中标签最大的一套，否则
            if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
              && ai.opt.get(px).get.filter(x => x.size == ai.t.size
                && x.head > ai.t.head).size > 0 && px != 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              //e)	若手牌中有大于X的炸弹或火箭，则打出其中标签最小的一套，否则不出
              if (px == 4) {
                if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(4).get.filter(x => x.size == ai.t.size && x.head > ai.t.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  ai.zdnm = 1
                } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                  hit = ai.opt.get(1).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  ai.zdnm = 1
                }
              }
            }
          }
        }
      }

      hit
    } catch {
      case t: Throwable =>
      ArrayBuffer()
    }

  }

  //===========================================其刚=============================
  /**
   * 垃圾下家主动出牌
   */
  def activeDownR(ai: AI): ArrayBuffer[Int] = {
    var hit = new ArrayBuffer[Int]()
    try {
      //a)	若手中有非单非对的且标签<10的套牌，打出其中长度最长标签最小的一套
      val a = ai.opt.filter(x => x._1 != 11 && x._1 != 2).flatMap(_._2).filter(x => x.head < 10).toBuffer
      if (a.length > 0) {
        hit = a.sortWith((x, y) => x.size > y.size).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      } else {
        //b)	打出手牌中套数最多的套牌种类中标签最小的一套
        //hit = ai.opt.toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head
        hit = ai.opt.maxBy(x => x._2.size)._2.head

        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      }
      hit
    } catch {
      case t: Throwable =>
       
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  /**
   * 垃圾下家被动出牌
   */
  def passiveDownR(ai: AI): ArrayBuffer[Int] = {

    try {
      var hit = new ArrayBuffer[Int]()
      val px = judgeTp(ai.t)
      //a)	若X为农民打出或者飞机，则过牌，否则
      if (ai.h != 0 || px == 1) {
        return hit
      } else {
        //b)	在当前组牌方式下，若手牌中没有大于X的同类套牌，则d)，否则
        var s = ai.opt.get(judgeTp(ai.t)).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head)
        if (px == 12345 || px == 112233 || px == 331 || px == 332) {
          s = ai.opt.get(judgeTp(ai.t)).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head && x.length == ai.t.length)
        }
        if (s.size > 0) {
          //c)	若X为单牌，
          if (11 == judgeTp(ai.t)) {
            //且手牌中有大于X且在JQKA范围内的单牌，则打出其中标签最小的一套
            val top = s.filter(x => x.head >= 11 && x.head <= 14 && x.head > ai.t.head);
            if (top.size > 0) {
              hit = top.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {

              //否则打出大于X的同类套牌标签最小的一套
              hit = s.head;
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else {
            //打出大于X的同类套牌标签最小的一套
            hit = s.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } else {
          val s = ai.opt.flatMap(_._2).flatMap(x => x).filter(x => x > ai.t.head).toBuffer
          if (s.size > 0) {

            //遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出

            hit = getBDTX(px, ai)
            //ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
      ArrayBuffer()
    }
  }

  /**
   * 战斗下家主动出牌
   */
  def activeDownF(ai: AI): ArrayBuffer[Int] = {

    try {
      var hit = new ArrayBuffer[Int]()
      val ncn = getNCNum(ai) //非冲锋套
      val cft = getCFT(ai).filter(x => x._1 == 11 || x._1 == 2 || x._1 == 4 || x._1 == 1).flatMap(_._2).size

      if ((getNCNum(ai) == 1 && ai.opt.get(1).get.toBuffer.size > 0 && getCNum(ai) == 0) || (getNCNum(ai) == 0 && ai.opt.get(1).get.toBuffer.size > 0 && getCNum(ai) > 0)) {
        hit = ai.opt.get(1).get.toBuffer.sortWith((x, y) => x.head < y.head).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        return hit
      }

      //a)	若非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则b)，否则e)
      if (ncn - cft <= 1) {
        //b)	若手中有非单非对非冲锋套非炸弹非火箭的套牌，则打出其中标签最小的一套，否则

        if (ai.opt.filter(x => x._1 != 11 && x._1 != 2 && x._1 != 4 && x._1 != 1).flatMap(_._2).size > 0) {
          hit = ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4 && x._1 != 11).flatMap(_._2).toBuffer.sortWith((x, y) => x.size < y.size).last
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else if (ncn <= 1 && getCFT(ai).size > 0) {
          //c)	若手中非冲锋套非炸弹非火箭的套牌套数<=1，且手中有冲锋套，则打出其中标签最小的一套，否则
          hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else if (ncn == 1 && (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0)) {
          //d)	若手中非冲锋套非炸弹非火箭的套牌套数=1，且手中有炸弹（火箭），则打出非冲锋套非炸弹非火箭的套牌中标签最小的一套，否则
          hit = getNCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < x.head).head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

        } else {
          //e)	打出手牌中套数最多的套牌种类（非炸弹、非火箭、非冲锋套）中标签最小的一套
          if (getNCFT(ai).filter(x => x._2.size > 0).size > 0)
            hit = getNCFT(ai).filter(x => x._2.size > 0).toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head
          else
            hit = ai.opt.filter(x => x._2.size > 0).toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head

          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        }
      } else {
        //e)	打出手牌中套数最多的套牌种类（非炸弹、非火箭、非冲锋套）中标签最小的一套
        if (getNCFT(ai).filter(x => x._2.size > 0).size > 0)
          hit = getNCFT(ai).filter(x => x._2.size > 0).toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head
        else
          hit = ai.opt.filter(x => x._2.size > 0).toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head
        //hit = ai.opt.filter(x => x._2.size > 0).toBuffer.sortWith((x, y) => x._2.size > y._2.size).head._2.toBuffer.sortWith((x, y) => x.head < y.head).head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
      }
      hit
    } catch {
      case t: Throwable =>
       
        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == 0) {
          //没炸弹直接打最小标签套牌
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(1).getOrElse(ArrayBuffer()).size == ai.opt.flatMap(_._2).size) {
          //仅有炸弹直接打最小标签套牌炸弹
          val c = ai.opt.flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        } else {
          //先打普通套牌
          val c = ai.opt.filter(x => x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
          ai.opt.map { x => if (x._2.contains(c)) x._2 -= c }
          c
        }
    }
  }

  /**
   * 战斗下家被动出牌
   */
  def passiveDownF(ai: AI): ArrayBuffer[Int] = {

    try {
      var hit = new ArrayBuffer[Int]()
      val ncn = getNCNum(ai) //非冲锋套
      val cn = getCNum(ai) //冲锋套
      val zn = ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size
      val px = judgeTp(ai.t)

      if (px == 1) { return ArrayBuffer() }

      val cft = getCFT(ai).filter(x => x._1 == 11 || x._1 == 2 || x._1 == 4 || x._1 == 1).flatMap(_._2).size
      //如果对方是火箭直接跳过
      if (px == 1)
        hit
      if (ncn - cft <= 1) {
        //b)	若手牌中有大于X的同类套牌，则打出其中标签最小的一套，否则
        var s = ai.opt.get(judgeTp(ai.t)).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head)
        if (px == 12345 || px == 112233 || px == 331 || px == 332) {
          s = ai.opt.get(judgeTp(ai.t)).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head && x.length == ai.t.length)
        }
        if (s.size > 0) {

          hit = s.head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {

          val hasMaxB =
            ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 &&
              ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 &&
              ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.last.head

          if ((ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.dz >= 2) || hasMaxB) {
            hit = getBDTX(px, ai)

            if (hit.size > 0) {
              return hit
            }
          }
          //c)	若手牌中有火箭或炸弹，则打出其中标签最小的一套，否则不出
          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
            if (px == 4)
              hit = ai.opt.get(4).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
            else
              hit = ai.opt.get(4).getOrElse(ArrayBuffer()).sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
            hit = ai.opt.get(1).getOrElse(ArrayBuffer()).sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } else {
          val s = ai.opt.flatMap(_._2).flatMap(x => x).filter(x => x > ai.t.head).toBuffer
          if (s.size > 0) {
            //遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出
            hit = getBDTX(px, ai)
            //ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        }
      } else {
        //d)	若X为单牌或对牌，且手牌中有大于X的非冲锋同类套牌，则打出其中标签最小的一套，否则
        val fl = getNCFT(ai).get(px).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head)
        if ((11 == px || 2 == px) && fl.size > 0) {
          if (fl.size > 0) {
            hit = fl.sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }

        } else if (ai.h != 0) { //e)	若X为农民打出，则不出，否则
          hit
        } else if (ai.opt.get(px).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head).size > 0 && px != 4) { //f)	若手牌中有大于X的同类套牌（非炸弹），则打出其中标签最小的一套，否则
          var fl = ai.opt.get(px).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head)
          if (px == 12345 || px == 112233 || px == 331 || px == 332) {
            fl = ai.opt.get(px).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head && x.length == ai.t.length)
          }
          if (fl.size > 0) {
            hit = fl.sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ncn - cn - zn <= 1) {
          //g)	若手牌中有火箭或炸弹，则打出其中标签最小的一套，否则
          //          if (ai.opt.get(4).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head).size > 0) {
          //            val fl = ai.opt.get(4).getOrElse(ArrayBuffer()).filter(x => x.head > ai.t.head)
          //            if (fl.size > 0) {
          //              hit = fl.sortWith((x, y) => x.head < y.head).head
          //              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          //            }
          //          } else {
          //            hit = ai.opt.get(1).getOrElse(ArrayBuffer()).head
          //            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          //          }
          //        } 

          if (px != 4 && px != 1 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {
            if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
              hit = ai.opt.get(1).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else {
            //场外打出的是炸弹
            if (px == 4 && (ai.opt.get(1).getOrElse(ArrayBuffer()).size + ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0)) {
              if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.get(4).get.sortWith((x, y) => x.head < y.head).last.head > ai.t.head) {
                hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).sortWith((x, y) => x.head < y.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                hit = ai.opt.get(1).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        } else {
          val s = ai.opt.flatMap(_._2).flatMap(x => x).filter(x => x > ai.t.head).toBuffer
          if (s.size > 0) {
            //遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出

            hit = getBDTX(px, ai)
            //ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        }

      }
      hit
    } catch {
      case t: Throwable =>
      
        ArrayBuffer() // TODO: handle error
    }

  }

  def activeNm(ai: AI): ArrayBuffer[Int] = {

    if (ai.n1 == 0 || ai.n2 == 0 || ai.dz == 0) {
      println("牌局结束")
      return ArrayBuffer(-1)
    }

    var hit = ArrayBuffer[Int]()
    if (ai.opt.flatMap(_._2).size == 2) {

      val hasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.dz >= 2
      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1) {
        hit = ai.opt.get(1).get.head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        return hit
      } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size >= 1) {
        var hasMaxB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 < ai.opt.get(4).get.last.head

        if (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0) {
          hasMaxB = true
        }

        if (hasMaxB && !hasRock) {
          hit = ai.opt.get(4).get.last
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          return hit
        } else if (getCNum(ai) >= 1) {
          val hasB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
          if (!hasRock && !hasB) {
            hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          }
        }
      }
    }

    if (isGF(ai) == 1 || ai.zdnm == 1) {
      ai.zdnm = 1
    } else {
      ai.zdnm = 0
    }

    if (ai.dz == 1) {
      //println("当地主处于报单状态时，进入被地主报单逻辑")
      //a)	当地主处于报单状态时，进入被地主报单逻辑
      return activeFaceLandlordCallOne(ai)
    } else if (ai.r == 2 && ai.n1 == 1) {
      //println("当地主下家农民面对上家农民报单时，进入地主下家农民面对上家农民报单逻辑")
      //b)	当地主下家农民面对上家农民报单时，进入地主下家农民面对上家农民报单逻辑
      return activeDownFaceUpCallOne(ai)
    } else if (ai.dz == 2) {
      //println("当地主处于报双状态时，进入被地主报双逻辑")
      //c)当地主处于报双状态时，进入被地主报双逻辑
      return activeFaceLandlordCallTwo(ai)
    } else if (ai.r == 1 && ai.n2 == 1) {
      //println("当地主上家农民面对下家农民报单时，进入地主上家农民面对下家农民报单逻辑")
      //d)	当地主上家农民面对下家农民报单时，进入地主上家农民面对下家农民报单逻辑
      return activeUpFaceDownCallOne(ai)
    } else if (ai.r == 2 && ai.zdnm == 1 && ai.h == 2) {
      //println("若在地主下家，且为战斗农民，进入地主下家战斗农民逻辑")
      //e)	若在地主下家，且为战斗农民，进入地主下家战斗农民逻辑，否则

      return activeDownF(ai)
    } else if (ai.r == 2 && ai.zdnm == 0 && ai.h == 2) {
      //println("若在地主下家，且为垃圾农民，进入地主下家垃圾农民逻辑")
      //f)	若在地主下家，且为垃圾农民，进入地主下家垃圾农民逻辑
      return activeDownR(ai)
    } else if (ai.r == 1 && ai.zdnm == 1 && ai.h == 1) {
      //println("若在地主上家，且为战斗农民，进入地主上家战斗农民逻辑")
      //g)	若在地主上家，且为战斗农民，进入地主上家战斗农民逻辑，否则
      return activeUpF(ai)
    } else if (ai.r == 1 && ai.zdnm == 0 && ai.h == 1) {
      //h)进入地主上家垃圾农民逻辑
      //println("进入地主上家垃圾农民逻辑")
      return activeUpR(ai)
    } else {
      throw new Exception("")
    }
  }

  def passiveNm(ai: AI): ArrayBuffer[Int] = {

    if (ai.n1 == 0 || ai.n2 == 0 || ai.dz == 0) {
      println("牌局结束")
      return ArrayBuffer(-1)
    }

    var hit = ArrayBuffer[Int]()

    val px = judgeTp(ai.t)

    if (px == 1) { return ArrayBuffer() }

    if (ai.opt.flatMap(_._2).size == 2) {
      val hasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.dz >= 2
      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1) {
        hit = ai.opt.get(1).get.head
        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        return hit
      } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size >= 1) {

        var hasMaxB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 < ai.opt.get(4).get.last.head

        if (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0) {
          hasMaxB = true
        }

        if (hasMaxB && !hasRock) {
          hit = ai.opt.get(4).get.last
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          return hit
        } else if (getCNum(ai) >= 1) {
          val hasB = ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
          if (!hasRock && !hasB) {

            if (getCFT(ai).get(px).getOrElse(ArrayBuffer()).size > 0 && getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).last.head > ai.t.head) {
            }
            hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).last
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          }
        }
      }
    }

    if (isGF(ai) == 1 || ai.zdnm == 1) {
      ai.zdnm = 1
    } else {
      ai.zdnm = 0
    }

    if (ai.dz == 1 && ai.r != 0) {
      //a)	当地主处于报单状态时，进入被地主报单逻辑
      //println("当地主处于报单状态时，进入被地主报单逻辑")
      return passiveFaceLandlordCallOne(ai)
    } else if (ai.r == 2 && ai.n1 == 1 && ai.r != 0) {
      //b)	当地主下家农民面对上家农民报单时，进入地主下家农民面对上家农民报单逻辑
      //println("当地主下家农民面对上家农民报单时，进入地主下家农民面对上家农民报单逻辑")
      return passiveDownFaceUpCallOne(ai)
    } else if (ai.dz == 2 && ai.r != 0) {
      //c)当地主处于报双状态时，进入被地主报双逻辑
      //println("当地主处于报双状态时，进入被地主报双逻辑")
      return passiveFaceLandlordCallTwo(ai)
    } else if (ai.r == 1 && ai.n2 == 1 && ai.r != 0) {
      //d)	当地主上家农民面对下家农民报单时，进入地主上家农民面对下家农民报单逻辑
      //println("当地主上家农民面对下家农民报单时，进入地主上家农民面对下家农民报单逻辑")
      return passiveUpFaceDownCallOne(ai)
    } else if (ai.r == 2 && ai.zdnm == 1 && ai.r != 0) {
      //e)	若在地主下家，且为战斗农民，进入地主下家战斗农民逻辑，否则
      //println("若在地主下家，且为战斗农民，进入地主下家战斗农民逻辑")
      return passiveDownF(ai)
    } else if (ai.r == 2 && ai.zdnm == 0 && ai.r != 0) {
      //f)	若在地主下家，且为垃圾农民，进入地主下家垃圾农民逻辑
      //println("若在地主下家，且为垃圾农民，进入地主下家垃圾农民逻辑")
      return passiveDownR(ai)
    } else if (ai.r == 1 && ai.zdnm == 1 && ai.r != 0) {
      //g)	若在地主上家，且为战斗农民，进入地主上家战斗农民逻辑，否则
      //println("若在地主上家，且为战斗农民，进入地主上家战斗农民逻辑")
      return passiveUpF(ai)
    } else if (ai.r == 1 && ai.zdnm == 0 && ai.r != 0) {
      return passiveUpR(ai)
    } else {
      throw new Exception("")
    }
  }
}

object MappNF {

  def main(args: Array[String]): Unit = {

    val ai = new AI
    val af = new AFun
    val nf = new NFun

    for (x <- 3 to 15) {
      ai.jpq += (x, x, x, x)
    }
    ai.jpq += (99, 100)

    //ai.jpq = ArrayBuffer(4,6,9,13,8,8,11,15,3,3,6,9,9,12,13,13,14)

    //角色 n1 地主下家农民 n2 地主上家农民 d1 地主

    val ggp = ArrayBuffer[Int]()

    //手牌
    var ab = ArrayBuffer[Int]()

    println("\n#######################################################################################\n输入示例:")
    println(s"1. AI获得17张牌 示例： f 3,4,5,6,3,15,3,4,9...  \n2. y 表示抢地主，并获得三张手牌  \n3. n+(1,2) 表示为地主上家或下家角色")
    println("4. 角色： n1-->地主上家农民  n2-->地主下家农民  d1-->地主\n5. 出牌样例：    例如输入： hit 4,5,6,7,8 n1 表示n1出了个顺子 45678  如果是n1,0 表示该角色过牌")
    println("#######################################################################################")

    //f 3,3,5,6,6,7,8,9,9,10,11,12,12,13,14,15,15
    while (true) {
      var content = StdIn.readLine()
      if (content.startsWith("f")) { //获得原始17张手牌
        ab.clear()
        for (x <- content.split(" ")(1).split(",")) {
          ab += x.toInt
        }
        if (ab.size != 17 || ab.min < 3 || (ab -- ArrayBuffer(99, 100)).max > 15 || ab.max > 100) {
          println("输入错误,检查牌数是否为17张,或者包含非法牌值")

        } else {
          //找出控制数
          val cnt = getKZS(ab)

          //叫地主：若总控制数>=1，则叫地主；否则不叫
          //抢地主：若总控制数>1.8，则抢地主；否则不抢
          //加倍：若总控制数>2.5,则加倍；否则不加倍
          println("控制数：" + cnt)
          if (cnt >= 1.6) {
            println("建议叫地主!")
          } else if (cnt > 2.1) {
            println("建议抢地主!")
          } else if (cnt > 2.6) {
            println("建议加倍!")
          } else {
            println("建议当农民!")
          }
          ai.jpq --= ab
          println("手牌：", ab.sortWith(_ > _))
        }
      } else if (content.startsWith("ggp")) {
        for (x <- content.split(" ")(1).split(",")) {
          ggp += x.toInt
        }
      } else if (content.startsWith("y")) {
        ai.dz = 20
        ai.n1 = 17
        ai.n2 = 17
        println("选择当地主")
        println(s"公共牌：$ggp")

        ab ++= ggp
        ai.jpq --= ggp
        println("当前手牌：", ab.sortWith(_ < _))
        var p = "开始计算最优组牌 :"

        new Thread(new Runnable() {
          def run() {
            var g = "> "
            print(p)
            for (x <- 1 to 100) {
              g = ">"
              Thread.sleep(10)
              print(g)
            }
            println()

            println("AI手牌：", ab.sortWith(_ > _))
            ai.r = 0
            //找出最优组牌
            ai.opt = commb(ab, ai.jpq)
            println("非冲锋套========>")
            getNCFT(ai).flatMap(_._2).foreach(println)
            println("冲锋套========>")
            getCFT(ai).flatMap(_._2).foreach(println)
            println("炸弹========>")
            ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
          }
        }).start()
      } else if (content.startsWith("n1")) {
        ai.dz = 20
        ai.n1 = 17
        ai.n2 = 17
        ai.r = 1
        println("AI作为地主上家农民")
        ai.opt = commb(ab, ai.jpq)
        println("非冲锋套========>")
        getNCFT(ai).flatMap(_._2).foreach(println)
        println("冲锋套========>")
        getCFT(ai).flatMap(_._2).foreach(println)
        println("炸弹========>")
        ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
      } else if (content.startsWith("n2")) {
        ai.dz = 20
        ai.n1 = 17
        ai.n2 = 17
        ai.r = 2
        println("AI作为地主下家农民")
        ai.opt = commb(ab, ai.jpq)
        println("非冲锋套========>")
        getNCFT(ai).flatMap(_._2).foreach(println)
        println("冲锋套========>")
        getCFT(ai).flatMap(_._2).foreach(println)
        println("炸弹========>")
        ai.opt.filter(x => x._1 == 1 || x._1 == 4).flatMap(_._2).foreach(println)
      } else if (content.startsWith("hit")) {
        ai.jpq --= content.split(" ")(1).split(",").map(_.toInt)
        if (content.split(" ")(2).equals("n1")) {
          ai.n1 -= content.split(" ")(1).split(",").length
          ai.h = 1
          println("地主上家农民出牌========>\t", "[" + content.split(" ")(1) + "]")
        } else if (content.split(" ")(2).equals("n2")) {
          ai.n2 -= content.split(" ")(1).split(",").length
          ai.h = 2
          println("地主下家农民出牌========>\t", "[" + content.split(" ")(1) + "]")
        } else if (content.split(" ")(2).equals("d1")) {
          ai.dz -= content.split(" ")(1).split(",").length
          ai.h = 0
          println("地主出牌========>\t", "[" + content.split(" ")(1) + "]")
        }
        ai.t.clear()
        for (x <- content.split(" ")(1).split(",")) {
          ai.t += x.toInt
        }
        //        println("上家农民手牌数：", ai.n1)
        //        println("下家农民手牌数：", ai.n2)
        //        println("地主手牌数：", ai.dz)
        //        println("记牌器：", ai.jpq)
        //        println("当前桌面牌：", ai.t)
      } else if (content.startsWith("1")) {
        println("地主主动出牌：", af.activeDz(ai: AI))
      } else if (content.startsWith("2")) {
        println("地主被动出牌：", af.passiveDz(ai: AI))
      } else if (content.startsWith("3")) {
        println("农民主动出牌：", nf.activeNm(ai: AI))
      } else if (content.startsWith("4")) {
        println("农民被动出牌：", nf.passiveNm(ai: AI))
      } else if (content.startsWith("9")) {
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
        println("战斗标记", ai.zdnm)
      }
    }
  }
}