package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import com.gus.ai.CommbRule._
import scala.io.StdIn

class AFun {

  /**
   * 地主主动出牌
   *
   */
  def activeDz(ai: AI): ArrayBuffer[Int] = {

    try {

      if (ai.opt.flatMap(_._2).size == 0) {

        return ArrayBuffer(-1)
      }

      var hit = ArrayBuffer[Int]()

      if (ai.opt.flatMap(_._2).size == 2) {
        val hasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && (ai.n1 >= 2 || ai.n2 >= 2)

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

      //农民报单且报双
      if (ai.n1 + ai.n2 == 3) {

        //a)	若手中非冲锋套的套数<2  否则进入e)
        if (getNCNum(ai) < 2) {

          //b)	若手中有冲锋套，则打出标签最小的冲锋套
          if (getCNum(ai) > 0) {

            //hit = getCFT(ai).filter(x => x._2.size > 0).values.head.head
            hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

          } //c)	若手中有炸弹
          else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

            //d)	若大小王均未出现，则打出标签最小的非冲锋套非炸弹（火箭）套牌
            if (ai.jpq.containsSlice(ArrayBuffer(99, 100)) && getNCNum(ai) > 0) {

              if (ai.opt.get(4).getOrElse(ArrayBuffer()).size == 1 && ai.opt.get(11).getOrElse(ArrayBuffer()).size == 1) {

                val d1 = ArrayBuffer(ai.opt.get(11).get.head.head)
                val d2 = ai.opt.get(4).get.head
                hit = d2.take(3) ++ d1
                ai.opt.map { x => if (x._2.contains(d1)) x._2 -= d1 }
                ai.opt.map { x => if (x._2.contains(d2)) x._2 -= d2 }
                ai.opt = ai.opt.updated(11, ArrayBuffer(ArrayBuffer(d2.head)))

              } else {
                //hit = getNCFT(ai).filter(x => x._2.size > 0).values.head.head
                hit = getNCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }

            } //否则打出标签最小的炸弹
            else {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else {
            //打出仅有的一套
            hit = ai.opt.flatMap(_._2).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }
        } //e)	若手中非冲锋套的单牌套数>=2或者手中非冲锋套的对牌套数>=2   否则进入h)
        else {

          if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size >= 2
            || getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size >= 2) {
            //f)	若手中有（非火箭）炸弹，则进入g)否则进入h)
            if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
              //g)	若单牌套数<2，则炸弹带标签最小的两套对牌，否则炸弹带标签最小的两套单牌
              if (ai.opt.get(11).getOrElse(ArrayBuffer()).size < 2) {
                val p4 = ai.opt.get(4).get.head
                val p1 = ai.opt.get(2).get.take(2)
                hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
                ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
                ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
                ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
              } else {
                val p4 = ai.opt.get(4).get.head
                val p1 = ai.opt.get(11).get.take(2)
                hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
                ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
                ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
                ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
              }
            } else {

              //h)	若手中有非单、非对、非炸弹的冲锋套，则打出标签最小的（非单非对非炸弹）冲锋套，否则进入i)
              if (getCFT(ai).filter(x => x._1 != 11 && x._1 != 2 && x._1 != 4).flatMap(_._2).size > 0) {
                //hit = getCFT(ai).filter(x => x._2.size > 0).values.head.head
                hit = getCFT(ai).filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 2 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                //i)	若手牌中有非冲锋套的对牌，则打出其中标签最小的一套 
                if (ai.opt.get(2).getOrElse(ArrayBuffer(2)).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size == 0) {
                  hit = ai.opt.get(2).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else if (ai.opt.get(2).getOrElse(ArrayBuffer(2)).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                  val p4 = ai.opt.get(4).get.head
                  val p1 = ai.opt.get(2).get.take(1)
                  hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
                  ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
                  ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
                } else {
                  //j)	打出手中标签最大的（非冲锋套）单牌
                  hit = ai.opt.get(11).get.head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                }
              }
            }
          } else {
            //h)	若手中有非单、非对、非炸弹的冲锋套，则打出标签最小的（非单非对非炸弹）冲锋套，否则进入i)
            if (getCFT(ai).filter(x => x._1 != 11 && x._1 != 2 && x._1 != 4).flatMap(_._2).size > 0) {
              //hit = getCFT(ai).filter(x => x._2.size > 0).values.head.head
              hit = getCFT(ai).filter(x => x._2.size > 0 && x._1 != 11 && x._1 != 2 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              //i)	若手牌中有非冲锋套的对牌，则打出其中标签最小的一套 

              if (ai.opt.get(2).getOrElse(ArrayBuffer(2)).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size == 0) {
                hit = ai.opt.get(2).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else if (ai.opt.get(2).getOrElse(ArrayBuffer(2)).size > 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                val p4 = ai.opt.get(4).get.head
                val p1 = ai.opt.get(2).get.take(1)
                hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
                ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
                ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
              } else {
                //j)	打出手中标签最大的（非冲锋套）单牌
                hit = ai.opt.get(11).get.head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              }
            }
          }
        }
      } //任意农民报双逻辑,且组套完成后手牌中存在单牌或对牌时
      else if ((ai.n1 == 2 || ai.n2 == 2) && ai.opt.filter(x => x._1 == 11 || x._1 == 2).flatMap(_._2).size > 0) {

        //a)	若手中有单牌冲锋套，且手牌中还有其他单牌，则打出标签最小的单牌  否则进入b)
        if (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0 && getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size > 0) {
          if (getNCNum(ai) < 2 && !ai.jpq.containsSlice(ArrayBuffer(99, 100)) && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size == 0) {
            //hit = getCFT(ai).filter(x => x._2.size > 0).values.head.head
            hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {
            hit = getNCFT(ai).get(11).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          }

        } else {
          //b)	若手中有炸弹（非火箭），则进入c）否则进入d)；
          if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
            //c)	若对牌套数（非冲锋套）为2或3，则打出标签最小的四带二，否则进入d)；
            if (getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size == 2 || getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size == 3) {
              val p4 = ai.opt.get(4).get.head
              val p1 = ai.opt.get(2).get.take(2)
              hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
              ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
              ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
              ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
            } else {

              //d)	若非冲锋套的对牌套数>=2，则打出标签最大的非冲锋套的对牌，否则进入e)；
              if (getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size >= 2) {
                hit = getNCFT(ai).get(2).get.last
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {

                //e)	若手中有非对，非火箭，非炸弹的套牌，则打出其中标签最小的一套，否则进入f）
                if (ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4).flatMap(_._2).size > 0) {
                  //                hit = ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  //                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  //如果非冲锋套仅有一套，打冲锋套
                  if (getNCNum(ai) == 1 && getCFT(ai).size > 0) {
                    hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  } else {
                    hit = ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  }
                } else {
                  //f)	若手中有冲锋套，则打出手牌中标签最小的冲锋套，否则进入g)；
                  if (getCFT(ai).size > 0) {
                    //hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                    hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  } else {

                    //g)	若手中有炸弹（火箭），则进入h)，否则进入i）；
                    if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                      //h)	若外边存在火箭，或者存在比自己标签最小的炸弹还要大的炸弹，则进入i）
                      if ((ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)))
                        || (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
                          && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
                          && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.head.head)
                          && ai.n1 + ai.n2 >= 6) {

                        if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                          hit = ai.opt.get(4).get.head
                          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                        } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                          hit = ai.opt.get(1).get.head
                          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                        } else {
                          //i)	打出手中标签最大的对牌
                          hit = ai.opt.get(2).get.last
                          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                        }

                      } else {

                        if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                          hit = ai.opt.get(1).get.head
                          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                        } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                          hit = ai.opt.get(4).get.head
                          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                        }

                      }
                    } else {
                      if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(4).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(1).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else {
                        //i)	打出手中标签最大的对牌
                        hit = ai.opt.get(2).get.last
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }
                    }
                  }
                }
              }
            }
          } else {
            //d)	若非冲锋套的对牌套数>=2，则打出标签最大的非冲锋套的对牌，否则进入e)；
            if (getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size >= 2) {
              hit = getNCFT(ai).get(2).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              //e)	若手中有非对，非火箭，非炸弹的套牌，则打出其中标签最小的一套，否则进入f）
              if (ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4).flatMap(_._2).size > 0) {
                //如果非冲锋套仅有一套，打冲锋套
                if (getNCNum(ai) == 1 && getCFT(ai).size > 0) {
                  hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else {
                  hit = ai.opt.filter(x => x._1 != 2 && x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                }

              } else {

                //f)	若手中有冲锋套，则打出手牌中标签最小的冲锋套，否则进入g)；
                if (getCFT(ai).size > 0) {
                  //hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else {
                  //g)	若手中有炸弹（火箭），则进入h)，否则进入i）；
                  if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                    //h)	若外边存在火箭，或者存在比自己标签最小的炸弹还要大的炸弹，则进入i）
                    if ((ai.jpq.containsSlice(ArrayBuffer(99, 100)))
                      || (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
                        && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
                        && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.head.head) && ai.n1 + ai.n2 >= 6) {

                      if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(4).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(1).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else {
                        //i)	打出手中标签最大的对牌
                        hit = ai.opt.get(2).get.last
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }

                    } else {

                      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(1).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                      } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                        hit = ai.opt.get(4).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }

                    }
                  } else {
                    //i)	打出手中标签最大的对牌
                    if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(4).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(1).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else {
                      //i)	打出手中标签最大的对牌
                      hit = ai.opt.get(2).get.last
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    }
                  }
                }
              }
            }
          }
        }
      } //任意农民报单
      else if (ai.n1 == 1 || ai.n2 == 1) {
        /**
         * a)	若手中有炸弹（非火箭），则进入b）否则进入c)；
         * b)	若单牌套数（非冲锋套）为2或3，则打出标签最小的四带二，否则进入c)；
         * c)	若非冲锋套的单牌套数>=2，则打出标签最大的非冲锋套的单牌，否则进入d)；
         * d)	若手中有非单，非火箭，非炸弹的套牌，则打出其中标签最小的一套，否则进入e）；
         * e)	若手中有冲锋套，则打出手牌中标签最小的冲锋套，否则进入f)；
         * f)	若手中有炸弹或火箭，则进入g)，否则进入h）；
         * g)	若外边存在火箭，或者存在比自己标签最小的炸弹还要大的炸弹，则进入h）,否则打出标签最小的炸弹；
         * h)	打出手中标签最大的单牌
         *
         */

        if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
          //b)	若单牌套数（非冲锋套）为2或3，则打出标签最小的四带二，否则进入c)；
          if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size == 2 || getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size == 3) {
            val p4 = ai.opt.get(4).get.head
            val p1 = ai.opt.get(11).get.take(2)
            hit = ai.opt.get(4).get.head ++ p1.flatMap(x => x)
            ai.opt.map { x => if (x._2.contains(p4)) x._2 -= p4 }
            ai.opt.map { x => if (x._2.contains(p1.head)) x._2 -= p1.head }
            ai.opt.map { x => if (x._2.contains(p1.last)) x._2 -= p1.last }
          } else {
            //c)	若非冲锋套的单牌套数>=2，则打出标签最大的非冲锋套的单牌，否则进入d)；
            if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size >= 2) {
              hit = getNCFT(ai).get(11).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {

              // d)	若手中有非单，非火箭，非炸弹的套牌，则打出其中标签最小的一套，否则进入e）；
              if (ai.opt.filter(x => x._1 != 11 && x._1 != 1 && x._1 != 4).flatMap(_._2).size > 0) {
                hit = ai.opt.filter(x => x._1 != 11 && x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                //e)	若手中有冲锋套，则打出手牌中标签最小的冲锋套，否则进入f)；
                if (getCFT(ai).size > 0) {
                  //hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else {
                  //f)	若手中有炸弹或火箭，则进入g)，否则进入h）；
                  if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                    //g)	若外边存在火箭，或者存在比自己标签最小的炸弹还要大的炸弹，则进入h）,否则打出标签最小的炸弹；
                    if ((ai.jpq.containsSlice(ArrayBuffer(99, 100)) && ai.n1 + ai.n2 >= 3)
                      || (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
                        && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
                        && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.head.head)
                        && ai.n1 + ai.n2 >= 5) {

                      if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(4).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(1).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else {
                        //i)	打出手中标签最大的单牌
                        hit = ai.opt.get(11).get.last
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }

                    } else {
                      if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                        hit = ai.opt.get(1).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                      } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                        hit = ai.opt.get(4).get.head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      } else {
                        hit = ai.opt.flatMap(_._2).head
                        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                      }
                    }
                  } else {
                    if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(1).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                    } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                      hit = ai.opt.get(4).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else {
                      hit = ai.opt.flatMap(_._2).head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    }
                  }
                }
              }
            }
          }

        } else {

          //否则进入c)
          //c)	若非冲锋套的单牌套数>=2，则打出标签最大的非冲锋套的单牌，否则进入d)；
          if (getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size >= 2) {
            hit = getNCFT(ai).get(11).get.last
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {

            // d)	若手中有非单，非火箭，非炸弹的套牌，则打出其中标签最小的一套，否则进入e）；
            if (ai.opt.filter(x => x._1 != 11 && x._1 != 1 && x._1 != 4).flatMap(_._2).size > 0) {
              hit = ai.opt.filter(x => x._1 != 11 && x._1 != 1 && x._1 != 4).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            } else {
              //e)	若手中有冲锋套，则打出手牌中标签最小的冲锋套，否则进入f)；
              if (getCFT(ai).size > 0) {
                //hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size > y.size).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {

                //f)	若手中有炸弹或火箭，则进入g)，否则进入h）；
                if (ai.opt.get(1).getOrElse(ArrayBuffer()).size == 1 || ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                  //g)	若外边存在火箭，或者存在比自己标签最小的炸弹还要大的炸弹，则进入h）,否则打出标签最小的炸弹；
                  if ((ai.jpq.containsSlice(ArrayBuffer(99, 100)) && ai.n1 + ai.n2 >= 3)
                    || (ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).size > 0
                      && ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0
                      && ai.jpq.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 == 4).max._1 > ai.opt.get(4).get.head.head)
                      && ai.n1 + ai.n2 >= 5) {

                    if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(4).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(1).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else {
                      //i)	打出手中标签最大的单牌
                      hit = ai.opt.get(11).get.last
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    }

                  } else {
                    if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                      hit = ai.opt.get(1).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                    } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                      hit = ai.opt.get(4).get.head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    } else {
                      hit = ai.opt.flatMap(_._2).head
                      ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                    }
                  }
                } else {

                  if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0) {
                    hit = ai.opt.get(1).get.head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

                  } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0) {

                    hit = ai.opt.get(4).get.head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  } else {
                    hit = ai.opt.flatMap(_._2).head
                    ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  }
                }
              }
            }
          }
        }

      } //正常主动出牌
      /**
     *1)	若手中非冲锋套非炸弹（火箭）的套数<=1，且手中有冲锋套，则打出标签最小的冲锋套，否则 
			2)	若手中有长度>=3的非炸弹、且标签<=K的套牌、且此套牌标签大小不是场外最大，则打出其中长度最长标签最小的一套，否则
			3)	若手中有非冲锋套的单牌或非冲锋套的对牌，则打出其中套数最多标签最小的一套（若套数相等，打出标签最小的一套），否则
			4)	若手中有长度>=3的非炸弹（火箭）套牌，则打出其中长度最长标签最小的一套，否则
			5)	打出手牌中标签最小的一套牌
     * 
     */ else {

       //解决一些春天bug
        if (getNCNum(ai) == 1 && getCNum(ai) == 0 && ai.opt.get(4).getOrElse(ArrayBuffer()).length > 0) {

          //如果手上只剩炸弹，并且都时全场最大炸弹，先出炸弹
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

          var b = ai.opt.get(4).filter(x => x.filter(y => y.head > offSitBoomV).length > 0).size

          if (!(ai.jpq.contains(99) && ai.jpq.contains(100) && ai.n1 > 1 && ai.n2 > 1)) {
            if (ai.opt.get(4).getOrElse(ArrayBuffer()).length == b) {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              return hit
            }
          }
        }

        //1)	若手中非冲锋套非炸弹（火箭）的套数<=1，且手中有冲锋套，则打出标签最小的冲锋套，否则 
        if (getNCNum(ai) <= 1 && getCNum(ai) > 0) {

          //hit = getCFT(ai).filter(x => x._2.size > 0).values.head.head
          hit = getCFT(ai).filter(x => x._2.size > 0).flatMap(_._2).toBuffer.sortWith((x, y) => x.size < y.size).head
          //hit =  getCFT(ai).maxBy(x => x._2.size)._2.head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        } else {

          //2)	若手中有长度>=3的非炸弹、且标签<=K的套牌、且此套牌标签大小不是场外最大，则打出其中长度最长标签最小的一套，否则
          if (getNCNum(ai) > 0 && getNCFT(ai).flatMap(x => x._2)
            .filter(x => x.head <= 11 && x.size >= 3)
            .toBuffer.sortWith((x, y) => x.size > y.size).size > 0) {
            hit = getNCFT(ai).flatMap(x => x._2).toBuffer.sortWith((x, y) => x.size > y.size).head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {

            //3)	若手中有非冲锋套的单牌或非冲锋套的对牌，则打出其中套数最多标签最小的一套（若套数相等，打出标签最小的一套），否则
            if (getNCFT(ai).filter(x => x._1 == 11 || x._1 == 2)
              .map(_._2)
              .map(x => (x, x.size))
              .maxBy(x => x._2)._1.size > 0) {

              val s1 = ai.opt.filter(x => x._1 == 11).flatMap(_._2).size
              val d1 = ai.opt.filter(x => x._1 == 2).flatMap(_._2).size

              if (s1 != d1) {
                hit = ai.opt.filter(x => x._1 == 11 || x._1 == 2)
                  .map(_._2)
                  .map(x => (x, x.size))
                  .maxBy(x => x._2)
                  ._1
                  .minBy(x => x.head)
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                //如果相等，打出标签最小的
                if (ai.opt.filter(x => x._1 == 11).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head.head >
                  ai.opt.filter(x => x._1 == 2).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head.head) {
                  hit = ai.opt.filter(x => x._1 == 2).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                } else {
                  hit = ai.opt.filter(x => x._1 == 11).flatMap(_._2).toBuffer.sortWith((x, y) => x.head < y.head).head
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                }
              }
            } else {
              //	4)	若手中有长度>=3的非炸弹（火箭）套牌，则打出其中长度最长标签最小的一套，否则
              if (ai.opt.filter(x => x._1 == 11 || x._1 == 2)
                .map(_._2)
                .map(x => (x, x.size))
                .maxBy(x => x._2)._1.size > 0) {

                hit = ai.opt.filter(x => x._1 == 11 || x._1 == 2)
                  .map(_._2)
                  .map(x => (x, x.size))
                  .maxBy(x => x._2)
                  ._1
                  .minBy(x => x.head)
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              } else {
                hit = ai.opt.map(_._2).map(x => (x, x.size)).maxBy(x => x._2)._1.minBy(x => x.head)
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

  def passiveDz(ai: AI): ArrayBuffer[Int] = {

    try {
      if (ai.opt.flatMap(_._2).size == 0) {
        println("牌局已结束")
        return ArrayBuffer(-1)
      }

      var hit = ArrayBuffer[Int]()

      val px = judgeTp(ai.t)

      if (px == 1) { return ArrayBuffer() }

      if (ai.opt.flatMap(_._2).size == 2) {

        val hasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && (ai.n1 >= 2 || ai.n2 >= 2)
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
            if (px != 4) {
              hit = ai.opt.get(4).get.last
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
              return hit
            } else {
              if (ai.opt.get(4).get.filter(x => x.head > ai.t.head).length > 0) {
                hit = ai.opt.get(4).get.filter(x => x.head > ai.t.head).head
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                return hit
              }

            }

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

      val outHasRock = ai.jpq.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100))
      val hasB = ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0

      //王炸，要不起
      if (px == 1) {
        return ArrayBuffer()
      }

      if (ai.n1 + ai.n2 == 3) {
        //a)	若手中有大于X的同类冲锋套，则打出其中标签最小的一套，否则//顺子要做特殊处理，不光比牌形还要比长度
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
          //b)	若外边不存在火箭，且手中有炸弹（火箭），且打出炸弹（火箭）之后手中非冲锋套非炸弹（火箭）的套牌套数<=1，则打出标签最小的炸弹（火箭），
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
          } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            if (px == 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            hit = ai.opt.get(1).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else {
            //c)	遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出，否则
            hit = getBDTX(px, ai)
          }
        }
      } else if (ai.n1 == 2 || ai.n2 == 2) {

        //a)	若手中有大于X的同类冲锋套，则打出其中标签最小的一套，否则
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

          //b)	若外边不存在火箭，且手中有炸弹（火箭），且不存在比自己标签最小的炸弹还要大的炸弹，且打出炸弹（火箭）之后手中非冲锋套非炸弹（火箭）的套牌套数<=1，则打出标签最小的炸弹（火箭）
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
          } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            if (px == 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            hit = ai.opt.get(1).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else {

            //c)	若手中有大于X的同类套牌，则打出其中标签最大的一套，否则
            if (ai.opt.filter(_._1 == px).flatMap(_._2).size > 0 && ai.t.head < ai.opt.get(px).getOrElse(ArrayBuffer()).last.head) {

              hit = ai.opt.get(px).get.filter(x => x.head > ai.t.head && x.length == ai.t.length).sortWith((x, y) => x.head < y.head).head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }

            } else {
              //d)	遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出，否则
              hit = getBDTX(px, ai)
            }
          }
        }
      } else if (ai.n1 == 1 || ai.n2 == 1) {
        //     
        //      //又且仅有炸弹
        //      if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && ai.opt.flatMap(_._2).size == ai.opt.get(4).getOrElse(ArrayBuffer()).size) {
        //        hit = ai.opt.get(4).get.head
        //        ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        //        return hit
        //      }

        //a)	若手中有大于X的同类冲锋套，则打出其中标签最小的一套，否则
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

          //b)	若外边不存在火箭，且手中有炸弹（火箭），且不存在比自己标签最小的炸弹还要大的炸弹，且打出炸弹（火箭）之后手中非冲锋套非炸弹（火箭）的套牌套数<=1，则打出标签最小的炸弹（火箭），
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
          } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            if (px == 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            hit = ai.opt.get(1).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else {
            //c)	遍历所有手牌，若不能在手牌中找到大于X的同类套牌A，则不出，否则

            hit = getBDTX(px, ai)
          }
        }
      } else {

       
        //1)	按正常拆分，若手中没有比X大的同类型套牌，则进入4)，否则
        if (ai.opt.get(px).getOrElse(ArrayBuffer()).size > 0
          && ai.opt.get(px).get.filter(x => x.size == ai.t.size
            && x.head > ai.t.head).size > 0 && px != 4) {

          if (getNCNum(ai)
            - (getCFT(ai).get(11).getOrElse(ArrayBuffer()).size
              + getCFT(ai).get(2).getOrElse(ArrayBuffer()).size
              + getCFT(ai).get(1).getOrElse(ArrayBuffer()).size)
              + getCFT(ai).get(4).getOrElse(ArrayBuffer()).size <= 1) {

            //打出最大的一套
            for (x <- ai.opt.get(px).getOrElse(ArrayBuffer())) {
              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
              }
            }
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          } else {
            //打出最小的一套
            var fg = true
            for (x <- ai.opt.get(px).getOrElse(ArrayBuffer()) if fg) {
              if (x.head > ai.t.head && x.size == ai.t.size) {
                hit = x
                ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                fg = false
              }
            }
          }

        } else {

          //4)	若手牌中有炸弹，且非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1，则打出标签最小的一套炸弹
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
          } else if (ai.opt.get(4).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {

            if (px == 4) {
              var fg = true
              for (x <- ai.opt.get(px).get.sortWith((x, y) => x.head < y.head) if fg) {

                if (x.head > ai.t.head && x.size == ai.t.size) {
                  hit = x
                  ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
                  fg = false
                }
              }
            } else {
              hit = ai.opt.get(4).get.head
              ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            }
          } else if (ai.opt.get(1).getOrElse(ArrayBuffer()).size > 0 && getNCNum(ai) <= 1) {
            hit = ai.opt.get(1).get.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          } else {
           
            hit = getBDTX(px, ai)
          }
        }
      }
      hit
    } catch {
      case t: Throwable =>
        ArrayBuffer()
    }

  }
}

object MappAF {

  def main(args: Array[String]): Unit = {

    val ai = new AI
    val af = new AFun

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
    println(s"1. AI获得17张牌 示例： g 3,4,5,6,3,15,3,4,9...  \n2. y 表示抢地主，并获得三张手牌  \n3. nm 表示不抢地主")
    println("4. 角色： n1-->q上家农民  n2-->下家农民  d1-->地主\n5. 出牌样例：    例如输入： hit 4,5,6,7,8 n1 表示n1出了个顺子 45678  如果是n1,0 表示该角色过牌")
    println("#######################################################################################")

    while (true) {
      var content = StdIn.readLine()
      if (content.startsWith("f")) { //获得原始17张手牌
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
        println("选择当地主")
        println("获得三张牌：$ggp")

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
            //找出最优组牌
            ai.opt = commb(ab, ai.jpq)
            println("最优化组牌：", ai.opt)
          }
        }).start()
      } else if (content.startsWith("hit")) {

        ai.jpq --= content.split(" ")(1).split(",").map(_.toInt)
        if (content.split(" ")(2).equals("n1")) {
          ai.n1 -= content.split(" ")(1).split(",").length
        } else if (content.split(" ")(2).equals("n2")) {
          ai.n2 -= content.split(" ")(1).split(",").length

        } else if (content.split(" ")(2).equals("d1")) {
          ai.dz -= content.split(" ")(1).split(",").length
        }
        ai.t.clear()
        for (x <- content.split(" ")(1).split(",")) {
          ai.t += x.toInt
        }
        println("上家农民手牌数：", ai.n1)
        println("下家农民手牌数：", ai.n2)
        println("地主手牌数：", ai.dz)
        println("记牌器：", ai.jpq)
        println("当前桌面牌：", ai.t)
      } else if (content.startsWith("1")) {
        println(af.activeDz(ai: AI))
      } else if (content.startsWith("2")) {
        println(af.passiveDz(ai: AI))
      } else if (content.startsWith("9")) {
        println("当前手牌：", ai.opt)
        println("记牌器：", ai.jpq)
        println("上家农民：", ai.n1)
        println("下家农民：", ai.n2)
        println("地主：", ai.dz)
      }
    }
  }
}