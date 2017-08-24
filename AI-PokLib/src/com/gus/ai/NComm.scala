package com.gus.ai

import com.gus.ai.CommbRule._
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.collection.mutable.Buffer

object NComm {

 

  /**
   * 判断当前农民是为垃圾农民还是战斗农民
   */
  //每次轮到本AI动作时，若本AI非冲锋套非炸弹非火箭的套牌套数-（冲锋套单牌套数+冲锋套对牌套数+炸弹套数+火箭）<=1或总控制数>1，则为战斗农民，否则为垃圾农民

  //战斗农民 1  垃圾农民 0
  def isGF(ai: AI): Int = {
    val ts = getNCNum(ai)
    -getCFT(ai).filter(x => x._1 == 11 || x._1 == 2).flatMap(_._2).size
    -ai.opt.filter(x => x._1 == 1).flatMap(_._2).size - ai.opt.filter(x => x._1 == 4).flatMap(_._2).size
    val cn = ctrNumHitting(ai)
    
    if (ts <= 1 || cn >= 1) {
      1
    } else {
      0
    }
  }

  //三带优化
  def threeOptDS(ab: Buffer[Int], dqp: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]],
                 jpq: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {

    var dqp_c = dqp
    //剩余单牌
    val s = ab
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 == 1)
      .keySet
      .toBuffer
      .sortWith(_ < _)
    //剩余对子
    val d = ab.map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 == 2)
      .keySet
      .toBuffer
      .sortWith(_ < _) ++ dqp.get(2).getOrElse(ArrayBuffer()).flatMap(x => x)
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 == 2)
      .keySet
      .toBuffer
      .sortWith(_ < _)

    val sd1 = ArrayBuffer[ArrayBuffer[Int]]()
    //val sd2 = ArrayBuffer[ArrayBuffer[Int]]()

    val fly1 = ArrayBuffer[ArrayBuffer[Int]]()
    //val fly2 = ArrayBuffer[ArrayBuffer[Int]]()

    val f1 = getTwoSingAndNotCFT(s, jpq)
    //val f2 = getTwoDoubleAndNotCFT(d, jpq)

    //三带调优
    /**
     * 遍历标签小于10的单牌、对牌，若单牌和对牌套数相等，则带标签最小的；若两种套牌数量不等，则带套数多的一类中标签最小的。
     */

    val three = dqp_c.get(3).get

    for (x <- three if s.size > 0 && s.head < 10) {
      //单牌
      sd1 += three.head ++ ArrayBuffer(s.head)
      three -= three.head
      ab -= s.head
      s -= s.head
    }

    //如果不是冲锋套单
    for (x <- three if s.size > 0 && s.head >= 10) {
      if (!isCFT(s.head, jpq, 1)) {
        sd1 += three.head ++ ArrayBuffer(s.head)
        three -= three.head
        ab -= s.head
        s -= s.head
      }
    }

    /**
     * 遍历标签大于等于10的单牌、对牌，若单牌和对牌均为冲锋套
     * 若有顺子张数大于等于6张（双顺张数大于等于8张），则拆顺子最小牌（拆双顺最小对）作为被带牌张，否则，带标签最小的单牌、对牌冲锋套
     */

    dqp_c += 31 -> sd1
    //dqp_c += 32 -> sd2
    dqp_c
  }

  /**
   * 2)	地主报单农民组牌方式
   * a)	按以下两种顺序将每组牌分解为一些套的组合
   * i.	搜索顺子，顺子调优, 搜索炸弹，搜索三条，其他牌
   * ii.	搜索炸弹，搜索三条, 搜索顺子, 顺子调优, 其他牌
   * 三带规则：遍历单牌、对牌，优先带非冲锋套的单牌，否则带对
   * b)	根据以下顺序对比，记录最优组套方式及其中的套牌
   * i.	单牌最少
   * ii.	最小的单牌最大
   *
   */
  def commbDS(ab: Buffer[Int], jpq: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var cd = ab.clone
    var d1 = sequence(cd)
      .mergeMap(rocket(cd))
      .mergeMap(boom(cd))
      .mergeMap(three(cd))
      .mergeMap(doubleSequence(cd))
    var dqp1 = threeOptDS(cd, d1, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    cd = ab.clone()
    var d2 = rocket(cd)
      .mergeMap(boom(cd))
      .mergeMap(three(cd))
      .mergeMap(doubleSequence(cd))
      .mergeMap(sequence(cd))
    var dqp2 = threeOptDS(cd, d2, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    cd = ab.clone()
    var d3 = rocket(cd)
      .mergeMap(boom(cd))
      .mergeMap(three(cd))
      .mergeMap(sequence(cd))
      .mergeMap(doubleSequence(cd))
    var dqp3 = threeOptDS(cd, d3, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    var cnt1 = dqp1.get(11).getOrElse(ArrayBuffer()).size
    var cnt2 = dqp2.get(11).getOrElse(ArrayBuffer()).size
    var cnt3 = dqp3.get(11).getOrElse(ArrayBuffer()).size

    var dqp = TreeMap(cnt1 -> dqp1, cnt2 -> dqp2, cnt3 -> dqp3)
    dqp.ordering

    var opt = dqp.get(dqp.firstKey).get

    opt
  }

}