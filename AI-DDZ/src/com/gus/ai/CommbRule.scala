package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.collection.mutable.Buffer
import java.util.HashMap
import util.control.Breaks._

object CommbRule {

  implicit class TreeMapImprovement(var fatherMap: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]) {
    def mergeMap(childMap: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
      for (x <- childMap) {
        if (fatherMap.contains(x._1)) {
          fatherMap += x._1 -> (fatherMap.get(x._1).get ++ childMap.get(x._1).get)

        } else {
          fatherMap += x._1 -> childMap.get(x._1).get
        }
      }
      fatherMap
    }
  }

  /**
   * 叫地主：若总控制数>=1.6，则叫地主；否则不叫
   * 抢地主：若总控制数>2.1，则抢地主；否则不抢
   * 加倍：若总控制数>2.6,则加倍；否则不加倍
   *
   */
  def getKZS(ab: ArrayBuffer[Int]): Double = {

    var cnt = 0.0
    val tmp = ab.map((_, 1)).groupBy(_._1).map { x => (x._1, x._2.size) }
    if (tmp.filter(_._1 >= 99).size == 2) {
      cnt += 2.0
    } else if (tmp.filter(_._1 == 100).size == 1) {
      cnt += 1.0
    } else if (tmp.filter(_._1 == 99).size == 1) {
      cnt += 0.6
    }
    cnt += tmp.filter(_._1 == 15).getOrElse(15, 0) / 2.0
    cnt += tmp.filter(_._2 == 4).size
    cnt
  }

  /**
   * 1)	套牌张数大于敌方手牌数最大值的非炸弹套牌
   * 2)	绝对大的单、对
   * 3)	飞机、飞机带翅膀、连对
   * 4)	主干三张牌绝对大的三带一、三带二
   * 5)	剩余牌管不住的顺子
   *
   */
  def getCFT(ai: AI): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var tm = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    var ab = ArrayBuffer[ArrayBuffer[Int]]()

    for (x <- ai.opt) {

      for (y <- x._2) {

        if ((ai.r == 0 && y.size > ai.n1 && y.size > ai.n2 && x._1 != 11 && x._1 != 4) || (ai.r != 0 && y.size > ai.dz && x._1 != 11 && x._1 != 4)) {
          tm = tm.mergeMap(TreeMap(x._1 ->
            ArrayBuffer(y)))
        } else if (x._1 == 11 && y.filter(x => x >= ai.jpq.max).size > 0) {
          tm = tm.mergeMap(TreeMap(x._1 ->
            ArrayBuffer(
              y.filter(x => x >= ai.jpq.max))))
        } else if (x._1 == 2) {
          //找出记牌器中可能的最大对子，和当前手牌比较
          val dz = ai.jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 2)
          //如果没有对子则把所有对子都当冲锋套,并且记牌器中的牌大于2张
          if (dz.size > 0 && ai.jpq.size > 2) {
            if (y.filter(x => x >= dz
              .max._1).size > 0) {
              tm = tm.mergeMap(TreeMap(x._1 ->
                ArrayBuffer(y.filter(x => x >= dz
                  .max._1))))
            }
          } else {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }

        } //x._1 == 112233 || x._1 == 33 || x._1 == 331 || x._1 == 332 
        else if (x._1 == 112233) {
          if (y.head > 10) {
            val ld = ai.jpq.map((_, 1))
              .groupBy(_._1)
              .map { x => (x._1, x._2.size) }
              .filter(x => x._2 >= 2).keySet.toBuffer.sortWith(_ < _)

            val s = y.size / 2
            val v = y.head

            val f = ld.filter(x => x > v).sliding(s, 1).filter(x => x.last - x.head == s - 1).length

            if (f == 0) {
              //飞机、飞机带翅膀、连对
              tm = tm.mergeMap(TreeMap(x._1 ->
                ArrayBuffer(y)))
            }
          }
        } else if (x._1 == 33 || x._1 == 331 || x._1 == 332) {
          if (y.head > 10) {
            val ld = ai.jpq.map((_, 1))
              .groupBy(_._1)
              .map { x => (x._1, x._2.size) }
              .filter(x => x._2 >= 3).keySet.toBuffer.sortWith(_ < _)
            val s = y.distinct.size / 2
            val v = y.head
            val f = ld.filter(x => x > v).sliding(s, 1).filter(x => x.last - x.head == s - 1).length

            if (f == 0) {
              //飞机、飞机带翅膀、连对
              tm = tm.mergeMap(TreeMap(x._1 ->
                ArrayBuffer(y)))
            }
          }
        } else if (x._1 == 3) {
          val dz = ai.jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)

          if (dz.size > 0 && y.head > dz.keySet.max && ai.jpq.size > 3) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y.filter(x => x >= dz
                .max._1))))
          } else if (dz.size == 0) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 31) {
          val dz = ai.jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)
          if (dz.size > 0 && y.head > dz.keySet.max && ai.jpq.size > 3) {

            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          } else if (dz.size == 0) {

            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 32) {
          val dz = ai.jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)
          if (dz.size > 0 && y.head >= dz.keySet.max && ai.jpq.size > 3) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          } else if (dz.size == 0) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 12345) {
          //从剩余牌中组一个顺子，组不了则表示该顺子最大
          if (!findSequenceByJPQ(y, ai.jpq)) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        }
      }
    }

    var s = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    for (x <- tm) {
      for (y <- x._2) {
        if (y.size != 0) {
          s = s.updated(x._1, s.get(x._1).getOrElse(ArrayBuffer()) ++ ArrayBuffer(y))
        }
      }
    }
    s
  }

  def getCommbCFT(opt: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]], jpq: ArrayBuffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {

    var tm = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    var ab = ArrayBuffer[ArrayBuffer[Int]]()
    for (x <- opt) {
      for (y <- x._2) {

        if (x._1 == 11 && y.filter(x => x >= jpq.max).size > 0) {
          tm = tm.mergeMap(TreeMap(x._1 ->
            ArrayBuffer(
              y.filter(x => x >= jpq.max))))
        } else if (x._1 == 2) {
          //找出记牌器中可能的最大对子，和当前手牌比较
          val dz = jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 2)
          //如果没有对子则把所有对子都当冲锋套
          if (dz.size > 0) {
            if (y.filter(x => x >= dz
              .max._1).size > 0) {
              tm = tm.mergeMap(TreeMap(x._1 ->
                ArrayBuffer(y.filter(x => x >= dz
                  .max._1))))
            }
          } else {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }

        } else if (x._1 == 112233 || x._1 == 33 || x._1 == 331 || x._1 == 332) {
          //飞机、飞机带翅膀、连对
          tm = tm.mergeMap(TreeMap(x._1 ->
            ArrayBuffer(y)))
        } else if (x._1 == 3) {
          val dz = jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)

          if (dz.size > 0 && y.head > dz.keySet.head) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y.filter(x => x >= dz
                .max._1))))
          } else if (dz.size == 0) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 31) {
          val dz = jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)
          if (dz.size > 0 && y.head >= dz.keySet.head) {

            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          } else if (dz.size == 0) {

            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 32) {
          val dz = jpq.map((_, 1))
            .groupBy(_._1)
            .map { x => (x._1, x._2.size) }
            .filter(x => x._2 >= 3)
          if (dz.size > 0 && y.head >= dz.keySet.head) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          } else if (dz.size == 0) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        } else if (x._1 == 12345) {
          //从剩余牌中组一个顺子，组不了则表示该顺子最大
          if (!findSequenceByJPQ(y, jpq)) {
            tm = tm.mergeMap(TreeMap(x._1 ->
              ArrayBuffer(y)))
          }
        }
      }
    }

    var s = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    for (x <- tm) {
      for (y <- x._2) {
        if (y.size != 0) {
          s = s.updated(x._1, s.get(x._1).getOrElse(ArrayBuffer()) ++ ArrayBuffer(y))
        }
      }
    }
    s
  }

  /**
   * 得到非冲锋套的套套牌
   */
  //FIXME gus99999999999
  def getNCFT(ai: AI): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var ncft = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()

    for (x <- ai.opt if (x._1 != 4 && x._1 != 1)) {

      ncft = ncft.updated(x._1, ai.opt.get(x._1).getOrElse(ArrayBuffer())
        -- getCFT(ai).get(x._1).getOrElse(ArrayBuffer()))
    }

    ncft
  }

  /**
   * 得到冲锋套的数量
   */
  def getCNum(ai: AI): Int = {
    getCFT(ai).flatMap(_._2).size
  }

  /**
   * 得到所有非冲锋套的数量
   */
  def getNCNum(ai: AI): Int = {

    getNCFT(ai).flatMap(_._2).size
  }

  /**
   * a1 顺子
   * a2 记牌器
   */
  def findSequenceByJPQ(a1: ArrayBuffer[Int], a2: ArrayBuffer[Int]): Boolean = {

    if (a1.max == 14) {
      return false
    }
    val cnt = a1.size
    //记牌器去重
    var tp = a2.distinct.sortWith(_ < _)
    tp --= tp.filter(x => x <= a1.head && x != 15 && x != 99 & x != 100)

    var b = false
    //记牌器剩余牌大于顺子长度
    if (tp.size >= a1.size) {
      for (x <- a1.head to 14 if tp.size >= cnt && b == false) {

        if (tp.take(cnt).last - tp.take(cnt).head == cnt - 1) {
          b = true
        }
        tp -= tp.head
      }
    }
    b
  }

  /**
   * 1.单牌
   * 2.对子
   * n：标签
   */
  def isCFT(n: Int, jpq: Buffer[Int], tp: Int): Boolean = {

    var cft = false
    if (tp == 1) {
      cft = n >= jpq.max
    } else if (tp == 2) {
      val dz = jpq.map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 2)
      if (dz.size > 0) {
        cft = n >= jpq.map((_, 1))
          .groupBy(_._1)
          .map { x => (x._1, x._2.size) }
          .filter(x => x._2 >= 2)
          .max._1
      } else {

        cft = true
      }
    }
    cft
  }

  //获得不是冲锋套的单牌
  def getTwoSingAndNotCFT(s: Buffer[Int], jpq: Buffer[Int]): ArrayBuffer[Int] = {
    val p = ArrayBuffer[Int]()
    for (x <- s) {
      if (!isCFT(x, jpq, 1)) {
        p += x
      }
    }
    p.sortWith(_ < _)
  }

  //获得不是冲锋套的对牌
  def getTwoDoubleAndNotCFT(s: Buffer[Int], jpq: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    val p = ArrayBuffer[Int]()
    for (x <- s) {
      if (!isCFT(x, jpq, 2)) {
        p += x
      }
    }
    p.sortWith(_ < _)
  }

  //火箭
  def rocket(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    val king = ArrayBuffer[Int]()
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(_._1 >= 99)
        .keySet.toBuffer
        .sortWith(_ < _)
    ) {
      king += x
    }

    if (king.size == 2) {
      ab --= king
      TreeMap(1 -> ArrayBuffer(king))
    } else {
      TreeMap(1 -> ArrayBuffer())
    }
  }

  // 炸弹
  def boom(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    val four = ArrayBuffer[ArrayBuffer[Int]]()
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 == 4)
        .keySet
        .toBuffer
        .sortWith(_ < _)
    ) {
      four += ArrayBuffer(x, x, x, x)
      ab --= ArrayBuffer(x, x, x, x)
    }
    TreeMap(4 -> four)
  }

  //组三张：222 暂时不组进三张
  def three(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var three = ArrayBuffer[ArrayBuffer[Int]]()
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 == 3 && x._1 <= 15)
        .keySet
        .toBuffer.sortWith(_ < _)
    ) {
      three += ArrayBuffer(x, x, x)
      ab --= ArrayBuffer(x, x, x)
    }
    TreeMap(3 -> three)
  }

  def threeF(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var three = ArrayBuffer[ArrayBuffer[Int]]()
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 3 && x._1 <= 15)
        .keySet
        .toBuffer.sortWith(_ < _)
    ) {
      three += ArrayBuffer(x, x, x)
      ab --= ArrayBuffer(x, x, x)
    }
    TreeMap(3 -> three)
  }

  //组飞机
  def fly(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var fly = ArrayBuffer[ArrayBuffer[Int]]()
    val tmp = ArrayBuffer[Int]()
    //纯飞机
    var tp = -1
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 == 3 && x._1 <= 14)
        .keySet
        .toBuffer
        .sortWith(_ < _)
    ) {
      //      if (Math.abs(x - tp) == 1) {
      //        fly += ArrayBuffer(tp, tp, tp, x, x, x).sortWith(_ < _)
      //        ab --= ArrayBuffer(tp, tp, tp, x, x, x)
      //        tp = -1
      //      } else {
      //        tp = x
      //      }
      if (tp == -1) {
        tmp += (x, x, x)
        tp = x
      } else {
        if (Math.abs(x - tp) == 1) {
          tmp += (x, x, x)
          tp = x
        } else {
          if (tmp.size >= 6) {
            fly += tmp.clone()
            tmp.clear()
            tmp += (x, x, x)
            tp = x
          } else {
            tmp.clear()
            tmp += (x, x, x)
            tp = x
            //tp = -1
          }
        }
      }
    }
    if (tmp.size >= 6) {
      fly += tmp
    }
    ab --= fly.flatMap(x => x)
    TreeMap(33 -> fly.sortBy(_.head))
  }

  //组连对
  def doubleSequence(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    //    var tp0 = -1
    //    var tp1 = -1
    //    var cf = 0
    //    var ds = ArrayBuffer[ArrayBuffer[Int]]()
    //    for (
    //      x <- ab
    //        .map((_, 1))
    //        .groupBy(_._1)
    //        .map { x => (x._1, x._2.size) }
    //        .filter(x => x._2 >= 2 && x._1 <= 14)
    //        .keySet
    //        .toBuffer
    //        .sortWith(_ > _)
    //    ) {
    //      if (tp1 - tp0 == -1 && x - tp1 == -1) {
    //        ds += ArrayBuffer(tp0, tp0, tp1, tp1, x, x).sortWith(_ < _)
    //        ab --= ArrayBuffer(tp0, tp0, tp1, tp1, x, x)
    //        cf = -1
    //        tp0 = -1
    //        tp1 = -1
    //      }
    //      if (cf == 0) { tp0 = x }
    //      if (cf == 1) { tp1 = x }
    //      cf += 1
    //      //挤掉之前不能构成连顺的第一个对子，对顺子适用
    //      if (cf >= 3) { tp0 = tp1; tp1 = x }
    //    }

    var ds = ArrayBuffer[ArrayBuffer[Int]]()
    var tmp = 0
    val ld = ArrayBuffer[Int]()

    val double = ab
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 2 && x._1 <= 14)
      .keySet
      .toBuffer
      .sortWith(_ < _)

    if (double.size >= 3) {

      for (x <- double if x <= 14) {

        if (tmp == 0 || x - tmp == 1) {
          ld += (x, x)
          tmp = x
          if (x == double.last && ld.size >= 6) {
            ds += ld.clone()
            ld.clear()
          }

        } else {

          if (ld.size >= 6) {
            ds += ld.clone()
            //            ld.clear()
            //            ld += (x, x)
            //            tmp = x
            //          } else {
            //            ld.clear()
            //            ld += (x, x)
            //            tmp = x
          }
          ld.clear()
          ld += (x, x)
          tmp = x
        }
      }
    }

    ab --= ds.flatMap(x => x)
    TreeMap(112233 -> ds.sortBy(_.head))
  }

  /**
   * 组顺子
   */
  def sequence(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {

    var cd = ab.filter(_ <= 14).sortWith(_ < _).clone()

    var sz = ArrayBuffer[ArrayBuffer[Int]]()

    var tmp = -1

    //剩余的牌不足5张，并且剩余的牌不能再组成顺子
    while (cd.size >= 5 && tmp != 100) {
      var isz = ArrayBuffer[Int]()
      //如果找到5张的顺子，跳出for
      for (x <- cd if isz.size < 5) {
        if (Math.abs(x - tmp) == 1) {
          isz += x
        }
        if (tmp != -1 && (x - tmp > 1)) {
          //跳牌,出现断牌,清空之前已加入数组的牌
          isz = ArrayBuffer[Int]()
        }
        if (isz.size == 0) {
          isz += x
        }
        tmp = x
      }

      //已找不到任何顺子，跳出while
      if (isz.size == 5) {
        cd = cd -- isz
        sz += isz
        tmp = -1
      } else {
        tmp = 100
      }
    }

    //扩展顺子
    for (x <- sz; y <- cd) {
      //可继续连接的顺子
      if (y - x.max == 1) {
        x += y
        cd = cd - y
      }
    }

    //内部再调整,顺子之间的连接
    //var fsz = ArrayBuffer[ArrayBuffer[Int]]()
    val szTmp1 = sz.clone()
    val szTmp2 = sz.clone()

    //优化 fix by gus
    for (x <- sz; y <- sz if x != null && y != null) {
      if (y.min - x.max == 1) {
        sz += (x ++ y)
        //减去已合并的顺子
        sz -= x
        sz -= y
      }
    }

    //顺子调优，找出双顺
    var yp = ab -- (sz.flatMap(x => x))
    var ds = ArrayBuffer[ArrayBuffer[Int]]()
    for (x <- sz) {
      //比较头,大于等于8张的顺子直接切头尾比较是否存在连对
      if (yp.distinct.sortWith(_ < _).take(3) == x.take(3) && x.size >= 8) {
        ds += (x.take(3) ++ x.take(3)).sortWith(_ < _)
        x --= x.take(3)
      }
      //比较尾
      if (yp.distinct.sortWith(_ > _).take(3) == x.sortWith(_ > _).take(3) && x.size >= 8) {
        ds += (x.sortWith(_ > _).take(3) ++ x.sortWith(_ > _).take(3)).sortWith(_ < _)
        x --= x.sortWith(_ > _).take(3)
      }
    }

    /**
     * 顺子继续调优 头尾找可以配出  炸弹，三张，对子
     */
    ab --= sz.flatMap(x => x).sortWith(_ < _)
    ab --= ds.flatMap(x => x).sortWith(_ < _)

    //倒序value
    /**
     * 不破坏顺子结构情况下，及调优后顺子长度>=5）对每一个顺子,
     * 结合剩余牌，搜索开头和结尾存在的对或者三条或者炸弹, 将其组成对、三条或者炸弹.
     * (如果结尾的剩余牌数目大于开头，则先处理结尾，否则先处理开头)。
     * 例如 3456789T,  依次在剩余牌中有33349TT，我们先检测3和T的数目，3的数目多，
     * 我们先拿出333。然后再检测4和T的数目，T的数目多，我们拿出TT. 接着检查4和9，一样多，优先小牌。
     */
    //不是最优
    val two = ArrayBuffer[ArrayBuffer[Int]]()
    val three = ArrayBuffer[ArrayBuffer[Int]]()
    val four = ArrayBuffer[ArrayBuffer[Int]]()
    val r = ab.map((_, 1)).groupBy(_._1).map { x => (x._1, x._2.size) }

    r.toList.sortWith(_._1 < _._1) foreach {
      case (key, value) =>
        breakable {
          for (x <- sz if x.size > 5) {
            //头尾有相同的
            if (x.head == key || x.last == key) {
              if (value == 1) {
                two += ArrayBuffer(key, key)
                x -= key
                ab -= key
                break()
              } else if (value == 2) {
                three += ArrayBuffer(key, key, key)
                x -= key
                ab -= (key, key)
                break()
              } else if (value == 3) {
                four += ArrayBuffer(key, key, key, key)
                x -= key
                ab -= (key, key, key)
                break()
              }
            }
          }
        }
    }

    TreeMap(12345 -> sz, 112233 -> ds, 2 -> two, 3 -> three, 4 -> four)
  }

  //三带优化
  def threeOpt(ab: ArrayBuffer[Int], dqp: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]],
               jpq: ArrayBuffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {

    var dqp_c = dqp
    //剩余单牌
    val s = ab
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 == 1 && x._1 < 15)
      .keySet
      .toBuffer
      .sortWith(_ < _)
    //剩余对子

    if (dqp.get(2).getOrElse(ArrayBuffer()).size > 0) {
      ab ++= dqp.get(2).getOrElse(ArrayBuffer()).flatMap(x => x)
      dqp.get(2).get.clear()
    }

    val d = ab.map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 == 2 && x._1 < 15)
      .keySet
      .toBuffer
      .sortWith(_ < _)

    val sd1 = ArrayBuffer[ArrayBuffer[Int]]()
    val sd2 = ArrayBuffer[ArrayBuffer[Int]]()

    val fly1 = ArrayBuffer[ArrayBuffer[Int]]()
    val fly2 = ArrayBuffer[ArrayBuffer[Int]]()

    val f1 = getTwoSingAndNotCFT(s, jpq)
    val f2 = getTwoDoubleAndNotCFT(d, jpq)

    for (x <- dqp_c.get(33).getOrElse(ArrayBuffer())) {
      val len = x.distinct.size
      if (len <= f1.size) {
        val fs = f1.take(len)
        dqp_c = dqp_c.updated(33, dqp_c.get(33).get - x)
        for (y <- fs) {
          dqp_c = dqp_c.updated(11, dqp_c.get(11).getOrElse(ArrayBuffer()) - ArrayBuffer(y))
        }
        dqp_c = dqp_c.updated(331, dqp_c.get(331).getOrElse(ArrayBuffer()) ++ ArrayBuffer(x ++ fs))
        ab --= fs
        f1 --= fs
      }
    }

    for (x <- dqp_c.get(33).getOrElse(ArrayBuffer())) {
      val len = x.distinct.size
      if (len <= f2.size) {
        val fs = f2.take(len)
        dqp_c = dqp_c.updated(33, dqp_c.get(33).get - x)
        for (y <- fs) {
          dqp_c = dqp_c.updated(2, dqp_c.get(2).getOrElse(ArrayBuffer()) - ArrayBuffer(y, y))
        }
        dqp_c = dqp_c.updated(332, dqp_c.get(332).getOrElse(ArrayBuffer()) ++ ArrayBuffer(x ++ (fs ++ fs).sortWith(_ < _)))
        ab --= fs
        ab --= fs
        f2 --= fs
      }
    }

    //组飞机带翅膀单
    //    for (x <- dqp_c.get(33).getOrElse(ArrayBuffer()) if f2.size >= dqp_c.get(33).getOrElse(ArrayBuffer()).size) {
    //
    //      val dz1 = f1.remove(0)
    //      val dz2 = f1.remove(0)
    //      dqp_c = dqp_c.updated(33,
    //        dqp_c.get(33).get - x)
    //      dqp_c = dqp_c.updated(11,
    //        dqp_c.get(11).getOrElse(ArrayBuffer()) - ArrayBuffer(dz1, dz2))
    //      dqp_c = dqp_c.updated(331,
    //        dqp_c.get(331).getOrElse(ArrayBuffer()) ++ ArrayBuffer(x ++ ArrayBuffer(dz1, dz2)))
    //      ab -= (dz1, dz2)
    //    }
    //
    //    //组飞机带翅膀对
    //    for (x <- dqp_c.get(33).getOrElse(ArrayBuffer()) if f2.size >= dqp_c.get(33).getOrElse(ArrayBuffer()).size) {
    //
    //      val dz1 = f2.remove(0)
    //      val dz2 = f2.remove(0)
    //      dqp_c = dqp_c.updated(33,
    //        dqp_c.get(33).get - x)
    //      dqp_c = dqp_c.updated(2,
    //        dqp_c.get(2).getOrElse(ArrayBuffer()) - ArrayBuffer(dz1, dz1, dz2, dz2))
    //      dqp_c = dqp_c.updated(332,
    //        dqp_c.get(332).getOrElse(ArrayBuffer()) ++ ArrayBuffer(x ++ ArrayBuffer(dz1, dz1, dz2, dz2)))
    //      ab -= (dz1, dz1, dz2, dz2)
    //
    //    }

    //三带调优
    /**
     * 遍历标签小于10的单牌、对牌，若单牌和对牌套数相等，则带标签最小的；若两种套牌数量不等，则带套数多的一类中标签最小的。
     */

    val three = dqp_c.get(3).get

    for (x <- three if f1.size > 0) {
      //单牌
      val s = f1.remove(0)
      sd1 += three.head ++ ArrayBuffer(s)
      three -= three.head
      ab -= s
    }

    for (x <- three if f2.size > 0) {
      //对子
      val d = f2.remove(0)
      sd2 += three.head ++ ArrayBuffer(d, d)
      three -= three.head
      ab -= (d, d)
    }

    //如果不是冲锋套单
    //    for (x <- three if s.size > 0 && s.head >= 10) {
    //      if (!isCFT(s.head, jpq, 1)) {
    //        sd1 += three.head ++ ArrayBuffer(s.head)
    //        three -= three.head
    //        ab -= s.head
    //        s -= s.head
    //      }
    //    }

    //如果不是冲锋套对子
    //    for (x <- three if d.size > 0 && d.head >= 10) {
    //      if (!isCFT(d.head, jpq, 2)) {
    //        sd2 += three.head ++ ArrayBuffer(d.head, d.head)
    //        three -= three.head
    //        ab -= (d.head, d.head)
    //        d -= (d.head, d.head)
    //      }
    //    }

    /**
     * 遍历标签大于等于10的单牌、对牌，若单牌和对牌均为冲锋套
     * 若有顺子张数大于等于6张（双顺张数大于等于8张），则拆顺子最小牌（拆双顺最小对）作为被带牌张，否则，带标签最小的单牌、对牌冲锋套
     */

    dqp_c += 31 -> sd1
    dqp_c += 32 -> sd2
    dqp_c
  }

  //被动三带
  //t=1 31 t=2 32 n=0 按最小套数原则  n=1 强行拆
  def threeOptPassive(ai: AI, t: Int): ArrayBuffer[Int] = {

    var hit = ArrayBuffer[Int]()
    val ts = getNCNum(ai)
    var flag = true

    //    val s1 = ai.opt.get(11).getOrElse(ArrayBuffer()).filter(x => x.head <= 14)
    //    val s2 = ai.opt.get(2).getOrElse(ArrayBuffer()).filter(x => x.head <= 14)

    //    val tw = ai.opt.flatMap(_._2).flatMap(x => x).filter(_ == 15).toBuffer
    //    tw.size >= s1.size

    val ops = new AI

    if (t == 1) {
      val cd = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer
      var t3 = Buffer[Int]()
      var t2 = Buffer[Int]()

      var tmpNum = 0

      val m1 = ArrayBuffer[Int]()

      t3 = cd
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 3)
        .keySet
        .toBuffer
        .sortWith(_ < _)

      t2 = (cd
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 1 && x._1 < 15)
        .keySet
        .toBuffer)
        .sortWith(_ < _)

      for (x <- t3; y <- t2.filter(p => p != x).distinct.sortWith(_ < _) if flag == true) {

        if (ai.t.head < x) {

          val ab = ArrayBuffer[Int]()
          val tb = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.clone

          tb.map(ab += _)
          ab --= ArrayBuffer(x, x, x, y)
          val oai = new AI
          val zp: TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = commb(ab, ai.jpq)
          oai.n1 = ai.n1
          oai.n2 = ai.n2
          oai.dz = ai.dz
          oai.jpq = ai.jpq
          oai.opt = zp
          val dts = getNCNum(oai)

          if (m1.size == 0) {
            m1 += (x, x, x, y)
            m1 += dts
            hit = ArrayBuffer(x, x, x, y)
            ops.opt = zp

          } else {

            if (m1.last > dts) {

              hit = ArrayBuffer(x, x, x, y)
              m1.clear()
              ops.opt = zp
              m1 += (x, x, x, y)
              m1 += dts

            }
          }

          //          if (dts <= ts + 1) {
          //            hit = ArrayBuffer(x, x, x, y)
          //            flag = false
          //            ai.opt = zp
          //          }
        }
      }
    } else if (t == 2) {
      val cd = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer
      var t3 = Buffer[Int]()
      var t2 = Buffer[Int]()
      val m2 = ArrayBuffer[Int]()
      t3 = cd
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 3)
        .keySet
        .toBuffer
        .sortWith(_ < _)

      t2 = (cd
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 >= 2 && x._1 < 15)
        .keySet
        .toBuffer)
        .sortWith(_ < _)

      for (x <- t3; y <- t2.filter(p => p != x).distinct.sortWith(_ < _) if flag == true) {
        if (ai.t.head < x) {

          val ab = ArrayBuffer[Int]()
          val tb = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer
          tb.map(ab += _)
          ab --= ArrayBuffer(x, x, x, y, y)
          val zp = commb(ab, ai.jpq)

          val oai = new AI
          oai.n1 = ai.n1
          oai.n2 = ai.n2
          oai.dz = ai.dz
          oai.jpq = ai.jpq
          oai.opt = zp

          val dts = getNCNum(oai)

          if (m2.size == 0) {
            m2 += (x, x, x, y)
            m2 += dts
            hit = ArrayBuffer(x, x, x, y, y)
            ops.opt = zp
          } else {

            if (m2.last > dts) {
              hit = ArrayBuffer(x, x, x, y, y)
              m2.clear()
              ops.opt = zp
              m2 += (x, x, x, y, y)
              m2 += dts

            }
          }

          //          if (dts <= ts + 1) {
          //            hit = ArrayBuffer(x, x, x, y, y)
          //            flag = false
          //            ai.opt = zp
          //          }

        }
      }
    }

    if (ops.opt.flatMap(_._2).size > 0) {

      if (hit.head == 15 && (ai.dz <= 6 || ai.n1 <= 6 || ai.n2 <= 6 || getNCNum(ai) <= 1)) {

        ai.opt = ops.opt
      } else if (hit.head != 15) {
        ai.opt = ops.opt
      }
    }

    hit
  }

  def threeOptPassiveAdv(ai: AI, t: Int): ArrayBuffer[Int] = {

    var hit = ArrayBuffer[Int]()

    hit = threeOptPassive(ai, t)

    hit
  }

  //组对子
  def double(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    val two = ArrayBuffer[ArrayBuffer[Int]]()

    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(x => x._2 == 2 && x._1 <= 15)
        .keySet
        .toBuffer.sortWith(_ < _)
    ) {

      two += ArrayBuffer(x, x)
      ab --= ArrayBuffer(x, x)

    }
    TreeMap(2 -> two)
  }

  //组单对单牌局部优化
  def one(ab: Buffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {

    var one = ArrayBuffer[ArrayBuffer[Int]]()
    var two = ArrayBuffer[ArrayBuffer[Int]]()
    var three = ArrayBuffer[ArrayBuffer[Int]]()
    var four = ArrayBuffer[ArrayBuffer[Int]]()
    for (
      x <- ab
        .map((_, 1))
        .groupBy(_._1)
        .map { x => (x._1, x._2.size) }
        .filter(_._2 == 1)
        .keySet
        .toBuffer
        .sortWith(_ < _)
    ) {
      one += ArrayBuffer(x)
      ab --= ArrayBuffer(x)

    }
    TreeMap(11 -> one)
  }

  def commb(ab: ArrayBuffer[Int], jpq: ArrayBuffer[Int]): TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]] = {
    var cd = ab.clone
    var d1 = rocket(cd)
      .mergeMap(boom(cd))
      .mergeMap(fly(cd))
      .mergeMap(three(cd))
      .mergeMap(doubleSequence(cd))
      .mergeMap(sequence(cd))

    //      println(d1)
    //      println(cd)      
    var dqp1 = threeOpt(cd, d1, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    cd = ab.clone()
    var d2 = sequence(cd)
      .mergeMap(rocket(cd))
      .mergeMap(boom(cd))
      .mergeMap(fly(cd))
      .mergeMap(doubleSequence(cd))
      .mergeMap(three(cd))
    var dqp2 = threeOpt(cd, d2, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    cd = ab.clone()
    var d3 = rocket(cd)
      .mergeMap(boom(cd))
      .mergeMap(fly(cd))
      .mergeMap(three(cd))
      .mergeMap(sequence(cd))
      .mergeMap(doubleSequence(cd))
    var dqp3 = threeOpt(cd, d3, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    cd = ab.clone()

    var d4 = rocket(cd)
      .mergeMap(boom(cd))
      .mergeMap(doubleSequence(cd))
      .mergeMap(fly(cd))
      .mergeMap(three(cd))
      .mergeMap(sequence(cd))

    var dqp4 = threeOpt(cd, d4, jpq).mergeMap(double(cd)).mergeMap(one(cd))

    var cnt1 = 0
    var cnt2 = 0
    var cnt3 = 0
    var cnt4 = 0

    /**
     * 7.	套数
     * 	手牌套牌数量的总和n
     * 1）根据当前组牌，数出套数n0
     * 2）减去最优组牌中的冲锋套数，共m2，令n=n0-m2
     *
     */

    cnt1 = dqp1.flatMap(_._2).size
    cnt2 = dqp2.flatMap(_._2).size
    cnt3 = dqp3.flatMap(_._2).size
    cnt4 = dqp4.flatMap(_._2).size

    val b1 = dqp1.get(1).getOrElse(Buffer()).size + dqp1.get(4).getOrElse(Buffer()).size
    val b2 = dqp2.get(1).getOrElse(Buffer()).size + dqp2.get(4).getOrElse(Buffer()).size
    val b3 = dqp3.get(1).getOrElse(Buffer()).size + dqp3.get(4).getOrElse(Buffer()).size
    val b4 = dqp4.get(1).getOrElse(Buffer()).size + dqp4.get(4).getOrElse(Buffer()).size

    //key相互覆盖
    var dqp = TreeMap(cnt1 - 2 * b1 -> dqp1, cnt2 - 2 * b2 -> dqp2, cnt3 - 2 * b3 -> dqp3, cnt4 - 2 * b4 -> dqp4)
    //var dqp = TreeMap(cnt1 -> dqp1)
    dqp.ordering

    var opt = dqp.get(dqp.firstKey).get

    if (opt.flatMap(_._2).flatMap(x => x).size > 8) {

      //      if (opt.get(4).getOrElse(ArrayBuffer()).flatMap(x => x).contains(15)) {
      //
      //        opt = opt.updated(4, opt.get(4).get - ArrayBuffer(15, 15, 15, 15))
      //        for (x <- 1 to 4) {
      //          opt = opt.updated(11, opt.get(11).get += ArrayBuffer(15))
      //        }
      //
      //      } 

      val sd = opt.get(11).getOrElse(ArrayBuffer()).size

      var ho = 0
      if (sd > 0) {
        ho = opt.get(11).get.filter(p => p.filter(p => p < 13).length > 0).length
      }

      if (opt.get(3).getOrElse(ArrayBuffer()).flatMap(x => x).contains(15)) {

        opt = opt.updated(3, opt.get(3).get - ArrayBuffer(15, 15, 15))
        for (x <- 1 to 3) {
          opt = opt.updated(11, opt.get(11).get += ArrayBuffer(15))
          //opt = opt.updated(2, opt.get(2).get += ArrayBuffer(15, 15))
        }
      } else if (opt.get(2).getOrElse(ArrayBuffer()).flatMap(x => x).contains(15) && ho >= 2) {

        opt = opt.updated(2, opt.get(2).get - ArrayBuffer(15, 15))
        for (x <- 1 to 2) {
          opt = opt.updated(11, opt.get(11).get += ArrayBuffer(15))
        }
      } else if (opt.get(31).getOrElse(ArrayBuffer()).flatMap(x => x).contains(15)) {

        val o1 = opt.get(31).get.last.last
        opt = opt.updated(31, opt.get(31).get - opt.get(31).get.last)

        for (x <- 1 to 3) {
          opt = opt.updated(11, opt.get(11).get += ArrayBuffer(15))
        }
        opt = opt.updated(11, opt.get(11).get += ArrayBuffer(o1))

      } else if (opt.get(32).getOrElse(ArrayBuffer()).flatMap(x => x).contains(15)) {

        val o2 = opt.get(32).get.last.last
        opt = opt.updated(32, opt.get(31).get - opt.get(32).get.last)
        for (x <- 1 to 3) {
          opt = opt.updated(11, opt.get(11).get += ArrayBuffer(15))
        }
        opt = opt.updated(2, opt.get(11).get += ArrayBuffer(o2))
      }
    }

    if (opt.get(11).getOrElse(ArrayBuffer()).length > 0) {
      opt = opt.updated(11, opt.get(11).getOrElse(ArrayBuffer[ArrayBuffer[Int]]()).sortWith((x, y) => x.head < y.head))
    }

    //opt.get(11).getOrElse(ArrayBuffer()).sortWith((x,y)=>x.head<y.head)
    opt
  }

  //打出能管上的最小对子，且套牌只能增加一套
  def doubleForce(cd: ArrayBuffer[Int], c: AI): Seq[Int] = {
    val bx = cd.head
    val db = c.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 2)
      .keySet
      .toSeq
      //确保在相同套数下，走最小的
      .sortWith(_ < _)
    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- c.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }
    var cnt = 100
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = c.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(c)
    for (x <- db) {
      if (bx < x) {
        val tmp = reAb.clone
        val reOpt = commb(tmp --= ArrayBuffer(x, x), c.jpq)
        if (reOpt.flatMap(_._2).size < cnt) {
          minTs = reOpt
          nt = ArrayBuffer(x, x)
          cnt = reOpt.flatMap(_._2).size
        }
      }
    }

    if ((c.r == 0 && (c.n1 <= 2 || c.n2 <= 2)) || (c.r != 0 && c.dz == 1)) {
      c.opt = minTs
      nt
    } else if (minTs.size > 0) {
      //val t2 = minTs.map(_._2.size).reduce(_ + _)
      val oai = new AI
      oai.n1 = c.n1
      oai.n2 = c.n2
      oai.dz = c.dz
      oai.jpq = c.jpq
      oai.opt = minTs

      val t2 = getNCNum(oai)
      //如果套数多出小于1套可以拆
      if (t2 - t1 <= 1) {
        c.opt = minTs
        nt
      } else {
        Seq()
      }
    } else {
      Seq()
    }
  }

  //强行打单，再重组
  def sigleForce(cd: ArrayBuffer[Int], c: AI): Seq[Int] = {

    val bx = cd.head
    val db = c.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 > 0)
      .keySet
      .toSeq
      //确保在相同套数下，走最小的
      .sortWith(_ < _)

    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- c.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }

    var cnt = 100
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = c.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(c)
    for (x <- db) {
      if (bx < x) {
        val tmp = reAb.clone
        val reOpt = commb(tmp --= ArrayBuffer(x), c.jpq)

        if (reOpt.flatMap(_._2).size < cnt) {
          minTs = reOpt
          nt = ArrayBuffer(x)
          cnt = reOpt.flatMap(_._2).size
        }
      }
    }

    if ((c.r == 0 && (c.n1 <= 2 || c.n2 <= 2)) || (c.r != 0 && c.dz == 1)) {
      c.opt = minTs
      nt
    } else if (minTs.size > 0) {

      //val t2 = minTs.map(_._2.size).reduce(_ + _)
      val oai = new AI
      oai.n1 = c.n1
      oai.n2 = c.n2
      oai.dz = c.dz
      oai.jpq = c.jpq
      oai.opt = minTs

      val t2 = getNCNum(oai)

      //如果套数多出小于1套可以拆
      if (t2 - t1 <= 1) {
        c.opt = minTs
        nt

      } else {
        Seq()
      }
    } else {
      Seq()
    }
  }

  //强行顺子，再重组
  def sequenceForce(cd: ArrayBuffer[Int], c: AI): Seq[Int] = {
    val bx = cd.sortWith(_ < _).head
    val slen = cd.size
    val ab = c.opt.flatMap(_._2.flatMap(x => x)).toSeq.distinct
    val dis = ab.sortWith(_ < _)
    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- c.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = c.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(c)
    var cnt = 100
    for (x <- 0 to dis.length - slen) {
      val t = dis.drop(x).take(slen)
      if (bx < t.head && t.last - t.head == slen - 1 && t.last < 15) {
        //重新组牌
        val tmp = reAb.clone()
        val reOpt = commb(tmp --= t, c.jpq)
        if (reOpt.flatMap(_._2).size < cnt) {
          minTs = reOpt
          nt = t
          cnt = reOpt.flatMap(_._2).size
        }
      }
    }

    if (minTs.size > 0) {
      //val t2 = minTs.map(_._2.size).reduce(_ + _)
      val oai = new AI
      oai.n1 = c.n1
      oai.n2 = c.n2
      oai.dz = c.dz
      oai.jpq = c.jpq
      oai.opt = minTs
      val t2 = getNCNum(oai)
      //如果套数多出小于1套可以拆
      var n = 1
      if (c.r == 0 && (c.n1 <= 2 || c.n2 <= 2)) {
        n = 1
      }

      if (c.r != 0 && (c.dz <= 2)) {
        n = 1
      }

      if (t2 - t1 <= n) {
        c.opt = minTs
        return nt
      } else {
        Seq()
      }
    } else {
      Seq()
    }
  }

  //强行3张，再重组
  def threeForce(cd: ArrayBuffer[Int], c: AI): Seq[Int] = {

    val bx = cd.head
    val db = c.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 3)
      .keySet
      .toSeq
      //确保在相同套数下，走最小的
      .sortWith(_ < _)
    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- c.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }
    var cnt = 100
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = c.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(c)
    for (x <- db) {
      if (bx < x) {
        val tmp = reAb.clone
        val reOpt = commb(tmp --= ArrayBuffer(x, x, x), c.jpq)
        if (reOpt.flatMap(_._2).size < cnt) {
          minTs = reOpt
          nt = ArrayBuffer(x, x, x)
          cnt = reOpt.flatMap(_._2).size
        }
      }
    }

    if (minTs.size > 0) {
      //val t2 = minTs.map(_._2.size).reduce(_ + _)

      val oai = new AI
      oai.n1 = c.n1
      oai.n2 = c.n2
      oai.dz = c.dz
      oai.jpq = c.jpq
      oai.opt = minTs
      val t2 = getNCNum(oai)

      //如果套数多出小于1套可以拆
      var n = 1
      if (c.r == 0 && (c.n1 <= 2 || c.n2 <= 2)) {
        n = 1
      }

      if (c.r != 0 && (c.dz <= 2)) {
        n = 1
      }

      if (t2 - t1 <= n) {
        c.opt = minTs
        return nt
      } else {
        Seq()
      }
    } else {
      Seq()
    }
  }

  //强行31张，再重组,待实现
  def sd1Force(cd: ArrayBuffer[Int], c: AI): Seq[Int] = {

    Seq()
  }

  //判断牌型
  def judgeTp(h: ArrayBuffer[Int]): Int = {
    val i = h.groupBy(x => x).map(x => (x._1, x._2.size))
    h.size match {
      case 0 => 0
      case 1 => 11
      case 2 => if (h.distinct.size == 1) 2 else 1
      case 3 => if (h.distinct.size == 1) 3 else 0
      case 4 => if (h.distinct.size == 1) 4 else if (i.count(x => x._2 == 3) == 1) 31 else 0
      case 5 => if (h.distinct.size == 5) 12345 else if (h.distinct.size == 2) 32 else 0
      case 6 => if (h.distinct.size == 2 && i.count(x => x._2 == 4) != 1) 33 else if (i.count(x => x._2 == 4) == 1 && (h.distinct.size == 3 || h.distinct.size == 2)) 41 else if (h.distinct.size == 3) 112233 else if (h.distinct.size == 6) 12345 else 0
      case 7 => if (h.distinct.size == 7) 12345 else 0
      case 8 => if (h.distinct.size == 8) 12345 else if (h.distinct.size == 4 && i.count(x => x._2 == 3) == 0) 112233
      else if (i.count(x => x._2 == 4) == 1 && h.distinct.size == 3) 42
      else if (h.distinct.size >= 3 && i.count(x => x._2 == 3) == 2) 331 else 0
      case 9  => if (h.distinct.size == 9) 12345 else if (h.distinct.size == 3) 33 else 0
      case 10 => if (h.distinct.size == 10) 12345 else if (h.distinct.size == 4) 332 else if (h.distinct.size == 5) 112233 else 0
      case 11 => if (h.distinct.size == 11) 12345 else 0
      case 12 => if (h.distinct.size == 12) 12345 else if (h.distinct.size == 4) 33 else if ((h.distinct.size == 6 || h.distinct.size == 5) && i.count(x => x._2 == 3) == 3) 331 else if (h.distinct.size == 6) 112233 else 0
      case 14 => if (h.distinct.size == 7) 112233 else 0
      case 15 => if (h.distinct.size == 6) 332 else if (h.distinct.size == 5) 33 else 0
      case 16 => if (h.distinct.size == 8) 112233 else 0
      case 18 => if (h.distinct.size == 9) 112233 else 0
      case 20 => 0
      case _  => 0
    }
  }

  //强行打单，再重组
  def sigleF(ai: AI): Seq[Int] = {

    val bx = ai.t.head
    val db = ai.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 > 0)
      .keySet
      .toSeq
      //确保在相同套数下，走最小的
      .sortWith(_ < _)

    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- ai.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }

    var cnt = 100
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = ai.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(ai)

    for (x <- db) {
      if (bx < x) {
        val tmp = reAb.clone
        val reOpt = commb(tmp --= ArrayBuffer(x), ai.jpq)

        if (ai.n1 <= 2 || ai.n2 <= 2 || (ai.r != 0 && ai.dz <= 2)) {
          if (reOpt.flatMap(_._2).size <= cnt) {
            minTs = reOpt
            nt = ArrayBuffer(x)
            cnt = reOpt.flatMap(_._2).size
          }
        } else {
          if (reOpt.flatMap(_._2).size < cnt || (reOpt.flatMap(_._2).size <= cnt && x >= ai.jpq.max)) {
            minTs = reOpt
            nt = ArrayBuffer(x)
            cnt = reOpt.flatMap(_._2).size
          }
        }
      }
    }

    //强拆
    val oai = new AI
    oai.n1 = ai.n1
    oai.n2 = ai.n2
    oai.dz = ai.dz
    oai.jpq = ai.jpq
    oai.opt = minTs
    val t2 = getNCNum(oai)

    var n = 0
    //地主拆牌增量小于1
    if (ai.r == 0) {
      n = 1
    }

    //战斗农民拆牌增量小于2
    if (ai.r != 0 && ai.zdnm == 1) {
      n = 2
    }

    //垃圾农民拆牌增量小于3
    if (ai.r != 0 && ai.zdnm == 0) {
      n = 3
    }

    //    oai.opt.flatMap(_._2).foreach(println)
    //    println(oai.jpq)
    //    println(getCFT(oai))
    //
    //    println("------")
    //
    //    ai.opt.flatMap(_._2).foreach(println)
    //    println(ai.jpq)
    //    println(getCFT(ai))

    if (t2 - t1 < n && minTs.size > 0) {
      ai.opt = minTs
      return nt
    } else {
      return Seq()
    }
  }

  //强行顺子，再重组
  def sequenceF(ai: AI): Seq[Int] = {

    val bx = ai.t.sortWith(_ < _).head
    val slen = ai.t.size
    val ab = ai.opt.flatMap(_._2.flatMap(x => x)).toSeq.distinct
    val dis = ab.sortWith(_ < _)
    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- ai.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = ai.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(ai)
    var cnt = 100
    for (x <- 0 to dis.length - slen) {
      val t = dis.drop(x).take(slen)
      if (bx < t.head && t.last - t.head == slen - 1 && t.last < 15) {
        //重新组牌
        val tmp = reAb.clone()
        val reOpt = commb(tmp --= t, ai.jpq)
        if (reOpt.flatMap(_._2).size < cnt) {
          minTs = reOpt
          nt = t
          cnt = reOpt.flatMap(_._2).size
        }
      }
    }

    val oai = new AI
    oai.n1 = ai.n1
    oai.n2 = ai.n2
    oai.dz = ai.dz
    oai.jpq = ai.jpq
    oai.opt = minTs
    val t2 = getNCNum(oai)

    var n = 1
    if (ai.r == 0 && (ai.n1 <= 2 || ai.n2 <= 2)) {
      n = 1
    }

    if (ai.r != 0 && (ai.dz <= 2)) {
      n = 1
    }

    if (t2 - t1 <= n && minTs.size > 0) {
      ai.opt = minTs
      return nt
    } else {
      return Seq()
    }

    //    //强拆
    //    if ((ai.r == 0 && (ai.n1 <= 2 || ai.n2 <= 2)) || (ai.r != 0 && ai.dz <= 1)) {
    //      ai.opt = minTs
    //      nt
    //    } else if (minTs.size > 0) {
    //      //val t2 = minTs.map(_._2.size).reduce(_ + _)
    //
    //      val oai = new AI
    //      oai.n1 = ai.n1
    //      oai.n2 = ai.n2
    //      oai.dz = ai.dz
    //      oai.jpq = ai.jpq
    //      oai.opt = minTs
    //      val t2 = getNCNum(oai)
    //
    //      //如果套数多出小于1套可以拆
    //      if (t2 - t1 <= 0 && (ai.n1 > 2 && ai.n1 >= 2 || (ai.r != 0 && ai.dz > 2))) {
    //        ai.opt = minTs
    //        nt
    //      } else if (t2 - t1 <= 1 && (ai.n1 <= 2 || ai.n1 <= 2 || (ai.r != 0 && ai.dz <= 2))) {
    //        ai.opt = minTs
    //        nt
    //      } else {
    //        Seq()
    //      }
    //    }
    //    else {
    //      Seq()
    //    }
  }

  //打出能管上的最小对子，且套牌只能增加一套
  def doubleF(ai: AI): Seq[Int] = {

    val bx = ai.t.head
    val db = ai.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 2 && x._2 <= 4)
      .keySet
      .toSeq
      //确保在相同套数下，走最小的
      .sortWith(_ < _)
    val reAb = ArrayBuffer[Int]()
    var nt = Seq[Int]()
    for (x <- ai.opt.flatMap(_._2.flatMap(x => x))) {
      reAb += x
    }
    var cnt = 100
    //组牌后的最小套数
    var minTs = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()
    //原来套数
    //val t1 = ai.opt.map(_._2.size).reduce(_ + _)
    val t1 = getNCNum(ai)
    for (x <- db) {
      if (bx < x) {
        val tmp = reAb.clone
        val reOpt = commb(tmp --= ArrayBuffer(x, x), ai.jpq)

        if (ai.n1 <= 2 || ai.n2 <= 2 || (ai.r != 0 && ai.dz <= 2)) {
          if (reOpt.flatMap(_._2).size <= cnt) {
            minTs = reOpt
            nt = ArrayBuffer(x, x)
            cnt = reOpt.flatMap(_._2).size
          }
        } else {
          if (reOpt.flatMap(_._2).size < cnt || (reOpt.flatMap(_._2).size <= cnt && x >= ai.jpq.max)) {
            minTs = reOpt
            nt = ArrayBuffer(x, x)
            cnt = reOpt.flatMap(_._2).size
          }
        }
      }
    }

    val oai = new AI
    oai.n1 = ai.n1
    oai.n2 = ai.n2
    oai.dz = ai.dz
    oai.jpq = ai.jpq
    oai.opt = minTs
    val t2 = getNCNum(oai)

    var n = 1
    if (ai.r == 0 && (ai.n1 <= 2 || ai.n2 <= 2)) {
      n = 1
    }

    if (ai.r != 0 && (ai.dz <= 2)) {
      n = 1
    }

    if (t2 - t1 <= n && minTs.size > 0) {
      ai.opt = minTs
      return nt
    } else {
      return Seq()
    }
  }

  //强行31张，再重组
  def sd1F(ai: AI): Seq[Int] = {

    var hit = ArrayBuffer[Int]()

    val d = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer

    val cd = ArrayBuffer[Int]()

    for (x <- d) {

      cd += x
    }

    var d1 = three(cd)
      .mergeMap(rocket(cd))
      .mergeMap(boom(cd))
      .mergeMap(fly(cd))
      .mergeMap(doubleSequence(cd))
      .mergeMap(sequence(cd))
    var dqp1 = threeOpt(cd, d1, ai.jpq).mergeMap(double(cd)).mergeMap(one(cd))

    //继续找单=======================================================================

    hit

  }

  def getBDTX(px: Int, ai: AI): ArrayBuffer[Int] = {

    //FIXME getBDTX
    var hit = ArrayBuffer[Int]()
    if (px == 12345) {
      val a = seqNewF(ai)
      if (a.size > 0) {
        for (x <- a) {
          hit += x
        }
      }
    } else if (px == 11) {
      val a = sigleNewF(ai)
      if (a.size > 0) {
        for (x <- a) {
          hit += x
        }
      }
    } else if (px == 2) {
      val a = doubleNewF(ai)
      if (a.size > 0) {
        for (x <- a) {
          hit += x
        }
      }
    } else if (px == 31) {
      val a = threeOptPassiveAdv(ai, 1)

      if (a.size > 0) {
        if (a.head < 15) {
          for (x <- a) {
            hit += x
          }
        } else {
          if (ai.dz <= 6 || ai.n1 <= 6 || ai.n2 <= 6 || getNCNum(ai) <= 1) {
            for (x <- a) {
              hit += x
            }
          }
        }
      }
    } else if (px == 32) {

      val a = threeOptPassiveAdv(ai, 2)

      if (a.size > 0) {
        if (a.head < 15) {
          for (x <- a) {
            hit += x
          }
        } else {
          if (ai.dz <= 6 || ai.n1 <= 6 || ai.n2 <= 6 || getNCNum(ai) <= 1) {
            for (x <- a) {
              hit += x
            }
          }
        }
      }
    } else if (px == 3) {

      val a = threeNewF(ai)
      if (a.size > 0) {
        for (x <- a) {
          hit += x
        }
      }
    }
    hit
  }

  def sigleNewF(ai: AI): Seq[Int] = {

    var hit = Seq[Int]()
    val t = ai.t
    val ncNum = getNCNum(ai)
    val ncDNum = getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size
    val ncSNum = getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size
    val cc = ctrNumHitting(ai)

    val ts = ai.opt.flatMap(_._2).size

    //所有大于桌面牌的集合
    val ds = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.filter(_ > t.head).distinct

    //最大增量为3（垃圾农民）
    var tmp = ncNum + 2

    //所有满足第一轮条件的组牌

    var passAI = TreeMap[Int, AI]()

    for (x <- ds) {
      val hp = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.sortWith(_ < _).clone()

      //最终确定的最优组牌
      val oai = new AI
      oai.n1 = ai.n1
      oai.n2 = ai.n2
      oai.dz = ai.dz
      oai.jpq = ai.jpq

      if (t.head < x) {
        val ab = ArrayBuffer[Int]()
        hp.map(ab += _)
        ab -= x
        oai.opt = commb(ab, ai.jpq)
        //遍历到最大的
        if (getNCNum(oai) <= tmp) {
          passAI += (x -> oai)
        }
      }
    }

    var fg = true
    for ((x, y) <- passAI if fg) {

      //d)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<1 则打出其中标签最大的A，否则不出
      if (ai.r == 0 && ai.n1 + ai.n2 == 3 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<=1且非冲锋套对牌的套数（与原有非冲锋套对牌的套数相比）增加量<1，则打出其中标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 2 || ai.n2 == 2) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(2).getOrElse(ArrayBuffer()).size == ncDNum) {
        ai.opt = y.opt
        hit = Seq(x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套牌套数【与原有非冲锋套非炸弹（火箭）的套牌套数相比】增加量<=1且非冲锋套单牌套数（与原有非冲锋套的单牌套数相比）增加量<1，则打出其中增加量最小标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 1 || ai.n2 == 1) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(1).getOrElse(ArrayBuffer()).size == ncSNum) {

        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r == 0 && ((ts == y.opt.flatMap(_._2).size) || ctrNumHitting(y) > (y.opt.flatMap(_._2).size - 2) ||
        (y.opt.flatMap(_._2).size - ctrNumHitting(y)) <= ts - cc || ctrNumHitting(y) >= 3)) {
        //6)	若出完套数<=原套数-1 或出完控制数>出完套数-2 或出完套数-出完控制数<=当前套数-当前控制数 或出完控制数>=3，则打出满足条件标签最小的A，否则
        ai.opt = y.opt
        hit = Seq(x)
        fg = false
      } else if (ai.r == 1 && ai.dz == 1 && ai.h == 0) {
        //viii.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r == 2 && ai.dz == 1 && ncNum <= getNCNum(y) + 1) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.dz == 2 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.zdnm == 1 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.zdnm == 0) {
        ai.opt = y.opt
        hit = Seq(x)
        fg = false
      }
    }
    hit
  }

  def doubleNewF(ai: AI): Seq[Int] = {
    var hit = Seq[Int]()
    val t = ai.t
    val ncNum = getNCNum(ai)
    val ncDNum = getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size
    val ncSNum = getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size
    val cc = ctrNumHitting(ai)

    val ts = ai.opt.flatMap(_._2).size

    //所有大于桌面牌的集合
    val ds = ai.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 2 && x._2 <= 4 && x._1 > t.head)
      .keySet
      .toSeq

    //最大增量为3（垃圾农民）
    var tmp = ncNum + 2

    //所有满足第一轮条件的组牌

    var passAI = TreeMap[Int, AI]()

    for (x <- ds) {
      val hp = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.sortWith(_ < _).clone()

      //最终确定的最优组牌
      val oai = new AI
      oai.n1 = ai.n1
      oai.n2 = ai.n2
      oai.dz = ai.dz
      oai.jpq = ai.jpq

      if (t.head < x) {
        val ab = ArrayBuffer[Int]()
        hp.map(ab += _)
        ab -= (x, x)
        oai.opt = commb(ab, ai.jpq)

        //遍历到最大的
        if (getNCNum(oai) <= tmp) {
          passAI += (x -> oai)
        }
      }
    }

    var fg = true
    for ((x, y) <- passAI if fg) {

      //优化大于等于K的直接盖住
      if (ai.t.head >= 13) {
        ai.opt = y.opt
        hit = Seq(x, x)
        return hit
      }

      //d)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<1 则打出其中标签最大的A，否则不出
      if (ai.r == 0 && ai.n1 + ai.n2 == 3 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<=1且非冲锋套对牌的套数（与原有非冲锋套对牌的套数相比）增加量<1，则打出其中标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 2 || ai.n2 == 2) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(2).getOrElse(ArrayBuffer()).size == ncDNum) {
        ai.opt = y.opt
        hit = Seq(x, x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套牌套数【与原有非冲锋套非炸弹（火箭）的套牌套数相比】增加量<=1且非冲锋套单牌套数（与原有非冲锋套的单牌套数相比）增加量<1，则打出其中增加量最小标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 1 || ai.n2 == 1) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(1).getOrElse(ArrayBuffer()).size == ncSNum) {
        ai.opt = y.opt
        hit = Seq(x, x)
      } else if (ai.r == 0 && ((ts == y.opt.flatMap(_._2).size) || ctrNumHitting(y) > (y.opt.flatMap(_._2).size - 2) ||
        (y.opt.flatMap(_._2).size - ctrNumHitting(y)) <= ts - cc || ctrNumHitting(y) >= 3)) {
        //6)	若出完套数<=原套数-1 或出完控制数>出完套数-2 或出完套数-出完控制数<=当前套数-当前控制数 或出完控制数>=3，则打出满足条件标签最小的A，否则
        ai.opt = y.opt
        hit = Seq(x, x)
        fg = false
      } else if (ai.r != 0 && ai.dz == 1) {
        //viii.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌
        ai.opt = y.opt
        hit = Seq(x, x)
      } else if (ai.r != 0 && ai.dz == 2 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x)
      } else if (ai.r != 0 && ai.zdnm == 1 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x)
      } else if (ai.r != 0 && ai.zdnm == 0) {
        ai.opt = y.opt
        hit = Seq(x, x)
        fg = false
      }
    }
    hit
  }

  def threeNewF(ai: AI): Seq[Int] = {
    var hit = Seq[Int]()
    val t = ai.t
    val ncNum = getNCNum(ai)
    val ncDNum = getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size
    val ncSNum = getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size
    val cc = ctrNumHitting(ai)

    val ts = ai.opt.flatMap(_._2).size

    //所有大于桌面牌的集合
    val ds = ai.opt
      .flatMap(_._2.flatMap(x => x))
      .map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }
      .filter(x => x._2 >= 3 && x._2 <= 4 && x._1 > t.head)
      .keySet
      .toSeq

    //最大增量为3（垃圾农民）
    var tmp = ncNum + 2

    //所有满足第一轮条件的组牌

    var passAI = TreeMap[Int, AI]()

    for (x <- ds) {
      val hp = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.sortWith(_ < _).clone()

      //最终确定的最优组牌
      val oai = new AI
      oai.n1 = ai.n1
      oai.n2 = ai.n2
      oai.dz = ai.dz
      oai.jpq = ai.jpq

      if (t.head < x) {
        val ab = ArrayBuffer[Int]()
        hp.map(ab += _)
        ab -= (x, x, x)
        oai.opt = commb(ab, ai.jpq)

        //遍历到最大的
        if (getNCNum(oai) <= tmp) {
          passAI += (x -> oai)
        }
      }
    }

    var fg = true
    for ((x, y) <- passAI if fg) {

      //优化大于等于K的直接盖住
      if (ai.t.head >= 13) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
        return hit
      }

      //d)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<1 则打出其中标签最大的A，否则不出
      if (ai.r == 0 && ai.n1 + ai.n2 == 3 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<=1且非冲锋套对牌的套数（与原有非冲锋套对牌的套数相比）增加量<1，则打出其中标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 2 || ai.n2 == 2) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(2).getOrElse(ArrayBuffer()).size == ncDNum) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套牌套数【与原有非冲锋套非炸弹（火箭）的套牌套数相比】增加量<=1且非冲锋套单牌套数（与原有非冲锋套的单牌套数相比）增加量<1，则打出其中增加量最小标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 1 || ai.n2 == 1) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(1).getOrElse(ArrayBuffer()).size == ncSNum) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } else if (ai.r == 0 && ((ts == y.opt.flatMap(_._2).size) || ctrNumHitting(y) > (y.opt.flatMap(_._2).size - 2) ||
        (y.opt.flatMap(_._2).size - ctrNumHitting(y)) <= ts - cc || ctrNumHitting(y) >= 3)) {
        //6)	若出完套数<=原套数-1 或出完控制数>出完套数-2 或出完套数-出完控制数<=当前套数-当前控制数 或出完控制数>=3，则打出满足条件标签最小的A，否则
        ai.opt = y.opt
        hit = Seq(x, x, x)
        fg = false
      } else if (ai.r != 0 && ai.dz == 1) {
        //viii.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } else if (ai.r != 0 && ai.dz == 2 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } else if (ai.r != 0 && ai.zdnm == 1 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
      } else if (ai.r != 0 && ai.zdnm == 0) {
        ai.opt = y.opt
        hit = Seq(x, x, x)
        fg = false
      }
    }
    hit
  }

  def seqNewF(ai: AI): Seq[Int] = {
    var hit = Seq[Int]()
    val t = ai.t
    val len = t.size
    val ncNum = getNCNum(ai)
    val ncDNum = getNCFT(ai).get(2).getOrElse(ArrayBuffer()).size
    val ncSNum = getNCFT(ai).get(11).getOrElse(ArrayBuffer()).size
    val cc = ctrNumHitting(ai)

    val ts = ai.opt.flatMap(_._2).size

    //所有大于桌面牌的集合
    val ds = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.filter(x => x > t.head && x < 15).distinct.sortWith(_ < _)

    //最大增量为3（垃圾农民）
    var tmp = ncNum + 2

    //所有满足第一轮条件的组牌

    var passAI = TreeMap[Int, AI]()

    var pNum = -1

    for (y <- ds) {

      val hp = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.filter(x => x >= y && x < 15).distinct

      val hb = ai.opt.flatMap(_._2).flatMap(x => x).toBuffer.sortWith(_ < _).clone()

      val sz = hp.sortWith(_ < _).take(len)

      //最终确定的最优组牌
      val oai = new AI
      oai.n1 = ai.n1
      oai.n2 = ai.n2
      oai.dz = ai.dz
      oai.jpq = ai.jpq

      if (sz.last - sz.head == len - 1 && sz.size == len) {

        val ab = ArrayBuffer[Int]()
        hb.map(ab += _)
        for (x <- sz) {
          ab -= x
        }
        oai.opt = commb(ab, ai.jpq)

        //遍历到最大的

        if (pNum == -1) {

          passAI += (y -> oai)
        } else {

          if (getNCNum(oai) <= pNum) {

            for (x1 <- passAI.keySet) {
              passAI = passAI - (x1)
            }

            passAI += (y -> oai)
          }
        }
        pNum = getNCNum(oai)
      }

    }

    var fg = true
    for ((x, y) <- passAI if fg) {

      //d)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<1 则打出其中标签最大的A，否则不出
      if (ai.r == 0 && ai.n1 + ai.n2 == 3 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套数【与原有非冲锋套非炸弹（火箭）的套数相比】增加量<=1且非冲锋套对牌的套数（与原有非冲锋套对牌的套数相比）增加量<1，则打出其中标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 2 || ai.n2 == 2) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(2).getOrElse(ArrayBuffer()).size == ncDNum) {
        ai.opt = y.opt
        hit = Seq(x)
      } //e)	若打出A后，非冲锋套非炸弹（火箭）的套牌套数【与原有非冲锋套非炸弹（火箭）的套牌套数相比】增加量<=1且非冲锋套单牌套数（与原有非冲锋套的单牌套数相比）增加量<1，则打出其中增加量最小标签最大的A，否则不出
      else if (ai.r == 0 && (ai.n1 == 1 || ai.n2 == 1) && ncNum <= getNCNum(y) + 1 && getNCFT(y).get(1).getOrElse(ArrayBuffer()).size == ncSNum) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if ((ai.r == 0 && ts <= y.opt.flatMap(_._2).size + 1)) {
        //|| ctrNumHitting(y) > (y.opt.flatMap(_._2).size - 2) ||
        //(y.opt.flatMap(_._2).size - ctrNumHitting(y)) <= ts - cc || ctrNumHitting(y) >= 3
        //6)	若出完套数<=原套数-1 或出完控制数>出完套数-2 或出完套数-出完控制数<=当前套数-当前控制数 或出完控制数>=3，则打出满足条件标签最小的A，否则

        ai.opt = y.opt
        hit = Seq(x)
        fg = false
      } else if (ai.r == 1 && ai.dz == 1 && ai.h == 0) {
        //viii.	不考虑组套，遍历所有手牌，若能在手牌中找出大于X的同类套牌，则打出其中标签最大的一套，否则过牌
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r == 2 && ai.dz == 1 && ncNum <= getNCNum(y) + 1) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.dz == 2 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.zdnm == 1 && ncNum <= getNCNum(y)) {
        ai.opt = y.opt
        hit = Seq(x)
      } else if (ai.r != 0 && ai.zdnm == 0) {
        ai.opt = y.opt
        hit = Seq(x)
        fg = false
      }
    }

    if (hit.size > 0) {
      for (x <- hit(0) + 1 to hit(0) + len - 1) {

        hit = hit :+ x
      }
    }

    hit
  }

  /**
   * b)打牌过程中第一档和第二档的大牌控制赋值：
   * 第一档：当前牌面最大的牌（大、小王为同一档）；第二档：当前牌面次大的牌
   * 取当前剩余牌中最大的前两档牌，每有1张第一档牌加1，第二档牌加0.5
   * 3)	每个玩家的控制数最少为0
   * Eg:地主手牌：8888 AA KK TT  农民1：A KK QQ 9 6  农民2：JJ T 99
   * 地主的总控制数=1+1*2+0.5*2 = 4
   * 农民1的总控制数为：1+0.5*2=2
   * 农民2的总控制数为：0
   *
   */
  def ctrNumHitting(ai: AI): Double = {

    val dp = ai.opt.flatMap(_._2).flatMap(x => x).map((_, 1))
      .groupBy(_._1)
      .map { x => (x._1, x._2.size) }.toBuffer.sortWith((x, y) => x._1 > y._1)

    val jm = ai.jpq.max

    var ctrNum: Double = 0
    var cn = 0

    ctrNum += ai.opt.filter(_._1 == 4).flatMap(_._2).size

    var d2 = 0
    if (ai.jpq.distinct.size > 1) {
      d2 = (ai.jpq.distinct -= (ai.jpq.distinct.sortWith(_ < _).last)).sortWith(_ < _).last
    } else {
      d2 = jm
    }

    for (x <- dp if cn <= 1) {

      if (x._1 == 100 || x._1 == 99) {
        ctrNum += 1
        cn = 1
      } else {
        if (x._1 >= jm) {
          ctrNum += x._2
          cn += 1
        } else if (x._1 < jm && x._1 >= d2) {
          ctrNum += (0.5D * x._2)
          cn += 1
        }
      }
    }
    ctrNum
  }

  //花色转换
  def colourConvert(p: ArrayBuffer[Int], ai: AI): ArrayBuffer[String] = {

    if (p.size == 0 || p.head == -1) { return ArrayBuffer() }

    val converHit = ArrayBuffer[String]()

    for (x <- p) {

      if (x != 99 && x != 100) {

        val c = ai.colourBox.get(x).head

        ai.colourBox.get(x) -= c

        converHit += (ai.c.get(c).get + x)
      } else {
        converHit += x + ""
      }
    }
    converHit
  }

  def colourConvertN(p: ArrayBuffer[Int], ai: AI): ArrayBuffer[Int] = {

    if (p.size == 0 || p.head == -1) { return ArrayBuffer() }

    val converHit = ArrayBuffer[Int]()

    for (x <- p) {

      if (x != 99 && x != 100) {

        val c = ai.colourBox.get(x).head

        ai.colourBox.get(x) -= c

        converHit += (x * 10 + c)
      } else {
        converHit += x
      }
    }
    converHit
  }

  //花色切分 转储
  def segmentConvert(p: ArrayBuffer[Int], ai: AI) {

    ai.ncPoker.clear()
    ai.colourBox.clear()

    for (x <- p) {
      if (x != 99 && x != 100) {
        val t = x / 10
        val u = x % 10
        if (ai.colourBox.containsKey(t)) {
          ai.colourBox.put(t, ai.colourBox.get(t) += u)
        } else {
          ai.colourBox.put(t, Buffer(u))
        }
        ai.ncPoker += t
      } else {
        ai.ncPoker += x
      }
    }

    ai.handPoker = ai.ncPoker

  }

  //脱色
  def outClour(p: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    val pok = ArrayBuffer[Int]()
    for (x <- p) {
      if (x != 99 && x != 100) {
        val t = x / 10
        val u = x % 10
        pok += t
      } else {
        pok += x
      }
    }
    pok
  }

  def optimizeHp(ai: AI): ArrayBuffer[Int] = {

    var hit = ArrayBuffer[Int]()

    val px = judgeTp(ai.t)

    if (px == 1) { return ArrayBuffer() }

    if (ai.opt.flatMap(_._2).size == 2 && ai.r != 0) {
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

          val h = ai.opt.get(4).get.filter(p => p.head > ai.t.head)
          if (h.length > 0) {
            hit = h.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
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

    //如果全部都时炸弹
    if (getCNum(ai) + getNCNum(ai) == 0) {
      if (px != 4) {
        if (ai.opt.get(4).getOrElse(ArrayBuffer()).length > 0) {
          hit = ai.opt.get(4).get.head
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
        }
      }
      return hit
    }

    return hit
  }

  def optZD(ai: AI): ArrayBuffer[Int] = {

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

          val h = ai.opt.get(4).get.filter(p => p.head > ai.t.head)
          if (h.length > 0) {
            hit = h.head
            ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
            return hit
          }
        } else if (getCNum(ai) >= 1) {

          hit = getCFT(ai).flatMap(_._2).toBuffer.sortWith((x, y) => x.head > y.head).last
          ai.opt.map { x => if (x._2.contains(hit)) x._2 -= hit }
          return hit
        }
      }
    }

    return hit
  }
}