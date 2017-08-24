package com.gus.ai

import scala.collection.mutable.Buffer
import util.control.Breaks._
import scala.collection.immutable.TreeMap

object Comb {

  //搜索天王炸弹
  def combSupperBoom(ai: AI) {

    if (ai.handPoker.sortWith((x, y) => x.m < y.m).containsSlice(Buffer(Pok(99, 0), Pok(99, 0), Pok(100, 0), Pok(100, 0)))) {

      ai.opt = ai.opt.updated(1, Buffer(Buffer(Pok(99, 0), Pok(99, 0), Pok(100, 0), Pok(100, 0))))

      ai.handPoker -= (Pok(99, 0), Pok(99, 0), Pok(100, 0), Pok(100, 0))
    }
  }

  //五张和五张以上的炸弹 八炸以上不组
  def combFiveBoom(ai: AI) {

    val moreThanFiveBoom = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 >= 5)

    for ((x, y) <- moreThanFiveBoom) {

      if (y == 5) {

        ai.opt = ai.opt.updated(45, ai.opt.get(45).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x))

      } else if (y == 6) {

        ai.opt = ai.opt.updated(46, ai.opt.get(46).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x))

      } else if (y == 7) {

        ai.opt = ai.opt.updated(47, ai.opt.get(47).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x))

      } else if (y == 8) {

        ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x).take(4))
        ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x).takeRight(4))

      }

      ai.handPoker --= ai.handPoker.filter(_.m == x)

    }
  }

  //组同花顺（癞子调优）
  def combSeqColour(ai: AI) {

    val tmpB = Buffer[Buffer[Pok]]()

    val tmpS = Buffer[Pok]()

    var tmpP: Pok = null

    val comp1 = ai.handPoker.clone.sortWith((x, y) => x.m < y.m)

    var comp2 = ai.handPoker.clone.sortWith((x, y) => x.m < y.m)

    //先找出自然同花顺从2开始
    for (x <- comp1 if x != null) {

      breakable {
        tmpS.clear; tmpP = null;
        for (y <- comp2 -= x if y != null) {

          tmpS.size match {

            //首次比较，如果前2个同花且顺，入列，如果不跳数，入列，否则跳出
            case 0 => if (y.m == x.m + 1 && y.c == x.c) { tmpS += x; tmpS += y; tmpP = y }
            else if (x.m >= y.m - 1) { tmpS += x; tmpP = x }
            else { tmpS.clear; tmpP = null; break }

            //组成同花，更新手牌
            case 5 =>
              val ctmpS = tmpS.clone; tmpB += ctmpS; ai.handPoker --= ctmpS; comp2 --= ctmpS; tmpS.clear; tmpP = null; break

            //如果前2个同花且顺，入列，如果跳数则跳出，其它继续
            case _ => if (y.m == tmpP.m + 1 && y.c == tmpP.c) { tmpS += y; tmpP = y } else if (tmpP.m < y.m - 1 || tmpP.m > y.m) { tmpS.clear; tmpP = null; break }
          }

          if (tmpS.size == 5) {
            val ctmpS = tmpS.clone; tmpB += ctmpS; ai.handPoker --= ctmpS; comp2 --= ctmpS; tmpS.clear; tmpP = null;
          }
        }
      }
    }

    //A转换换成 1 再组一次
    tmpS.clear()

    ai.handPoker = ai.handPoker.map(x => if (x.m == 14) Pok(1, x.c) else x).clone().sortWith((x, y) => x.m < y.m)

    comp2 = ai.handPoker.clone().sortWith((x, y) => x.m < y.m)

    for (x <- ai.handPoker if x != null) {

      breakable {

        for (y <- comp2 -= x if y != null) {

          tmpS.size match {

            //首次比较，如果前2个同花且顺，入列，如果不跳数，入列，否则跳出
            case 0 => if (y.m == x.m + 1 && y.c == x.c) { tmpS += x; tmpS += y; tmpP = y }
            else if (x.m >= y.m - 1) { tmpS += x; tmpP = x }
            else { tmpS.clear; tmpP = null; break }

            //组成同花，更新手牌
            case 5 =>
              val ctmpS = tmpS.clone; tmpB += ctmpS; ai.handPoker --= ctmpS; comp2 --= ctmpS; tmpS.clear; tmpP = null; break

            //如果前2个同花且顺，入列，如果跳数则跳出，其它继续
            case _ => if (y.m == tmpP.m + 1 && y.c == tmpP.c) { tmpS += y; tmpP = y }

            else if (tmpP.m < y.m - 1) { tmpS.clear; tmpP = null; break }

          }
        }
      }
    }

    //未用完的A转换回来
    ai.handPoker = ai.handPoker.map(x => if (x.m == 1) Pok(14, x.c) else x).clone().sortWith((x, y) => x.m < y.m)
    //开始癞子调优,剩余牌中切分4组同花,不能自然组成单色同花的必然最多能组2次不一样的同花顺,每种花色去排序后4个一组步进,要组成的直接删除包含最大的牌的所有步进
    //第一层循环，是防止出现 AABBCCEE这样的同花顺

    for (o <- 1 to 2 if ai.lz.size > 0) {

      val c1 = ai.handPoker.filter(_.c == 1).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c2 = ai.handPoker.filter(_.c == 2).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c3 = ai.handPoker.filter(_.c == 3).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c4 = ai.handPoker.filter(_.c == 4).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val seqS = Buffer(c1, c3, c2, c4)

      for (s <- seqS) {

        for (x <- s if ai.lz.size > 0 && x != null && x.size == 4) {

          val head = x.head.m

          val last = x.last.m

          val lastV = x.last

          if (last - head == 3 || last - head == 4) {

            tmpB += (x += ai.lz.remove(0))

            ai.handPoker --= x

            val tmp = s.filter(p => p.contains(lastV) && !p.equals(x))

            s --= tmp
          }
        }
      }
    }

    //尝试一个癞子的组同花顺
    ai.handPoker = ai.handPoker.map(x => if (x.m == 14) Pok(1, x.c) else x).clone().sortWith((x, y) => x.m < y.m)

    for (o <- 1 to 2 if ai.lz.size > 0) {

      val c1 = ai.handPoker.filter(_.c == 1).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c2 = ai.handPoker.filter(_.c == 2).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c3 = ai.handPoker.filter(_.c == 3).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val c4 = ai.handPoker.filter(_.c == 4).distinct.sortWith((x, y) => x.m < y.m).sliding(4, 1).toBuffer

      val seqS = Buffer(c1, c3, c2, c4)

      for (s <- seqS) {

        for (x <- s if ai.lz.size > 0 && x != null && x.size == 4) {

          val head = x.head.m

          val last = x.last.m

          val lastV = x.last

          if (last - head == 3 || last - head == 4) {

            tmpB += (x += ai.lz.remove(0))

            ai.handPoker --= x

            val tmp = s.filter(p => p.contains(lastV) && !p.equals(x))

            s --= tmp

          }
        }
      }
    }

    //尝试两个癞子的组同花顺
    for (o <- 1 to 2 if ai.lz.size == 2) {

      val c1 = ai.handPoker.filter(_.c == 1).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c2 = ai.handPoker.filter(_.c == 2).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c3 = ai.handPoker.filter(_.c == 3).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c4 = ai.handPoker.filter(_.c == 4).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val seqS = Buffer(c1, c3, c2, c4)

      for (s <- seqS) {

        for (x <- s if ai.lz.size > 0 && x.size == 3) {

          val head = x.head.m

          val last = x.last.m

          val lastV = x.last

          if (last - head <= 4) {

            tmpB += (x += (ai.lz.remove(0), ai.lz.remove(0)))

            ai.handPoker --= x

            val tmp = s.filter(p => p.contains(lastV) && !p.equals(x))

            s --= tmp
          }
        }
      }
    }

    //未用完的A转换回来
    ai.handPoker = ai.handPoker.map(x => if (x.m == 1) Pok(14, x.c) else x).clone().sortWith((x, y) => x.m < y.m)
    //尝试两个癞子的组同花顺
    for (o <- 1 to 2 if ai.lz.size == 2) {

      val c1 = ai.handPoker.filter(_.c == 1).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c2 = ai.handPoker.filter(_.c == 2).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c3 = ai.handPoker.filter(_.c == 3).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val c4 = ai.handPoker.filter(_.c == 4).distinct.sortWith((x, y) => x.m < y.m).sliding(3, 1).toBuffer

      val seqS = Buffer(c1, c3, c2, c4)

      for (s <- seqS) {

        for (x <- s if ai.lz.size > 0 && x != null && x.size == 4) {

          val head = x.head.m

          val last = x.last.m

          val lastV = x.last

          if (last - head <= 4) {

            tmpB += (x += (ai.lz.remove(0), ai.lz.remove(0)))

            ai.handPoker --= x

            val tmp = s.filter(p => p.contains(lastV) && !p.equals(x))

            s --= tmp
          }
        }
      }
    }

    for (th <- tmpB if th.filter(p => p.m == ai.master).length == 0) {

      breakable {

        for (y <- th if y.m < 10) {

          if (ai.handPoker.filter(x => x.m == y.m).length > 0 && ai.handPoker.filter(x => x.m == th.last.m + 1 && x.c == y.c).length > 0) {

            ai.handPoker += y

            val last = ai.handPoker.filter(x => x.m == th.last.m + 1 && x.c == y.c).clone

            ai.handPoker -= last.head

            th -= y

            th += last.head
          }

          break
        }
      }
    }

    if (tmpB.size > 1) {

      ai.opt = ai.opt.updated(50, tmpB.sortWith((x, y) => x.head.m < y.head.m))

    } else {

      ai.opt = ai.opt.updated(50, tmpB)

    }
    ai.opt = ai.opt.updated(50, ai.opt.get(50).getOrElse(Buffer()).map(x => x.map(y => if (y.m == 1) Pok(14, y.c) else y)))
  }

  def combSeq(ai: AI) {

    val hb = ai.handPoker.filter(x => x.m <= 14).sortWith((x, y) => x.m < y.m)

    var seq = Buffer[Buffer[Pok]]()

    var tmp = -1

    while (hb.size >= 5 && tmp != 100) {

      var seqChild = Buffer[Pok]()

      for (x <- ai.handPoker if seqChild.size < 5) {

        if (Math.abs(x.m - tmp) == 1) {

          seqChild += x
        }
        if (tmp != -1 && (x.m - tmp > 1)) {
          //跳牌,出现断牌,清空之前已加入数组的牌
          seqChild = Buffer[Pok]()
        }
        if (seqChild.size == 0) {
          seqChild += x
        }
        tmp = x.m
      }

      //已找不到任何顺子，跳出while
      if (seqChild.size == 5) {

        ai.handPoker --= seqChild

        seq += seqChild

        tmp = -1

      } else {

        tmp = 100

      }
    }

    //组A2345，查看剩余的2345和14
    ai.handPoker = ai.handPoker.map(x => if (x.m == 14) Pok(1, x.c) else x).clone().sortWith((x, y) => x.m < y.m)

    tmp = -1

    while (ai.handPoker.size >= 5 && tmp != 100) {

      var seqChild = Buffer[Pok]()

      for (x <- ai.handPoker if seqChild.size < 5) {

        if (Math.abs(x.m - tmp) == 1) {

          seqChild += x
        }
        if (tmp != -1 && (x.m - tmp > 1)) {
          //跳牌,出现断牌,清空之前已加入数组的牌
          seqChild = Buffer[Pok]()
        }
        if (seqChild.size == 0) {
          seqChild += x
        }
        tmp = x.m
      }

      //已找不到任何顺子，跳出while
      if (seqChild.size == 5) {

        ai.handPoker --= seqChild

        seq += seqChild

        tmp = -1

      } else {

        tmp = 100

      }
    }

    //未用完的A转换回来
    ai.handPoker = ai.handPoker.map(x => if (x.m == 1) Pok(14, x.c) else x).clone().sortWith((x, y) => x.m < y.m)

    //放1个癞子组顺子 
    tmp = -1

    while (ai.handPoker.size >= 4 && tmp != 100 && ai.lz.size > 0) {

      var seqChild = Buffer[Pok]()

      for (x <- ai.handPoker if seqChild.size <= 4) {

        if (seqChild.size == 0) {

          seqChild += x

          tmp = x.m

        } else {

          val first = seqChild.head

          //FIXME=================================================================
          if (first != null && x.m - first.m <= seqChild.length + 1 && x.m != tmp && seqChild.length < 4) {

            seqChild += x

            tmp = x.m

          } else if (first != null && x.m - first.m > seqChild.length + 1 && seqChild.size < 4) {
            //跳牌,出现断牌,清空之前已加入数组的牌
            seqChild = Buffer[Pok]()

            seqChild += x

            tmp = x.m
          }
        }
      }

      if (seqChild.size == 4) {

        ai.handPoker --= seqChild

        seq += (seqChild += ai.lz.remove(0))

        tmp = -1

      } else {

        tmp = 100

      }
    }

    //组A2345，查看剩余的2345和14
    ai.handPoker = ai.handPoker.map(x => if (x.m == 14) Pok(1, x.c) else x).clone().sortWith((x, y) => x.m < y.m)
    tmp = -1

    while (ai.handPoker.size >= 5 && tmp != 100 && ai.lz.size > 0) {

      var seqChild = Buffer[Pok]()

      for (x <- ai.handPoker if seqChild.size <= 4) {

        if (seqChild.size == 0) {

          seqChild += x

          tmp = x.m

        } else {

          val first = seqChild.head

          if (first != null && x.m - first.m <= 4 && x.m != tmp) {

            seqChild += x

            tmp = x.m

          } else if (first != null && x.m - first.m > 4 && seqChild.size < 4) {

            //跳牌,出现断牌,清空之前已加入数组的牌
            seqChild = Buffer[Pok]()

            seqChild += x

            tmp = x.m

          }
        }
      }

      if (seqChild.size == 4) {

        ai.handPoker --= seqChild

        seq += (seqChild += ai.lz.remove(0))

        tmp = -1

      } else {

        tmp = 100

      }
    }

    //未用完的A转换回来
    ai.handPoker = ai.handPoker.map(x => if (x.m == 1) Pok(14, x.c) else x).clone().sortWith((x, y) => x.m < y.m)

    //放2个癞子组普通顺子，不划算。。。待定

    if (seq.size > 1) {

      ai.opt = ai.opt.updated(51, seq.sortWith((x, y) => x.head.m < y.head.m))

    } else {

      ai.opt = ai.opt.updated(51, seq)

    }

    ai.opt = ai.opt.updated(51, ai.opt.get(51).getOrElse(Buffer()).map(x => x.map(y => if (y.m == 1) Pok(14, y.c) else y)))

    if (ai.opt.get(51).getOrElse(Buffer()).length > 0) {

      for (x <- ai.opt.get(51).get if x.filter(x => x.m == ai.master).length == 0) {
        var cnt = 0

        if (ai.handPoker.filter(p => p.m == x.head.m).length >= 1
          && ai.handPoker.filter(p => p.m == x.last.m + 1).length == 1) {
          cnt = 1
        }
        if (ai.handPoker.filter(p => p.m == x(0).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(1).m).length >= 1
          && ai.handPoker.filter(p => p.m == x.last.m + 1).length == 1
          && ai.handPoker.filter(p => p.m == x.last.m + 2).length == 1) {
          cnt = 2
        }
        if (ai.handPoker.filter(p => p.m == x(0).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(1).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(2).m).length >= 1
          && ai.handPoker.filter(p => p.m == x.last.m + 1).length == 1
          && ai.handPoker.filter(p => p.m == x.last.m + 2).length == 1
          && ai.handPoker.filter(p => p.m == x.last.m + 3).length == 1) {
          cnt = 3
        }
        if (ai.handPoker.filter(p => p.m == x(0).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(1).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(2).m).length >= 1
          && ai.handPoker.filter(p => p.m == x(3).m).length >= 1
          && ai.handPoker.filter(p => p.m == x.last.m + 1).length == 1
          && ai.handPoker.filter(p => p.m == x.last.m + 2).length == 1
          && ai.handPoker.filter(p => p.m == x.last.m + 3).length == 1
          & ai.handPoker.filter(p => p.m == x.last.m + 4).length == 1) {
          cnt = 4
        }

        if (cnt == 1) {
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          ai.handPoker --= x.takeRight(1)
          ai.handPoker += x.remove(0)

        } else if (cnt == 2) {
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          ai.handPoker --= x.takeRight(2)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)

        } else if (cnt == 3) {
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          ai.handPoker --= x.takeRight(3)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)

        } else if (cnt == 4) {

          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          x += ai.handPoker.filter(p => p.m == x.last.m + 1).head
          ai.handPoker --= x.takeRight(4)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)
          ai.handPoker += x.remove(0)
        }
      }
    }
  }

  //四张和三张组成的炸弹
  def combFourBoom(ai: AI) {

    val moreThanFourBoom = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 4)

    for ((x, y) <- moreThanFourBoom) {

      if (y == 4) {

        ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += ai.handPoker.filter(_.m == x))

      }
      ai.handPoker --= ai.handPoker.filter(_.m == x)
    }
    //癞子调优
    val moreThanThreeBoom = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 3)

    for ((x, y) <- moreThanThreeBoom if ai.lz.size > 0) {

      if (y == 3) {

        ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += (ai.handPoker.filter(_.m == x).toBuffer += ai.lz.remove(0)))

      }

      ai.handPoker --= ai.handPoker.filter(_.m == x)

    }
  }

  //三顺
  def combthreeSeq(ai: AI) {

    val moreThanThree = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 >= 3).toBuffer.sortWith((x, y) => x._1 < y._1)

    val three = Buffer[Buffer[Pok]]()

    val fly = Buffer[Buffer[Pok]]()

    for ((x, y) <- moreThanThree) {

      if (y < 6) {

        val t3 = ai.handPoker.filter(p => p.m == x).take(3)

        three += t3

        ai.handPoker --= t3

      } else if (y >= 6) {

        var t3 = ai.handPoker.filter(p => p.m == x).take(3)

        three += t3

        ai.handPoker --= t3

        t3 = ai.handPoker.filter(p => p.m == x).take(3)

        three += t3

        ai.handPoker --= t3

      }
    }

    val ct1 = three.clone

    val ct2 = three.clone

    val tmp = Buffer[Buffer[Pok]]()

    for (x <- ct1 if x != null) {

      breakable {

        for (y <- (ct2 -= x) if y != null) {

          if (x.head.m == y.head.m - 1) {

            fly += (x ++ y)

            tmp += x

            tmp += y

            ct1 -= y

            ct2 -= y

            break
          }
        }
      }
    }

    //主牌放到最后
    three --= tmp

    val master = three.filter(p => p.filter(_.m == ai.master).size > 0)

    three --= master

    three ++= master

    if (ai.lz.size > 0) {

      val moreThanTwo = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
        .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 2).toBuffer.sortWith((x, y) => x._1 < y._1)

      val doubleP = Buffer[Buffer[Pok]]()

      for ((x, y) <- moreThanTwo) {

        doubleP += ai.handPoker.filter(p => p.m == x)

      }

      val doubleTmp = Buffer[Buffer[Pok]]()

      val threeTmp = Buffer[Buffer[Pok]]()

      for (x <- doubleP if ai.lz.size > 0) {

        breakable {

          for (y <- three if y != null && ai.lz.size > 0) {

            if (x.head.m == y.head.m - 1) {

              fly += (x ++ Buffer(ai.lz.remove(0)) ++ y)

              doubleTmp += x

              threeTmp += y

              three -= y

              break

            } else if (x.head.m == y.head.m + 1) {

              fly += (y ++ x ++ Buffer(ai.lz.remove(0)))

              doubleTmp += x

              threeTmp += y

              three -= y

              break

            }
          }
        }
      }

      ai.handPoker --= doubleTmp.flatMap(x => x).toBuffer
    }

    if (ai.lz.size == 2) {

      val moreThanOne = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
        .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 1).toBuffer.sortWith((x, y) => x._1 < y._1)

      val oneP = Buffer[Buffer[Pok]]()

      for ((x, y) <- moreThanOne) {

        oneP += ai.handPoker.filter(p => p.m == x)

      }

      val oneTmp = Buffer[Buffer[Pok]]()

      val threeTmp = Buffer[Buffer[Pok]]()

      for (x <- oneP if ai.lz.size == 2) {

        breakable {

          for (y <- three if y != null && ai.lz.size > 0) {

            if (x.head.m == y.head.m - 1) {

              fly += (x ++ Buffer(ai.lz.remove(0), ai.lz.remove(0)) ++ y)

              oneTmp += x

              threeTmp += y

              three -= y

              break

            } else if (x.head.m == y.head.m + 1) {

              fly += (y ++ x ++ Buffer(ai.lz.remove(0), ai.lz.remove(0)))

              oneTmp += x

              threeTmp += y

              three -= y

              break
            }
          }
        }
      }
      ai.handPoker --= oneTmp.flatMap(x => x).toBuffer
    }

    ai.opt = ai.opt.updated(3, three)

    ai.opt = ai.opt.updated(33, fly)

  }

  //三连对
  def combdoubleSeq(ai: AI) {

    ai.handPoker = ai.handPoker.sortWith((x, y) => x.m < y.m).clone

    val e = Buffer[Buffer[Pok]]()

    val f = Buffer[Pok]()

    var g = 100

    while (ai.handPoker.length >= 6 && g != 0) {

      val k = Buffer[Buffer[Pok]]()

      g = 0

      for (x <- ai.handPoker) {

        if (f.length == 0) {

          f += x

        } else if (f.length == 6) {

          e += f.clone

          k += f.clone

          f.clear

          f += x

          g += 1

        } else if (f.length % 2 != 0 && x.m - f.last.m == 0) {

          f += x

        } else if (f.length % 2 == 0 && x.m - f.last.m == 1) {

          f += x

        } else {

          if (x.m - f.last.m != 0) {

            f.clear

            f += x

          }
        }
      }
      if (f.length == 6) {

        e += f.clone

        k += f.clone

        f.clear

        g += 1

      }

      ai.handPoker --= k.flatMap(x => x).toBuffer
    }

    //一个癞子调优
    val moreThanOne = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 >= 1).toBuffer.sortWith((x, y) => x._1 < y._1)

    val oneP = Buffer[Buffer[Pok]]()

    for ((x, y) <- moreThanOne) {

      oneP += ai.handPoker.filter(p => p.m == x)

    }

    f.clear

    breakable {

      for (x <- oneP if ai.lz.length > 0) {

        if (f.length == 5) {

          break

        }

        if (f.length == 0) {

          if (x.length > 1) {

            f ++= x.take(2)

          } else {

            f += x.head

          }
        } else if (f.length % 2 != 0 && x.head.m - f.last.m == 1 && x.length > 1) {

          f ++= x.take(2)

        } else if (f.length % 2 == 0 && x.head.m - f.last.m == 1 && x.length >= 1) {

          if (x.length > 1) {

            f ++= x.take(2)

          } else if (x.length == 1) {

            f ++= x.take(1)

          }
        } else if (f.length % 2 != 0 && x.length == 1) {

          f.clear

          f ++= x.take(1)

        }
      }
    }
    if (f.length == 5) {

      e += (f += ai.lz.remove(0))

      ai.handPoker --= e.flatMap(x => x).toBuffer

    }
    ai.opt = ai.opt.updated(222, e)
  }

  //三张
  def combthree(ai: AI) {

    val moreThanTree = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 3).toBuffer.sortWith((x, y) => x._1 < y._1)

    val threeP = Buffer[Buffer[Pok]]()

    for ((x, y) <- moreThanTree) {

      threeP += ai.handPoker.filter(p => p.m == x)

    }
    ai.handPoker --= threeP.flatMap(x => x).toBuffer

    ai.opt = ai.opt.updated(3, ai.opt.get(3).getOrElse(Buffer()) ++= threeP)
  }

  //对子
  def combDouble(ai: AI) {

    val moreThanTwo = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 2).toBuffer.sortWith((x, y) => x._1 < y._1)

    val twoP = Buffer[Buffer[Pok]]()

    for ((x, y) <- moreThanTwo) {

      twoP += ai.handPoker.filter(p => p.m == x)
    }

    ai.handPoker --= twoP.flatMap(x => x).toBuffer

    ai.opt = ai.opt.updated(2, twoP)

  }

  //单
  def combOne(ai: AI) {

    val moreThanOne = ai.handPoker.sortWith((x, y) => x.m < y.m).map((_, 1))
      .groupBy(_._1.m).map { x => (x._1, x._2.size) }.filter(_._2 == 1).toBuffer.sortWith((x, y) => x._1 < y._1)

    val oneP = Buffer[Buffer[Pok]]()

    for ((x, y) <- moreThanOne) {

      oneP += ai.handPoker.filter(p => p.m == x)

    }

    ai.handPoker --= oneP.flatMap(x => x).toBuffer

    ai.opt = ai.opt.updated(11, oneP)

  }

  def comb32(ai: AI) {

    val maxDouble = isCFTByType(ai, 2)
    val t3 = ai.opt.get(3).getOrElse(Buffer())
    val t2 = ai.opt.get(2).getOrElse(Buffer()).filter(x => x.head.m <= 10 && x.head.m != ai.master)

    val td = Buffer[Buffer[Pok]]()

    if (t3.length > 0 && t2.length > 0) {

      if (t3.length >= t2.length) {

        val three = ai.opt.get(3).get.take(t2.length)
        val double = t2

        val cd = double.clone
        for (x <- three) {
          td += (x ++ double.remove(0))
        }

        ai.opt = ai.opt.updated(3, ai.opt.get(3).getOrElse(Buffer()) --= three)
        ai.opt = ai.opt.updated(2, ai.opt.get(2).getOrElse(Buffer()) --= cd)
        ai.opt = ai.opt.updated(32, td)

      } else {
        val three = ai.opt.get(3).get.take(t3.length)
        val double = t2.take(t3.length)
        val cd = double.clone
        for (x <- three) {
          td += (x ++ double.remove(0))
        }
        ai.opt = ai.opt.updated(3, ai.opt.get(3).getOrElse(Buffer()) --= three)
        ai.opt = ai.opt.updated(2, ai.opt.get(2).getOrElse(Buffer()) --= cd)
        ai.opt = ai.opt.updated(32, td)
      }
    }
  }

  //根据记牌器找出当前组牌中的冲锋套
  def combCFT(ai: AI): TreeMap[Int, Buffer[Buffer[Pok]]] = {

    var jpqTmp = Buffer[Int]()

    //提出主牌
    for (x <- ai.jpq) {
      if (x == ai.master) {
        jpqTmp += 15
      } else {
        jpqTmp += x
      }
    }

    var maxV = 0

    var cft = TreeMap[Int, Buffer[Buffer[Pok]]]()
    //排除掉炸弹、同花顺、天王炸
    for (x <- ai.opt if x._2.length > 0 && x._1 != 1 && x._1 != 50 && x._1 / 10 != 4) {

      x._1 match {

        case 0 =>

        //单牌
        case 11 =>
          if (jpqTmp.length > 0) {
            maxV = jpqTmp.max
          } else {
            maxV = 0
          }
          val v1 = ai.opt.get(11).get.filter(p => p.head.m >= maxV && p.head.m != ai.master).toBuffer
          val v2 = ai.opt.get(11).get.filter(p => p.head.m == ai.master).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(11, cft.get(11).getOrElse(Buffer()) += x)
            }
          }
          if (v2.length > 0 && maxV <= 15) {
            for (x <- v2) {
              cft = cft.updated(11, cft.get(11).getOrElse(Buffer()) += x)
            }
          }

        //对子
        case 2 =>

          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).size > 0) {

            maxV = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).max._1
          } else {
            maxV = 0
          }

          val v1 = ai.opt.get(2).get.filter(p => p.head.m >= maxV && p.head.m != ai.master).toBuffer
          val v2 = ai.opt.get(2).get.filter(p => p.head.m == ai.master).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(2, cft.get(2).getOrElse(Buffer()) += x)
            }
          }
          if (v2.length > 0 && maxV <= 15) {
            for (x <- v2) {

              cft = cft.updated(2, cft.get(2).getOrElse(Buffer()) += x)
            }
          }

        case 3 =>

          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).size > 0) {

            maxV = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).max._1
          } else {
            maxV = 0
          }

          val v1 = ai.opt.get(3).get.filter(p => p.head.m >= maxV && p.head.m != ai.master).toBuffer
          val v2 = ai.opt.get(3).get.filter(p => p.head.m == ai.master).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(3, cft.get(3).getOrElse(Buffer()) += x)
            }
          }
          if (v2.length > 0 && maxV <= 15) {
            for (x <- v2) {

              cft = cft.updated(3, cft.get(3).getOrElse(Buffer()) += x)
            }
          }

        case 32 =>
          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).size > 0 && jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).size > 1) {

            maxV = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).max._1
          } else {
            maxV = 0
          }
          val v1 = ai.opt.get(32).get.filter(p => p.head.m >= maxV && p.head.m != ai.master).toBuffer
          val v2 = ai.opt.get(32).get.filter(p => p.head.m == ai.master).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(32, cft.get(32).getOrElse(Buffer()) += x)
            }
          }
          if (v2.length > 0 && maxV <= 15) {
            for (x <- v2) {

              cft = cft.updated(32, cft.get(32).getOrElse(Buffer()) += x)
            }
          }

        case 33 =>
          var allThree = Buffer[Int]()
          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).size > 1) {

            allThree = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 3).keySet.toBuffer.sortWith(_ < _)

            val seq33 = allThree.sliding(2, 1).toBuffer.filter(x => x(1) - x(0) == 1)

            if (seq33.length > 0) {

              maxV = seq33.last.min
            } else {
              maxV = 0
            }

          } else {
            maxV = 0
          }

          val v1 = ai.opt.get(33).get.filter(p => p.head.m >= maxV).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(33, cft.get(33).getOrElse(Buffer()) += x)
            }
          }

        case 51 =>

          var allSeq = Buffer[Int]()

          //当顺子用的时候主牌就是原来大小
          jpqTmp = ai.jpq

          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 1).size >= 5) {

            allSeq = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 1).keySet.toBuffer.sortWith(_ < _)

            val seq51 = allSeq.sliding(5, 1).toBuffer.filter(x => x(4) - x(0) == 4)

            if (seq51.length > 0) {

              maxV = seq51.last.max
            } else {
              maxV = 0
            }

          } else {
            maxV = 0
          }

          val v1 = ai.opt.get(51).get.filter(p => (p.last.m != ai.master && p.last.m >= maxV) || (p(3).m - p(0).m == 3 && p.last.m == ai.master && p(3).m + 1 >= maxV) || (p(3).m - p(0).m > 3 && p.last.m == ai.master && p(3).m >= maxV)).toBuffer

          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(51, cft.get(51).getOrElse(Buffer()) += x)
            }
          }

        case 222 =>

          var allTwo = Buffer[Int]()
          if (jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).size > 2) {

            allTwo = jpqTmp.map((_, 1)).groupBy(_._1).map(x => (x._1, x._2.size)).filter(_._2 >= 2).keySet.toBuffer.sortWith(_ < _)

            val seq222 = allTwo.sliding(3, 1).toBuffer.filter(x => x(2) - x(0) == 2)

            if (seq222.length > 0) {

              maxV = seq222.last.min
            } else {
              maxV = 0
            }

          } else {
            maxV = 0
          }

          val v1 = ai.opt.get(222).get.filter(p => p.head.m >= maxV).toBuffer
          if (v1.length > 0) {
            for (x <- v1) {
              cft = cft.updated(222, cft.get(222).getOrElse(Buffer()) += x)
            }
          }
      }
    }

    cft

  }

  //非冲锋套
  def combNCFT(ai: AI): TreeMap[Int, Buffer[Buffer[Pok]]] = {
    var ncft = TreeMap[Int, Buffer[Buffer[Pok]]]()
    val cft = combCFT(ai)

    for (x <- ai.opt.filter(x => x._1 != 1 && x._1 != 50 && x._1 / 10 != 4)) {
      for (y <- x._2) {

        if (!cft.get(x._1).getOrElse(Buffer()).contains(y)) {

          ncft = ncft.updated(x._1, ncft.get(x._1).getOrElse(Buffer()) += y)
        }
      }
    }
    ncft
  }

  //炸弹
  def combAllBOOM(ai: AI): TreeMap[Int, Buffer[Buffer[Pok]]] = {

    var boom = TreeMap[Int, Buffer[Buffer[Pok]]]()

    val cft = combCFT(ai)

    for (x <- ai.opt.filter(x => x._1 == 1 || x._1 == 50 || x._1 / 10 == 4)) {
      for (y <- x._2) {

        boom = boom.updated(x._1, boom.get(x._1).getOrElse(Buffer()) += y)

      }
    }
    boom
  }

  def isCFTByType(ai: AI, px: Int): Int = {
    if (px == 2) {

      val maxDoubleArr = ai.jpq.map((_, 1))
        .groupBy(_._1).map { x => (x._1, x._2.size) }.filter(_._2 >= 2).toBuffer.sortWith((x, y) => x._1 < y._1)

      var maxDouble = 0

      if (maxDoubleArr.length == 0) {
        0
      } else {
        maxDouble = maxDoubleArr.last._1
      }

      val masterSize = ai.jpq.filter(_ == ai.master).length

      if (masterSize >= 2) {
        15
      } else {
        maxDouble
      }
    } else {
      0
    }
  }

  //判断牌型
  def judgeTp(h: Buffer[Pok]) {

    val m = h.groupBy(x => x.m).map(x => (x._1, x._2.size)).toBuffer.sortWith((x, y) => x._1 < y._1)

    val c = h.groupBy(x => x.c).map(x => (x._1, x._2.size))

    println(m, c)

  }

  def combPoker(ai: AI) {

    val lz = ai.lz.clone
    val tmphp = ai.handPoker.clone
    var tmpOpt = TreeMap[Int, Buffer[Buffer[Pok]]]()
    var cnt = 0

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }

    combSupperBoom(ai)
    combFiveBoom(ai)
    combSeqColour(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combSeq(ai)
    combFourBoom(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> A")
    //ai.opt.flatMap(_._2).foreach(println)
    for (x <- ai.opt) {
      tmpOpt = tmpOpt.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combFourBoom(ai)
    combSeqColour(ai)
    combFiveBoom(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combSeq(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> B")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(tmpOpt, ai.opt)

    var ob = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      ob = ob.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combFourBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combFiveBoom(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> C")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(ob, ai.opt)
    var oc = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      oc = oc.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combFiveBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combFourBoom(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> D")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(oc, ai.opt)
    var od = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      od = od.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combFiveBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combFourBoom(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> E")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(od, ai.opt)
    var oe = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      oe = oe.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combFiveBoom(ai)
    combFourBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> F")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(oe, ai.opt)
    var of = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      of = of.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combFiveBoom(ai)
    combFourBoom(ai)
    combSeq(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> G")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(of, ai.opt)
    var og = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      og = og.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combFiveBoom(ai)
    combSeq(ai)
    combFourBoom(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> H")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(og, ai.opt)
    var oh = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      oh = oh.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combFiveBoom(ai)
    combSeq(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combFourBoom(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> I")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(oh, ai.opt)
    var oi = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      oi = oi.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combFiveBoom(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combFourBoom(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> J")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(oi, ai.opt)
    var oj = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      oj = oj.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combthreeSeq(ai)
    combdoubleSeq(ai)
    combFiveBoom(ai)
    combFourBoom(ai)
    combthree(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> K")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(oj, ai.opt)
    var ok = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      ok = ok.updated(x._1, x._2.clone)
    }
    //println()

    for (x <- ai.opt) {
      ai.opt.get(x._1).get.clear()
    }
    ai.lz.clear
    ai.lz ++= lz.clone
    ai.handPoker = tmphp.clone
    combSupperBoom(ai)
    combSeqColour(ai)
    combSeq(ai)
    combFiveBoom(ai)
    combFourBoom(ai)
    combthreeSeq(ai)
    combthree(ai)
    combdoubleSeq(ai)
    combDouble(ai)
    combOne(ai)
    comb32(ai)
    cnt = ai.opt.flatMap(_._2).size
    //println(cnt, ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, combCFT(ai).flatMap(_._2).size, "-----> L")
    //ai.opt.flatMap(_._2).foreach(println)
    tmpOpt = searchBetter(ok, ai.opt)
    var ol = TreeMap[Int, Buffer[Buffer[Pok]]]()
    for (x <- tmpOpt) {
      ol = ol.updated(x._1, x._2.clone)
    }
    //println()

    ai.opt = ol
    //顺子调优
    for (x <- ai.opt.get(51).getOrElse(Buffer()) if x.filter(p => p.m == ai.master).length == 0) {
      if (x.head.m == ai.master) {
        if (ai.opt.get(11).getOrElse(Buffer()).length > 0) {
          //找出一张可以接上原来顺子的大牌
          val od = ai.opt.get(11).get.flatMap(x => x).filter(y => y.m == x.last.m + 1).take(1)

          if (od.length > 0) {
            val tm = x.head
            x -= x.head
            x ++= od
            ai.opt.get(11).get -= od
            ai.opt.get(11).get += Buffer(tm)
          }
        }
      }
    }

    //同花顺子调优
    for (x <- ai.opt.get(50).getOrElse(Buffer())) {
      if (x.head.m == ai.master) {
        if (ai.opt.get(11).getOrElse(Buffer()).length > 0) {
          //找出一张可以接上原来顺子的大牌
          val od = ai.opt.get(11).get.flatMap(x => x).filter(y => y.m == x.last.m + 1 && y.c == x.last.c && y.c == x.head.c).take(1)

          if (od.length > 0) {
            val tm = x.head
            x -= x.head
            x ++= od
            ai.opt.get(11).get -= od
            ai.opt.get(11).get += Buffer(tm)
          }
        }
      }
    }

    ai.lz = lz.clone -- ai.opt.flatMap(_._2).flatMap(x => x)
    //println("ultimate opt (请测试注意： 此阶段会包含癞子对4张及以上炸弹、拆天王炸的最后调优)=====> ", ai.opt.flatMap(_._2).flatMap(x => x).size + ai.lz.size, ai.opt.flatMap(_._2).size)

    freeLz(ai)

    //最后调整一下王炸

    if (ai.opt.get(11).getOrElse(Buffer()).size + ai.opt.get(2).getOrElse(Buffer()).size > 0) {

      if (ai.opt.get(1).getOrElse(Buffer()).size > 0) {

        ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(Buffer()) += Buffer(Pok(100, 0)))
        ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(Buffer()) += Buffer(Pok(100, 0)))
        ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(Buffer()) += Buffer(Pok(99, 0)))
        ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(Buffer()) += Buffer(Pok(99, 0)))
        ai.opt.get(1).get.clear

      }
    }

    //顺子调优

    if (ai.opt.get(12345).getOrElse(Buffer()).length > 0 && ai.opt.get(11).getOrElse(Buffer()).length > 0) {

      for (x <- ai.opt.get(11).get) {

        x.head.m
      }
    }

    //ai.opt.flatMap(_._2).foreach(println)

  }

  def freeLz(ai: AI) {

    while (ai.lz.length > 0) {
      if (ai.opt.get(2).getOrElse(Buffer()).filter(x => x.filter(y => y.m != 100 && y.m != 99).size > 0).length > 0) {
        val ss = ai.opt.get(2).getOrElse(Buffer()).filter(x => x.filter(y => y.m != 100 && y.m != 99).size > 0)
        val du = ss.last.clone
        ai.opt.get(2).get -= du

        if (combNCFT(ai).get(2).getOrElse(Buffer()).length > 0) {
          val sd2 = combNCFT(ai).get(2).get.head
          ai.opt.get(2).get -= sd2
          ai.opt = ai.opt.updated(32, ai.opt.get(32).getOrElse(Buffer()) += ((du += ai.lz.remove(0)) ++= sd2))
        } else {
          ai.opt = ai.opt.updated(3, ai.opt.get(3).getOrElse(Buffer()) += (du += ai.lz.remove(0)))
        }

      } else if (ai.opt.get(11).getOrElse(Buffer()).filter(x => x.filter(y => y.m != 100 && y.m != 99).size > 0).length > 0) {
        val ss = ai.opt.get(11).getOrElse(Buffer()).filter(x => x.filter(y => y.m != 100 && y.m != 99).size > 0)
        val si = ss.last.clone
        ai.opt.get(11).get -= si
        ai.opt = ai.opt.updated(2, ai.opt.get(2).getOrElse(Buffer()) += (si += ai.lz.remove(0)))
      } else {
        if (ai.opt.get(44).getOrElse(Buffer()).size > 0) {
          val b44 = ai.opt.get(44).get
          val b44l = b44.last.clone
          ai.opt.get(44).get -= b44l
          ai.opt = ai.opt.updated(45, ai.opt.get(45).getOrElse(Buffer()) += (b44l += ai.lz.remove(0)))
        } else if (ai.opt.get(45).getOrElse(Buffer()).size > 0) {
          val b45 = ai.opt.get(45).get
          val b45l = b45.last.clone
          ai.opt.get(45).get -= b45l
          ai.opt = ai.opt.updated(46, ai.opt.get(46).getOrElse(Buffer()) += (b45l += ai.lz.remove(0)))
        } else if (ai.opt.get(46).getOrElse(Buffer()).size > 0) {
          val b46 = ai.opt.get(46).get
          val b46l = b46.last.clone
          println(b46l, ai.lz)
          ai.opt.get(46).get -= b46l
          ai.opt = ai.opt.updated(47, ai.opt.get(47).getOrElse(Buffer()) += (b46l += ai.lz.remove(0)))
        } else if (ai.opt.get(47).getOrElse(Buffer()).size > 0) {
          val b47 = ai.opt.get(47).get
          val b47l = b47.last.clone
          ai.opt.get(47).get -= b47l
          ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += b47l.take(4))
          ai.opt = ai.opt.updated(44, ai.opt.get(44).getOrElse(Buffer()) += (b47l.takeRight(3).toBuffer += ai.lz.remove(0)))

          //ai.opt = ai.opt.updated(48, ai.opt.get(48).getOrElse(Buffer()) += (b47l += ai.lz.remove(0)))
        } else {
          ai.opt = ai.opt.updated(11, ai.opt.get(11).getOrElse(Buffer()) += Buffer(ai.lz.remove(0)))
        }
      }
    }
  }

  def searchBetter(oT: TreeMap[Int, Buffer[Buffer[Pok]]], nT: TreeMap[Int, Buffer[Buffer[Pok]]]): TreeMap[Int, Buffer[Buffer[Pok]]] = {

    val oBCnt = oT.filter(x => x._1 == 1 || x._1 == 50 || x._1 / 10 == 4).flatMap(_._2).size
    val nBCnt = nT.filter(x => x._1 == 1 || x._1 == 50 || x._1 / 10 == 4).flatMap(_._2).size

    val otCnt = oT.flatMap(_._2).size
    val ntCnt = nT.flatMap(_._2).size

    if (otCnt - 2 * oBCnt < ntCnt - 2 * nBCnt) {
      oT
    } else {
      nT
    }
  }

  def initComb() {

  }

  //进贡,给出去的牌可能抗贡，这由服务端现行判断之后，如果抗贡就不需要调用此方法！！！ 否则 ai.handPoker -= payPok 之后需要再次把牌还回去！！！
  def payPok(ai: AI): Pok = {

    val s = ai.handPoker.filter(_ != Pok(ai.master, 2)).toBuffer
    var payPok = Pok(0, 0)
    if (s.contains(Pok(100, 0))) {
      return Pok(100, 0)
    } else if (s.contains(Pok(99, 0))) {
      return Pok(99, 0)
    } else if (s.contains(Pok(ai.master, 1))) {
      payPok = Pok(ai.master, 1)
    } else if (s.contains(Pok(ai.master, 3))) {
      payPok = Pok(ai.master, 3)
    } else if (s.contains(Pok(ai.master, 4))) {
      payPok = Pok(ai.master, 4)
    } else {
      payPok = s.maxBy(_.m)
    }
    //ai.handPoker -= payPok
    val cs = findCS(payPok, ai)
    ai.handPoker -= cs
    cs
  }

  //给出不能组成同花的最大牌
  def findCS(pok: Pok, ai: AI): Pok = {

    var rePayPok = Pok(0, 0)

    val moreThanTwo = ai.handPoker.filter(p => p.m == pok.m)

    val tmpAI = new AI()

    tmpAI.handPoker = ai.handPoker.clone
    tmpAI.lz = ai.lz

    combSeqColour(tmpAI)

    val t = tmpAI.opt.get(50).getOrElse(Buffer()).clone

    for (z <- moreThanTwo) {

      var isContains = false

      for (x <- t if x != null) {

        //如果z存在某一个同花顺
        if (x.contains(z)) {
          t -= x
          isContains = true
        }
      }
      //所有同花顺都不包含z
      if (!isContains) return z
    }

    //所有都Z都存在某一个同花顺，随机拿一个z出去
    if (rePayPok == Pok(0, 0)) {
      rePayPok = pok
    }
    rePayPok
  }

  //AI进贡者进贡后得到还牌，AI受贡者得到进攻者换牌，均调用此方法
  def receivePok(ai: AI, pok: Pok) {

    ai.handPoker += pok

  }

  //还贡
  /**
   * 1）进贡：给出一张除红桃主牌外最大的牌，如果此牌大于一张选出不能组成同花的那张，如果都能组成，任意选一张。
   * 2）还贡：对手牌进行组套后，
   * a)	若手中有标签小于K的单牌，则还其中标签最小的一张，否则
   * b)	若手中有标签小于K的对牌，则将其中标签最小的一套拆为两张单牌，并还出其中的一张单，否则
   * c)	若手中有标签小于K的三张，则将其中标签最小的一套拆分为一张单牌和一套对牌，并还出其中的单牌，否则
   * d)	若手中有长度大于等于5且不含癞子的炸弹，则将其中标签最小的一套拆分为一张单牌和一套炸弹，并还出其中的单牌
   * e)	不考虑组牌，还出手中标签最小的一张牌
   *
   */
  def rePayPok(ai: AI): Pok = {

    var p = Pok(0, 0)

    if (ai.opt.get(11).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).length > 0) {
      p = ai.opt.get(11).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(2).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).length > 0) {
      p = ai.opt.get(2).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(3).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).length > 0) {
      p = ai.opt.get(3).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 13 && x.m != ai.master).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(45).getOrElse(Buffer()).flatMap(x => x).length > 0) {
      p = ai.opt.get(45).getOrElse(Buffer()).flatMap(x => x).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(46).getOrElse(Buffer()).flatMap(x => x).length > 0) {
      p = ai.opt.get(46).getOrElse(Buffer()).flatMap(x => x).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(47).getOrElse(Buffer()).flatMap(x => x).length > 0) {
      p = ai.opt.get(47).getOrElse(Buffer()).flatMap(x => x).minBy(_.m).asInstanceOf[Pok]
    } else if (ai.opt.get(48).getOrElse(Buffer()).flatMap(x => x).length > 0) {
      p = ai.opt.get(48).getOrElse(Buffer()).flatMap(x => x).minBy(_.m).asInstanceOf[Pok]
    } else {

      p = ai.opt.flatMap(_._2).flatMap(x => x).filter(x => x.m != ai.master).minBy(_.m)
    }

    p
  }

  /**
   * 点数：组牌结束后，按以下方式计算总点数，每个炸弹4点，每套冲锋套1点，对主牌1点，连对1点，钢板1点，逢人配1点，累赘牌每套-1点，不重复计点；
   */

  def countT(ai: AI): Int = {

    var n1 = combAllBOOM(ai).flatMap(_._2).size * 4 + combCFT(ai).flatMap(_._2).size

    val gb = ai.opt.get(33).getOrElse(Buffer())

    val ld = ai.opt.get(222).getOrElse(Buffer())

    val dz = ai.opt.get(2).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m == ai.master).size / 2

    val m3 = ai.opt.get(3).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m == ai.master).size / 3

    val badS = combNCFT(ai).get(11).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 10 && x.m != ai.master).size

    val badD = combNCFT(ai).get(2).getOrElse(Buffer()).flatMap(x => x).filter(x => x.m < 6 && x.m != ai.master).size / 2

    for (x <- gb) {
      if (!combCFT(ai).flatMap(_._2).toBuffer.contains(x)) {
        n1 += 1
      }
    }

    for (x <- ld) {
      if (!combCFT(ai).flatMap(_._2).toBuffer.contains(x)) {
        n1 += 1
      }
    }

    if (ai.jpq.sortWith(_ < _).containsSlice(Buffer(99, 99)) || ai.jpq.sortWith(_ < _).containsSlice(Buffer(100, 100))) {

      n1 += dz
    }

    n1 -= (badS + badD)

    n1
  }

}

object Mapp {

  def main(args: Array[String]): Unit = {
    import com.gus.ai.Comb._

    val ai = new AI

    val list = Buffer[Int]()

    for (x <- 2 to 14) {

      list += (x * 10 + 1, x * 10 + 1, x * 10 + 2, x * 10 + 2, x * 10 + 3, x * 10 + 3, x * 10 + 4, x * 10 + 4)

    }

    list += 99

    list += 99

    list += 100

    list += 100

    //随机洗牌
    //        val initP = util.Random.shuffle(list).take(27).sortWith(_ < _)

    //       list --= initP

    //**************************************************************************************************初始化手牌
    val initP = Buffer(34, 101, 112, 131, 141, 122).sortWith(_ < _)

    list --= initP
    //val initP = Buffer(21,22, 31,31,32,41,53,61,71,81,91,91,93).sortWith(_ < _)

    println("initP===========>", initP.sortWith(_ < _))

    //############################设置主牌(M)
    ai.master = 12

    //############################取出癞子(M)
    val lzLen = initP.filter(_ == ai.master * 10 + 2).size

    initP --= initP.filter(_ == ai.master * 10 + 2).toBuffer

    if (lzLen == 1) {

      ai.lz += Pok(ai.master, 2)

    } else if (lzLen == 2) {

      ai.lz ++= Buffer(Pok(ai.master, 2), Pok(ai.master, 2))

    }

    val tmpLz = ai.lz.clone

    //=====================================================组牌==========================================================================
    val tmpHp = Buffer[Pok]()

    initP.map(x => if (x != 99 && x != 100) { ai.handPoker += Pok(x / 10, x % 10); tmpHp += Pok(x / 10, x % 10) } else { ai.handPoker += Pok(x, 0); tmpHp += Pok(x, 0) })

    ai.handPoker = ai.handPoker.sortWith((x, y) => x.m < y.m)
    println("ai.handPoker===========>  ", ai.handPoker)

    //记牌器中的牌不要花色
    //ai.jpq = list.map { x => if (x != 99 && x != 100) { x / 10 } else { x } }
    ai.jpq = Buffer(5, 5, 5, 5, 8, 8, 9, 9, 9, 10, 11, 12, 2, 2, 100)
    //**************************************************************************************************初始化手牌结束

    //28->27 还贡
    //combPoker(ai)
    //println(rePayPok(ai))

    //27->26 进贡
    //println(payPok(ai))

    //进还贡演示 可选,这两个逻辑不能同时打开===============================================================================================================================
    //如果AI头游 二游逻辑
    //    receivePok(ai, Pok(100, 0))
    //    tmpHp += Pok(100, 0)
    //    //ai.handPoker = tmpHp.clone
    //
    //    combPoker(ai)
    //    ai.lz = tmpLz
    //    val rePay = rePayPok(ai)
    //    tmpHp -= rePay
    //    println(rePay, "=========================>>>")
    //    ai.handPoker = tmpHp
    //
    //    println(ai.handPoker)
    //    combPoker(ai)

    //如果AI三、四游逻辑

    //    val g = payPok(ai)
    //    tmpHp -= g
    //    println(g, "--------------->>>>")
    //
    //    ai.lz = tmpLz
    //    receivePok(ai, Pok(4, 1))
    //    tmpHp += Pok(4, 1)
    //    ai.handPoker = tmpHp
    //
    //    combPoker(ai)

    //=====================================================组牌==========================================================================

    //    ai.handPoker =  Buffer(Pok(2,2), Pok(2,2), Pok(5,2), Pok(5,3), Pok(6,1), Pok(6,2), Pok(7,3), Pok(7,4), Pok(9,1), Pok(9,2), Pok(9,3), Pok(10,1), Pok(11,2), Pok(12,3), Pok(13,1), Pok(14,1))
    //    ai.lz = Buffer(Pok(3,2))
    //    combSupperBoom(ai)
    //    combFiveBoom(ai)
    //    combSeqColour(ai)
    //    combthreeSeq(ai)
    //    combdoubleSeq(ai)
    //    combthree(ai)
    //    combSeq(ai)
    //    combFourBoom(ai)
    //    combDouble(ai)
    //    combOne(ai)
    //    comb32(ai)
    //    
    //    ai.opt.flatMap(_._2).foreach(println)

    //combSeqColour(ai)

    //combSeq(ai)

    combPoker(ai)
    //
    println("\n\n显示冲锋套=======>")
    combCFT(ai).flatMap(_._2).foreach(println)
    println("显示非冲锋套=======>")
    combNCFT(ai).flatMap(_._2).foreach(println)
    println("显示炸弹=======>")
    combAllBOOM(ai).flatMap(_._2).foreach(println)
    println("总牌力点数：=====>>>", countT(ai))

  }
}