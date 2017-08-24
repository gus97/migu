package com.gus.ai

import com.gus.ai.CommbRule._
import com.gus.ai.NComm._
import java.util.TreeMap
import scala.collection.mutable.ArrayBuffer

class Service {

  //获得初始牌,初始化记牌器
  //ab 接口获得的初始手牌
  def setHandPoke(ai: AI, ab: ArrayBuffer[Int]) {
    //设置初始手牌
    segmentConvert(ab, ai)
    //组牌
    ai.opt = commb(ai.handPoker, ai.jpq)
  }

  //叫地主
  def canCall(ai: AI): Boolean = {
    if (getKZS(ai.handPoker) >= 1) {
      true
    } else {
      false
    }
  }

  //抢地主
  def canForestall(ai: AI): Boolean = {
    if (getKZS(ai.handPoker) > 1.8) {
      true
    } else {
      false
    }
  }

  //加倍
  def canDouble(ai: AI): Boolean = {
    if (getKZS(ai.handPoker) > 2.5) {
      true
    } else {
      false
    }
  }

  //获取三张底牌 这三张牌
  def getThreeCards(ai: AI, ab: ArrayBuffer[Int]) {
    //更新出牌权
    ai.h = 0
    //更新手牌，原来有花色手牌加有花色底牌
    segmentConvert(ai.hcPoker ++= ab, ai)
    //更新AI记牌器
    ai.jpq --= ai.ncPoker

  }

  //出牌通知
  //role 0：地主出牌 1：地主上家出牌 2：地主下家出牌  (role可能需要转换)
  //ab 当前玩家打出的牌，AI本身出牌也需要调用一下
  def playHandNotify(ai: AI, role: Int, ab: ArrayBuffer[Int]) {
    //更新AI记牌器
    ai.jpq --= ab
    //更新当前出牌的角色
    ai.h = role
    //更新当前台面打出的牌
    ai.t = ab
    if (role == 0) {
      ai.dz -= ab.length
    } else if (role == 1) {
      ai.n1 -= ab.length
    } else if (role == 2) {
      ai.n2 -= ab.length
    } else {

      throw new Exception("")
    }
  }

  def getStatus(ai: AI): ArrayBuffer[Any] = {
    val status = ArrayBuffer[Any]()

    if (ai.r != 0) {
      if (isGF(ai) == 1 || ai.zdnm == 1) {
        ai.zdnm = 1
      } else {
        ai.zdnm = 0
      }
    }

    status += ai.zdnm
    status += ai.opt.flatMap(_._2)
    status += ai.opt.flatMap(_._2).size
    status += getCFT(ai).flatMap(_._2).size
    status += ai.opt.get(1).get.size + ai.opt.get(4).get.size
    status
  }
  //  //开始组牌
  //  //获得当前最优组牌是 ai.opt 处理后可以入库
  //  def commbHandPoker(ai: AI) {
  //    ai.opt = commb(ai.handPoker, ai.jpq)
  //  }

  //出牌
  //出牌前可以查看当前最优组牌是 ai.opt 处理后可以入库
  def playHand(ai: AI, af: AFun, nf: NFun): ArrayBuffer[Int] = {

    if (ai.n1 == 0 || ai.n2 == 0 || ai.dz == 0) {
      println("牌局结束")
      return ArrayBuffer(-1)
    }

    if (ai.t.sortWith(_ < _).containsSlice(ArrayBuffer(99, 100)) && ai.r != ai.h) {
      return ArrayBuffer()
    }

    var hit = ArrayBuffer[Int]()

    //优化处理
    if (ai.r != 0 && ai.r != ai.h) {
      val p = optimizeHp(ai)
      if (p.size > 0) {
        return colourConvertN(p, ai)
      }
    } else if (ai.r == ai.h) {
      val p = optZD(ai)
      if (p.size > 0) {
        return colourConvertN(p, ai)
      }
    }

    if (ai.r != 0) {
      if (isGF(ai) == 1 || ai.zdnm == 1) {
        ai.zdnm = 1
      } else {
        ai.zdnm = 0
      }
    }

    println("角色,战斗,上一轮打牌角色", ai.r, ai.zdnm, ai.h, "---->")

    if (ai.r == 0 && ai.h == 0) { //地主主动
      hit = af.activeDz(ai)
    } else if (ai.r == 0 && ai.h != 0) { //地主被动

      hit = af.passiveDz(ai)
    } else if (ai.dz == 1 && ai.r != 0 && ai.r == ai.h) { //农民面对地主报单主动

      hit = nf.activeFaceLandlordCallOne(ai)
    } else if (ai.r == 2 && ai.n1 == 1 && ai.h == 2) { //地主下家面对地主上家报单主动出牌
      hit = nf.activeDownFaceUpCallOne(ai)
    } else if (ai.dz == 2 && ai.r != 0 && ai.r == ai.h) { //农民面对地主报双主动出牌

      hit = nf.activeFaceLandlordCallTwo(ai)
    } else if (ai.r == 1 && ai.n2 == 1 && ai.h == 1) { //上家农民面对下家农民报单 

      hit = nf.activeUpFaceDownCallOne(ai)
    } else if (ai.r == 2 && ai.zdnm == 1 && ai.h == 2) { //下家战斗农民主动出牌
      hit = nf.activeDownF(ai)
    } else if (ai.r == 2 && ai.zdnm == 0 && ai.h == 2) { //下加垃圾农民主动出牌
      hit = nf.activeDownR(ai)
    } else if (ai.r == 1 && ai.zdnm == 1 && ai.h == 1) { //上家战斗农民主动出牌
      hit = nf.activeUpF(ai)
    } else if (ai.r == 1 && ai.zdnm == 0 && ai.h == 1) { //上家垃圾农民主动出牌
      hit = nf.activeUpR(ai)
    } else if (ai.dz == 1 && ai.r != 0 && ai.r != ai.h) { //农民面对地主报单被动出牌

      hit = nf.passiveFaceLandlordCallOne(ai)
    } else if (ai.r == 2 && ai.n1 == 1 && ai.h != 2) { //地主下家面对地主上家报单被动
      hit = nf.passiveDownFaceUpCallOne(ai)
    } else if (ai.dz == 2 && ai.r != 0 && ai.r != ai.h) { //农民面对地主报双被动出牌
      hit = nf.passiveFaceLandlordCallTwo(ai)
    } else if (ai.r == 1 && ai.n2 == 1 && ai.h != 1) { //地主上家农民面对地主下家农民报单被动出牌
      hit = nf.passiveUpFaceDownCallOne(ai)
    } else if (ai.r == 2 && ai.zdnm == 1 && ai.h != 2) { //地主下家战斗农民被动出牌
      hit = nf.passiveDownF(ai)
    } else if (ai.r == 2 && ai.zdnm == 0 && ai.h != 2) { //地主下家垃圾农民被动出牌
      hit = nf.passiveDownR(ai)
    } else if (ai.r == 1 && ai.zdnm == 1 && ai.h != 1) { //地主上家战斗农民被动出牌
      hit = nf.passiveUpF(ai)
    } else if (ai.r == 1 && ai.zdnm == 0 && ai.h != 1) { //地主上家垃圾农民被动出来
      hit = nf.passiveUpR(ai)
    } else {
      throw new Exception("")
    }

    /*
     * ==================================================
     * 外部业务处理打出的牌hit
     * ==================================================
     */
    //内部处理

    //更新当前出牌的角色
    //    ai.h = ai.r
    //    if (ai.r == 0) {
    //      ai.dz -= hit.length
    //    } else if (ai.r == 1) {
    //      ai.n1 -= hit.length
    //    } else if (ai.r == 2) {
    //      ai.n2 -= hit.length
    //    }

    colourConvertN(hit, ai)
  }

  //AI被动出牌验证
  //ab为playHand 打出的牌，带花色的
  def check(ab: ArrayBuffer[Int], ai: AI): Boolean = {

    if (ai.r == ai.h || ab.length == 0) { return true }

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
    //    println(px, tpx, pFirst > tFirst, ai.t.size == ab.size)
    if (px == tpx && pFirst > tFirst && ai.t.size == ab.size) {
      return true
    } else {
      return false
    }
  }
}

//调用示例
object M67 {

  def main(args: Array[String]): Unit = {

    val serv = new Service

    val af = new AFun
    val nf = new NFun
    val ai = new AI()

    //发牌给AI
    var initPoker = ArrayBuffer(61, 101, 102, 103, 92, 91, 93, 131, 132, 133)
    //设置AI记牌器
    ai.jpq = ArrayBuffer(3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7, 11, 11, 11, 12, 13, 14, 14, 14, 15, 15, 100)
    // 设置AI手牌 
    serv.setHandPoke(ai, initPoker)

    ai.opt.flatMap(_._2).foreach(println)

    //顺序调用可否叫地主，抢地主，加倍
    //println(serv.canCall(ai))
    //
    //    //如过最终抢得地主
    //    val three = ArrayBuffer(99, 121, 122) //三张底牌
    //    serv.getThreeCards(ai, three)

    //出牌
    ai.r = 1
    ai.h = 0
    ai.n1 = 13
    ai.n2 = 10
    ai.dz = 15
    ai.t = ArrayBuffer(8, 8, 8)
    val hit = serv.playHand(ai, af, nf)
    println(hit, "----->")
    ai.opt.flatMap(_._2).foreach(println)
  }
}