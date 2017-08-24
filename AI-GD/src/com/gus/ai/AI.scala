package com.gus.ai

import scala.collection.mutable.Buffer
import scala.collection.immutable.TreeMap

class AI {

  //牌对象
  //case class P(var m: Int, c: Int)
  //癞子
  var lz = Buffer[Pok]()

  //默认主牌
  var master: Int = 2

  //初始手牌
  var handPoker = Buffer[Pok]()
  
  //
  var tmpHandPock = Buffer[Pok]()

  //AI的最优组牌
  var opt = new TreeMap[Int, Buffer[Buffer[Pok]]]

  //初始4家手牌数 
  //AI
  var mm = 27
  //对家
  var mo = 27
  //下家
  var mn = 27
  //上家
  var mp = 27
  
  //当前AI组牌中的冲锋套
  val cft = Buffer[Buffer[Pok]]()
  
  //自己和对家的强牌型
  val strongPx = Buffer[Int]()
  
  //自己和对家都的弱牌型
  val weakPx = Buffer[Int]()
  
  //记牌器
  var jpq = Buffer[Int]()
  
  //出牌记录仪,1牌数量，2，炸弹扔掉几个 3，打出的牌型|标签，4，走活的牌型
  val n1 = TreeMap[Int,Buffer[Int]]()
  val f1 = Buffer[Int]()
  val n2 = Buffer[Int]()
}