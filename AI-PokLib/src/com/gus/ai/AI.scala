package com.gus.ai

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.TreeMap
import scala.collection.immutable.Map
import java.util.HashMap
import scala.collection.mutable.Buffer

class AI() {
  //AI角色 0地主家 1地主上家 2地主下家
  var r = -1

  //当前出牌的角色 0地主家 1地主上家 2地主下家
  var h = 0

  //地主上家农民牌数
  var n1 = 17

  //地主下家农民牌数
  var n2 = 17

  //地主牌数
  var dz = 20

  //战斗农民，1是战斗农民 0垃圾农民
  var zdnm = 0

  //当前台面上的牌
  var t = ArrayBuffer[Int]()

  //记牌器中的牌，在外部初始化
  var jpq = ArrayBuffer[Int]()

  //最优组牌
  var opt = TreeMap[Int, ArrayBuffer[ArrayBuffer[Int]]]()

  //期望输出
  var exp = ArrayBuffer[Int]()

  //AI 当前手牌
  var handPoker = ArrayBuffer[Int]()

  //任务编号
  var num = 0

  //花色对应关系
  //1 2 3 4 ==> ♠ ♥ ♣ ♦
  val c = Map(1 -> s"\u2660", 2 -> s"\u2665", 3 -> s"\u2663", 4 -> s"\u2666")
  
  //初始花色盒子
  val colourBox = new HashMap[Int, Buffer[Int]]()
  
  //初始无花色手牌
  var ncPoker = ArrayBuffer[Int]()
  
   //初始带花色手牌
  var hcPoker = ArrayBuffer[Int]()

}