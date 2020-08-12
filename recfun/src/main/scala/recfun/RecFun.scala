package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
//    val mystr = "())("
//    println(balance(mystr.toList))

//      println(countChange(300,List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if(c == 0 || c == r ) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def score(charArr: List[Char], sc: Int) : Int = {
      if (charArr.isEmpty) sc
      else if (charArr.head == '(') {
        score(charArr.tail, sc+1)
      }else if (charArr.head == ')') {
        if (sc>0) score(charArr.tail, sc-1)
        else sc-1
      }else {
        score(charArr.tail, sc)
      }
    }
    val scores = score(chars, sc=0)
    println(scores)
    if (scores == 0) true
    else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def combinations(moneyLeft: Int, coinsLeft: List[Int]): Int ={
      if (moneyLeft==0) 1
      else if (moneyLeft<0) 0
      else if (coinsLeft.isEmpty && moneyLeft>0) 0
      else combinations(moneyLeft, coinsLeft.tail) + combinations(moneyLeft-coinsLeft.head, coinsLeft)
    }
    combinations(money, coins)
  }
}
