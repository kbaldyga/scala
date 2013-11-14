package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 && r == 0) 1
    else if (r == 0 && c > r) 0
    else if (c == 0) 1
    else pascal(c-1, r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def traverse(xs: List[Char], acc: Int): Boolean = {
      if (xs.isEmpty && acc==0) true
      else if(acc < 0 || (xs.isEmpty && acc != 0)) false
      else traverse(xs.tail, (if (xs.head == ')') (acc-1) else if (xs.head == '(') (acc+1) else acc))
    }
    traverse(chars,0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
