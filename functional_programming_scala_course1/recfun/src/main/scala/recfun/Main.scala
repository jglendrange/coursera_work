package recfun

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

    	def factorial(n: Int): Int = {if(n ==0 ) 1 else n * factorial(n-1)}

      if (c > r) 0

    	else (factorial(r)) / (factorial(c) * factorial(r-c))
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def routine(chars: List[Char], counter: Int): Boolean ={
        if ((chars.isEmpty && counter != 0) || counter < 0) false
        else if (chars.isEmpty && counter == 0) true
        else if (chars.head == '(') routine(chars.tail, counter + 1)
        else if (chars.head == ')') routine(chars.tail, counter - 1)
        else routine(chars.tail, counter)
     }
     routine(chars, 0)

    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val m = coins.length
      def count(money: Int, coins: List[Int], m: Int): Int ={
        if (money == 0) 1

        else if (money < 0 || m == 0) 0

        else count(money, coins, m-1) + count(money - coins(m-1), coins, m)
      }
      count(money, coins, m)
    }
  }
