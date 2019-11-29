object factorial extends App {
  def fact(n: Int): Int = {
      if (n == 0) 1
      else  n*fact(n-1)
  }
  println(fact(5))
}
