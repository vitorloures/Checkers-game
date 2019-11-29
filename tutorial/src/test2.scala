object test2 extends App {
  for (i <- 1 to 10)
     println(i)

   val xs = for (i <- 1 to 10; if i % 2 == 0) yield i

   for (x <- xs)
   println(x)

   val ys = for {
     i <- 1 to 5
     j <- 1 to i
     } yield (i, j)

   for ((a, b) <- ys)
   println(s"($a, $b)")
}