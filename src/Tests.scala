object Tests extends App {
    var b1 :List[Char] = List('a', 'c', 'e')
    b1.foreach(println)
    val a : Char = 'a'
    val a_int : Int = a.toInt
    val b_int : Int = a_int+1
    val b  = b_int.toChar
    println(a, a_int, b, b_int)


}
