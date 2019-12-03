object Tests extends App {
    //Math operations with char
    var b1 :List[Char] = List('a', 'c', 'e')
    b1.foreach(println)
    val a : Char = 'a'
    val a_int : Int = (a-'a').toInt
    val b_int : Int = a_int+1
    val b  = ('a'+1).toChar
    println(a, a_int, b, b_int)

    val tup1 = ('a',1)
    val vec1 = (1, 1)

    val sum_tup = ((tup1._1+vec1._1).toChar,tup1._2+vec1._2)
    println(sum_tup)



}
