import scala.collection.mutable.ListBuffer

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

    val abs2 = math.abs(-2)
    val sum_tup = ((tup1._1+vec1._1).toChar,tup1._2+vec1._2)
    println(sum_tup, abs2)

    var list: ListBuffer[Int] = ListBuffer(4, 6, 2, 7, 0,8, 9)
    println(list)
    val old_elem = list.indexOf(7)
    println(old_elem)
    list(old_elem) = 77
    println(list)


}
