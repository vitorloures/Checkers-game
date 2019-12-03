import Array._
import scala.collection.mutable.ListBuffer

object Board {
  var board_pos_states: Array[Array[String]] = ofDim[String](8,8) //'Empty', 'Human' or 'Machine'
  var human_turn : Boolean = true
  var piece_list: ListBuffer[Piece] = ListBuffer()
  var possible_moves: List[((String, Int),(String, Int))] = List()

  def start_game(): Unit = {
    human_turn = true
    //Initialize piece positions

    for (i <- 0 to 7) {
      for ( j <- 0 to 7) {
        board_pos_states(i)(j) = "Empty"
        if (i<=2){
          if((i+j)%2==0){
            board_pos_states(i)(j) = "Human"
            var new_piece:Piece = new Piece(true, position = (('a'+j).toChar,i+1))
            piece_list = piece_list :+ new_piece
          }
        }
        else if(i>=5){
          if((i+j)%2==0){
            board_pos_states(i)(j) = "Machine"
            var new_piece:Piece = new Piece(false, position = (('a'+j).toChar,i+1))
            piece_list = piece_list :+ new_piece
          }
        }
      }
    }

    println("Game Started: Do your first move!\n")
  }

  def print_game_state(): Unit = {
    println("Printing game state")
    for(i <- piece_list){
        println(i.get_is_human_team(), i.get_pos())
    }
  }

  //Compute all the possible moves to be done in such turn
  def compute_moves(): List[((Char, Int), (Char, Int))] = {
    val vec_1 = (1,1)
    val vec_2 = (1,-1)
    var moves = List():List[((Char, Int), (Char, Int))]
    var cap_happened = false

    for(i <- piece_list){
      //Moving logic for normal pieces
      val is_i_turn = (i.is_human_team && human_turn) || (!i.is_human_team && !human_turn)
      val old_i = an2ij(i.get_pos()._1, i.get_pos()._2)._1
      val old_j = an2ij(i.get_pos()._1, i.get_pos()._2)._2

      if(!i.get_is_king() && is_i_turn){
        var hum_or_mac = 1
        if(!i.get_is_human_team()) hum_or_mac = -1

        val new_i_1 = old_i + vec_1._1*hum_or_mac
        val new_j_1 = old_j + vec_1._2*hum_or_mac
        val in_board_1 = (0<=new_i_1 && new_i_1 <=7 && 0<=new_j_1 && new_j_1 <= 7)
        val new_i_2 = old_i + vec_2._1*hum_or_mac
        val new_j_2 = old_j + vec_2._2*hum_or_mac
        val in_board_2 = (0<=new_i_2 && new_i_2 <=7 && 0<=new_j_2 && new_j_2 <= 7)

        if(in_board_1){
          if(board_pos_states(new_i_1)(new_j_1) == "Empty" && !cap_happened) {
            moves = (i.get_pos(), ij2an(new_i_1, new_j_1))::moves
          }
          //Capturing logic for normal piece
          else if(is_adversary_team(i, (new_i_1, new_j_1))){
            val i_cap = new_i_1 + vec_1._1*hum_or_mac
            val j_cap = new_j_1 + vec_1._2*hum_or_mac
            //If it is possible to capture
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }
        }
        if(in_board_2){
          if(board_pos_states(new_i_2)(new_j_2) == "Empty" && !cap_happened){
            moves = (i.get_pos(), ij2an(new_i_2, new_j_2))::moves
          }

          //Capturing logic
          else if(is_adversary_team(i, (new_i_2, new_j_2))){
            val i_cap = new_i_2 + vec_2._1*hum_or_mac
            val j_cap = new_j_2 + vec_2._2*hum_or_mac
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }
        }
      }

      //King's case
      else if(i.get_is_king() && is_i_turn) {
        val new_i_1 = old_i + vec_1._1
        val new_j_1 = old_j + vec_1._2
        val in_board_1 = (0 <= new_i_1 && new_i_1 <= 7 && 0 <= new_j_1 && new_j_1 <= 7)

        val new_i_2 = old_i + vec_2._1
        val new_j_2 = old_j + vec_2._2
        val in_board_2 = (0 <= new_i_2 && new_i_2 <= 7 && 0 <= new_j_2 && new_j_2 <= 7)

        val new_i_3 = old_i - vec_1._1
        val new_j_3 = old_j - vec_1._2
        val in_board_3 = (0 <= new_i_3 && new_i_3 <= 7 && 0 <= new_j_3 && new_j_3 <= 7)

        val new_i_4 = old_i - vec_2._1
        val new_j_4 = old_j - vec_2._2
        val in_board_4 = (0 <= new_i_4 && new_i_4 <= 7 && 0 <= new_j_4 && new_j_4 <= 7)

        if (in_board_1) {
          if (board_pos_states(new_i_1)(new_j_1) == "Empty" && !cap_happened) {
            moves = (i.get_pos(), ij2an(new_i_1, new_j_1)) :: moves
          }
          //Capturing logic
          else if(is_adversary_team(i, (new_i_1, new_j_1))){
            val i_cap = new_i_1 + vec_1._1
            val j_cap = new_j_1 + vec_1._2
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }
        }

        if (in_board_2) {
          if (board_pos_states(new_i_2)(new_j_2) == "Empty" && !cap_happened) {
            moves = (i.get_pos(), ij2an(new_i_2, new_j_2)) :: moves
          }
          else if(is_adversary_team(i, (new_i_2, new_j_2))){
            val i_cap = new_i_2 + vec_2._1
            val j_cap = new_j_2 + vec_2._2
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }

        }

        if (in_board_3) {
          if (board_pos_states(new_i_3)(new_j_3) == "Empty" && !cap_happened) {
            moves = (i.get_pos(), ij2an(new_i_3, new_j_3)) :: moves
          }
          else if(is_adversary_team(i, (new_i_3, new_j_3))){
            val i_cap = new_i_3 - vec_1._1
            val j_cap = new_j_3 - vec_1._2
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }
        }

        if (in_board_4) {
          if (board_pos_states(new_i_4)(new_j_4) == "Empty" && !cap_happened) {
            moves = (i.get_pos(), ij2an(new_i_4, new_j_4)) :: moves
          }
          else if(is_adversary_team(i, (new_i_4, new_j_4))){
            val i_cap = new_i_4 - vec_2._1
            val j_cap = new_j_4 - vec_2._2
            if(board_pos_states(i_cap)(j_cap) == "Empty"){
              if(!cap_happened){
                cap_happened = true
                moves = List():List[((Char, Int), (Char, Int))]
              }
              moves = (i.get_pos(), ij2an(i_cap, j_cap))::moves
            }
          }
        }

      }
    }

    moves
  }

  def new_cap_possible(cur_pos: (Char, Int)): Boolean = {
    val cur_piece = piece_list.filter(_.get_pos()==cur_pos).head

    if(cur_piece.get_is_human_team() && !cur_piece.is_king){
      false
    }
    else if(!cur_piece.get_is_human_team() && !cur_piece.is_king){
      false
    }
      //King's case
    else{
      false
    }

  }

  //Get a valid move and execute it
  def execute_move(old_pos: (Char, Int), new_pos: (Char, Int)) : Unit = {
      val mov_piece = piece_list.filter(_.get_pos()==old_pos)
      val old_i = an2ij(old_pos._1, old_pos._2)._1
      val old_j = an2ij(old_pos._1, old_pos._2)._2
      val new_i = an2ij(new_pos._1, new_pos._2)._1
      val new_j = an2ij(new_pos._1, new_pos._2)._2
      var cap_happened = false

      if(mov_piece.length != 1)
        println("######## ERROR #########")
      else {
        board_pos_states(old_i)(old_j) = "Empty"
        if (mov_piece.head.get_is_human_team())    board_pos_states(new_i)(new_j) = "Human"
        else board_pos_states(new_i)(new_j) = "Machine"
      }

    //Check if the piece should become a king
      if(human_turn && new_pos._2 == 8)   mov_piece.head.set_king()
      else if(!human_turn && new_pos._2 == 1) mov_piece.head.set_king()


      if(human_turn) printf("Your move:\t(%c, %d)-(%c, %d)\n", old_pos._1, old_pos._2, new_pos._1, new_pos._2)

    //If it happens a capture, delete the captured piece
      if(math.abs(new_pos._2-old_pos._2) == 2){
        cap_happened = true
        val vec_dir = ((new_i-old_i)/2,(new_j-old_j)/2)
        val del_i = old_i+vec_dir._1
        val del_j = old_j+vec_dir._2
        board_pos_states(del_i)(del_j) = "Empty"
        val del_pos = ij2an(del_i, del_j)
        val del_piece = piece_list.filter(_.get_pos()==del_pos).head
        piece_list -= del_piece
      }

    //Check for double
    if (cap_happened && new_cap_possible(new_pos)!=Nil) {

    }
    human_turn = !human_turn
  }

  def ai_move() : Unit = {
    val pos_moves = compute_moves()
    val rand = scala.util.Random
    val choose = rand.nextInt(pos_moves.length-1)
    val old_pos = pos_moves(choose)._1
    val new_pos = pos_moves(choose)._2
    execute_move(old_pos, new_pos)
    printf("AI move:\t(%c, %d)-(%c, %d)\n", old_pos._1, old_pos._2, new_pos._1, new_pos._2)
    println("Your turn")
  }

  def print_mov_list():Unit = {
    val moves = compute_moves()
    println("Possible moves: ", moves.length)
    for(i <- moves){
      print(i)
      print(" // ")
    }
    print('\n')
  }

  def display_board():Unit = {
    for(i<- -1 to 7){
        for(j<- -1 to 7){
          if(i == -1 && j == -1) print(" \t")
          else if(i!= -1 && j != -1) {
            if (board_pos_states(i)(j) == "Empty") print("0\t")
            else if (board_pos_states(i)(j) == "Human") print("H\t")
            else print("M\t")
          }
          else if (i == -1) printf("%c\t", 'A'+j)
          else if (j == -1) printf("%d\t", i+1)

        }
      print('\n')
    }
    print('\n')
  }

  def is_adversary_team(piece: Piece, pos: (Int, Int)):Boolean = {
    val i = pos._1
    val j = pos._2

    if((piece.get_is_human_team() && board_pos_states(i)(j) == "Machine") || (!piece.get_is_human_team() && board_pos_states(i)(j) == "Human"))
      return true
    else
      return false
  }

  //Convert from position in the board (a,1) to the matrix (0,0)
  def an2ij(char: Char, num: Int) : (Int, Int) = {
    val i = num-1
    val j = (char-'a').toInt

    (i,j)
  }

  //Convert from position in the matrix (0,0) to the board (a,1)
  def ij2an(i: Int, j: Int): (Char, Int) = {
    val a = ('a'+j).toChar
    val n = i+1

    (a, n)
  }
}
