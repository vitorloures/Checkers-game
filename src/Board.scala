import Array._

object Board {
  var board_pos_states: Array[Array[String]] = ofDim[String](8,8) //'Empty', 'Human' or 'Machine'
  var human_turn : Boolean = true
  var piece_list: List[Piece] = List()
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

    //Compute moves
    //possible_moves = compute_moves()

    //print_game_state()
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

    for(i <- piece_list){
      //Moving logic for normal pieces
      val is_i_turn = (i.is_human_team && human_turn) || (!i.is_human_team && !human_turn)
      if(!i.get_is_king() && is_i_turn){
        var hum_or_mac = 1
        if(!i.get_is_human_team()) hum_or_mac = -1

        val old_i = an2ij(i.get_pos()._1, i.get_pos()._2)._1
        val old_j = an2ij(i.get_pos()._1, i.get_pos()._2)._2
        val new_i_1 = old_i + vec_1._1*hum_or_mac
        val new_j_1 = old_j + vec_1._2*hum_or_mac
        val in_board_1 = (0<=new_i_1 && new_i_1 <=7 && 0<=new_j_1 && new_j_1 <= 7)
        val new_i_2 = old_i + vec_2._1*hum_or_mac
        val new_j_2 = old_j + vec_2._2*hum_or_mac
        val in_board_2 = (0<=new_i_2 && new_i_2 <=7 && 0<=new_j_2 && new_j_2 <= 7)

        if(in_board_1){
          if(board_pos_states(new_i_1)(new_j_1) == "Empty") {
            moves = (i.get_pos(), ij2an(new_i_1, new_j_1))::moves
          }
          //Capturing logic for normal piece
          else if(board_pos_states(new_i_1)(new_j_1) == "Machine"){

          }
        }
        if(in_board_2){
          if(board_pos_states(new_i_2)(new_j_2) == "Empty"){
            moves = (i.get_pos(), ij2an(new_i_2, new_j_2))::moves
          }
          //Capturing logic
          else if(board_pos_states(new_i_2)(new_j_2) == "Machine"){

          }
        }
      }

      //Queen's case
      else{
      }

    }

    moves
  }

  //Get a valid move and execute it
  def execute_move(old_pos: (Char, Int), new_pos: (Char, Int)) : Unit = {
      val mov_piece = piece_list.filter(_.get_pos()==old_pos)
      if(mov_piece.length != 1)
        println("######## ERROR #########")
      else {
        val old_i = an2ij(old_pos._1, old_pos._2)._1
        val old_j = an2ij(old_pos._1, old_pos._2)._2
        board_pos_states(old_i)(old_j) = "Empty"
        val new_i = an2ij(new_pos._1, new_pos._2)._1
        val new_j = an2ij(new_pos._1, new_pos._2)._2
        if (mov_piece.head.get_is_human_team())    board_pos_states(new_i)(new_j) = "Human"
        else board_pos_states(new_i)(new_j) = "Machine"
      }

    //If is a capture, delete dead piece
  }

  def AI_move() : Unit = {
    
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
    for(i<- 0 to 7){
      for(j<- 0 to 7){
            if(board_pos_states(i)(j) == "Empty") if(i==0 && j==1) print("H\t") else print("0\t")
            else if(board_pos_states(i)(j) == "Human") print("@\t")
            else print("ç\t")

      }
      print('\n')
    }
    print('\n')
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
