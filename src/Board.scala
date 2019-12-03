import Array._

object Board {
  var board_pos_states  = ofDim[String](8,8) //'Empty', 'Human' or 'Machine'
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

  /*
    //Substitute this code section for a for loop : toInt and toChar will help
    var pos = ('a',1)
    var new_piece:Piece = new Piece(true, position = ('a',1))
    piece_list = piece_list :+ new_piece

    //board_pos_states()//Add state Human or Machine

    new_piece = new Piece(true, position = ('c',1))
    piece_list = piece_list :+ new_piece
    new_piece= new Piece(true, position = ('e',1))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('g',1))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('b',2))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('d',2))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('f',2))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('h',2))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('a',3))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('c',3))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('e',3))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(true, position = ('g',3))
    piece_list = piece_list :+ new_piece

    new_piece = new Piece(false, position = ('b',6))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('d',6))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('f',6))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('h',6))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('a',7))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('c',7))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('e',7))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('g',7))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('b',8))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('d',8))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('f',8))
    piece_list = piece_list :+ new_piece
    new_piece = new Piece(false, position = ('h',8))
    piece_list = piece_list :+ new_piece
    */


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
      if(!i.get_is_king()){
        var hum_or_mac = 1
        if(!i.get_is_human_team()) hum_or_mac = -1

        val new_i_1 = (i.get_pos()._1-'a').toInt + vec_1._1*hum_or_mac
        val new_j_1 = i.get_pos()._2 + vec_1._2*hum_or_mac
        val in_board_1 = (0<=new_i_1 && new_i_1 <=7 && 0<=new_j_1 && new_j_1 <= 7)
        val new_i_2 = (i.get_pos()._1-'a').toInt + vec_2._1*hum_or_mac
        val new_j_2 = i.get_pos()._2 + vec_2._2*hum_or_mac
        val in_board_2 = (0<=new_i_2 && new_i_2 <=7 && 0<=new_j_2 && new_j_2 <= 7)

        if(in_board_1){
          if(board_pos_states(new_i_1)(new_j_1) == "Empty" && ((human_turn && i.get_is_human_team()) || (!human_turn && !i.get_is_human_team()))){
            moves = (i.get_pos(), ((new_i_1+'a').toChar,new_j_1))::moves
          }
          //Capturing logic for normal piece
          else if(board_pos_states(new_i_1)(new_j_1) == "Machine"){

          }
        }
        if(in_board_2){
          if(board_pos_states(new_i_2)(new_j_2) == "Empty"){
            moves = (i.get_pos(), ((new_i_2+'a').toChar,new_j_2))::moves
          }
          //Capturing logic
          else if(board_pos_states(new_i_2)(new_j_2) == "Machine"){

          }
        }
        //For the Human Queen
      }

      //Queen's case
      else{
      }

    }

    moves
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


  /*
  1)
  2)
  3)
  4)
  5)
  6)
  */


}
