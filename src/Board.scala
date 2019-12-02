import Array._

object Board {
  var board_pos_states  = ofDim[String](8,8) //Empty, Human or Machine
  var human_turn : Boolean = true
  var piece_list: List[Piece] = List()
  var possible_moves: List[(String, Int)] = List()

  def start_game(): Unit = {
    human_turn = true
    //Initialize piece positions

    //Substitute this code section for a for loop : toInt and toChar will help
    for (i <- 0 to 7) {
      for ( j <- 0 to 7) {
        board_pos_states(i)(j) = "Empty"
      }
    }
    var new_piece:Piece = new Piece(true, position = ('a',1))
    piece_list = piece_list :+ new_piece

    board_pos_states()//Add state Human or Machine

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

    //Compute moves
    possible_moves = compute_moves()

    println("Game Started: Do your first move!\n")
  }

  def print_game_state()  = {
    println("Printing game state")
    for(i <- piece_list){
        println(i.get_is_human_team(), i.get_pos())
    }

    def compute_moves(): List[((Char, Int), (Char, Int))] = {

    }


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
