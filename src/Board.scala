import Array._
import scala.collection.mutable.ListBuffer
import io.AnsiColor._

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
            val new_piece:Piece = new Piece(true, position = (('a'+j).toChar,i+1), false)
            piece_list = piece_list :+ new_piece
          }
        }
        else if(i>=5){
          if((i+j)%2==0){
            board_pos_states(i)(j) = "Machine"
            var new_piece:Piece = new Piece(false, position = (('a'+j).toChar,i+1), false)
            piece_list = piece_list :+ new_piece
          }
        }
      }
    }

    println("Game Started: Do your first move!\n")
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
            if(inboard(i_cap, j_cap)){
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
        if(in_board_2){
          if(board_pos_states(new_i_2)(new_j_2) == "Empty" && !cap_happened){
            moves = (i.get_pos(), ij2an(new_i_2, new_j_2))::moves
          }

          //Capturing logic
          else if(is_adversary_team(i, (new_i_2, new_j_2))){
            val i_cap = new_i_2 + vec_2._1*hum_or_mac
            val j_cap = new_j_2 + vec_2._2*hum_or_mac
            if(inboard(i_cap, j_cap))
            {
                if(board_pos_states(i_cap)(j_cap) == "Empty") {
                  if (!cap_happened) {
                    cap_happened = true
                    moves = List(): List[((Char, Int), (Char, Int))]
                  }
                  moves = (i.get_pos(), ij2an(i_cap, j_cap)) :: moves
                }
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

            if(inboard(i_cap, j_cap))
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
            if(inboard(i_cap, j_cap))
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
            if(inboard(i_cap, j_cap))
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
            if(inboard(i_cap, j_cap))
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
  def execute_move(old_pos: (Char, Int), new_pos: (Char, Int)) : Boolean = {
      val mov_piece = piece_list.filter(_.get_pos()==old_pos)
      val old_i = an2ij(old_pos._1, old_pos._2)._1
      val old_j = an2ij(old_pos._1, old_pos._2)._2
      val new_i = an2ij(new_pos._1, new_pos._2)._1
      val new_j = an2ij(new_pos._1, new_pos._2)._2
      var cap_happened = false
      val possible_moves = compute_moves()

    if(possible_moves.count(_ == (old_pos, new_pos)) != 1){
        println("######## ERROR #########")
        println("Invalid move. Please, insert a valid move.")
        return false
    }

    if(mov_piece.length != 1) {
        println("######## ERROR #########")
        println("Invalid move. Please, insert a valid move.")
        return false
    }
      else {
        //Update the board and piece_list with the movement
        val old_piece = piece_list.filter(_.get_pos()==old_pos).head
        //val new_piece:Piece = new Piece(old_piece.get_is_human_team(), position = new_pos, false)
        var new_piece:Piece = old_piece
        if(old_piece.get_is_king())
             new_piece = new Piece(old_piece.get_is_human_team(), position = new_pos, true)
        else
            new_piece = new Piece(old_piece.get_is_human_team(), position = new_pos, false)
        val elem_pos = piece_list.indexOf(old_piece)
        piece_list(elem_pos) = new_piece

          board_pos_states(old_i)(old_j) = "Empty"
        if (mov_piece.head.get_is_human_team())    board_pos_states(new_i)(new_j) = "Human"
        else board_pos_states(new_i)(new_j) = "Machine"
      }

        //Check if the piece should become a king
      if(human_turn && new_pos._2 == 8)  {
        make_king(new_i, new_j)
      }
      else if(!human_turn && new_pos._2 == 1){
        make_king(new_i, new_j)
      }

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
    if (cap_happened && new_cap_possible(new_pos)) {
      printf("\n\n\ndouble capture\n\n\nn")
      val pos_moves = compute_moves()
      val from = pos_moves.head._1
      val to = pos_moves.head._2
      execute_move(from, to)
    }

    human_turn = !human_turn
    true
  }

  def ai_move() : Unit = {
    val pos_moves = compute_moves()
    val rand = scala.util.Random
    val choose = rand.nextInt(pos_moves.length)
    val old_pos = pos_moves(choose)._1
    val new_pos = pos_moves(choose)._2
    if(compute_moves() != Nil)
      execute_move(old_pos, new_pos)
    printf("AI move:\t(%c, %d)-(%c, %d)\n", old_pos._1, old_pos._2, new_pos._1, new_pos._2)
    println("Your turn")
  }

  def display_board():Unit = {

    for(i<- -1 to 7){
      for(j<- -1 to 7) {
        if (i == -1 && j == -1) print(" \t")
        else if (i != -1 && j != -1) {
          if (board_pos_states(i)(j) == "Empty") print(s"${BOLD}${WHITE}0\t")
          else if (board_pos_states(i)(j) == "Human") {
            val pos = ij2an(i, j)
            val hum_piece = piece_list.filter(_.get_pos() == pos).head
            if (!hum_piece.is_king) {
              print(s"${BOLD}${CYAN}H\t${RESET}")
            }
            else {
              print(s"${BOLD}${CYAN}h\t${RESET}")
            }
          }
          else {
              val pos = ij2an(i, j)
              val mac_piece = piece_list.filter(_.get_pos() == pos).head
            if (!mac_piece.is_king) {
              print(s"${BOLD}${RED}M\t${RESET}")
            }
            else{
              print(s"${BOLD}${RED}m\t${RESET}")
            }
          }
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

  def make_king(i: Int, j: Int): Unit = {
    val pos = ij2an(i, j)
    val del_piece = piece_list.filter(_.get_pos() == pos).head
    val king: Piece = new Piece(del_piece.is_human_team, pos, true)
    piece_list -= del_piece
    piece_list += king
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

  def inboard(i: Int, j: Int): Boolean = {
  if(i>=0 && i<=7 && j<=7 && j>=0) true
  else false
  }

  // DEBUGING FUNCTIONS
  def print_pieces(): Unit = {
  for (cur_piece <- piece_list){
    printf("(%c, %d), human: %b, king: %b\n", cur_piece.pos._1, cur_piece.pos._2, cur_piece.is_human_team, cur_piece.is_king)
  }
  }

  def rand_move() : Unit = {
    val pos_moves = compute_moves()
    val rand = scala.util.Random
    val choose = rand.nextInt(pos_moves.length)
    val old_pos = pos_moves(choose)._1
    val new_pos = pos_moves(choose)._2
    if(compute_moves() != Nil)
      execute_move(old_pos, new_pos)
    //printf("Your move:\t(%c, %d)-(%c, %d)\n", old_pos._1, old_pos._2, new_pos._1, new_pos._2)
    Board.display_board()
    // AI move
    Board.ai_move()
    Board.display_board()
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

  def print_game_state(): Unit = {
    println("Printing game state")
    for(i <- piece_list){
      println(i.get_is_human_team(), i.get_pos())
    }
  }

}
