object Main extends App {
  startingMessage()
  // IO interface

  def display_instruction(): Unit ={
    println("Enter the input")
    println("1 : To move the piece")
    println("2 : To see all the possible moves (help)")
    println("3 : To reset the game")
    println("4 : Exit")
  }

  def startingMessage(): Unit ={
    println("***************Welcome to the CHECKERS Game! ***************\n")
    println("Instructions:")
    println("If you are going to do multiple capture (moves), just do one by one move\n")
    Board.start_game()
    Board.display_board()

    do{
      println("Now, lets start: Type your command (restart, help or a move)")
      display_instruction()

      val input = scala.io.StdIn.readInt()

      input match {
        case 1 =>
          println("Give the original position of the piece. for the example \"a-3\" ")
          var read_tuple = scala.io.StdIn.readLine()
          var tuple = read_tuple.split("-")
          val org_tuple = (tuple(0).charAt(0), tuple(1).toInt)

          println("Give the destination position of the piece. for the example \"g-7\"")
          read_tuple = scala.io.StdIn.readLine()
          tuple = read_tuple.split("-")
          val dst_tuple = (tuple(0).charAt(0), tuple(1).toInt)

          if(Board.execute_move(org_tuple,dst_tuple)) {
            Board.display_board()
            Board.print_pieces()
            // AI move
            Board.ai_move()
            Board.print_pieces()
            Board.display_board()
          }
        case 2 =>
          Board.print_mov_list()

        case 3 =>
          Board.start_game()
          println("New game !")
          Board.display_board()

        case 4 => System.exit(1)

        case default =>
          println("Wrong input")
      }

      //do while compute_moves != NULL
    }while(Board.compute_moves() != null)


  } // for startingMessage function

}

/*
def startingMessage(): Unit ={
    println("Welcome to the CHECKERS Game!\n")
    println("Instructions:")
    println("1- To move the piece, type the original and the final position as the example above:")
    println("a,3-b,4")
    println("2- If you want to see all the possible moving, type 'help'")
    println("3- If you are going to do multiple capture, just type the first one.")
    println("4- If you want to reset the game, type 'restart'")
    println("Now, lets start: Type your command (restart, help or a move)")
  }

  Board.start_game()
  //Board.print_game_state()
  Board.display_board()
  Board.print_mov_list()
  Board.execute_move(('g',3),('f',4))
  Board.ai_move()
  Board.display_board()

  //do while compute_moves != NULL
}



 */