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
    println("Now, lets start: Type your command (restart, help or a move)")
    Board.start_game()
    Board.display_board()

    do{
      display_instruction()

      val line = scala.io.StdIn.readLine.trim



      //val input = scala.io.StdIn.readInt()

      line match {
        case "1" =>
          println("Give the original position of the piece. For example \"a-3\" ")
          var read_tuple = scala.io.StdIn.readLine()
          var tuple = read_tuple.split("-")
          val org_tuple = (tuple(0).charAt(0), tuple(1).toInt)

          println("Give the destination position of the piece. For example \"g-7\"")
          read_tuple = scala.io.StdIn.readLine()
          tuple = read_tuple.split("-")
          val dst_tuple = (tuple(0).charAt(0), tuple(1).toInt)

          if(Board.execute_move(org_tuple,dst_tuple)) {
            Board.display_board()
            // AI move
            Board.ai_move()
            Board.display_board()
          }
        case "2" =>
          Board.print_mov_list()

        case "3" =>
          Board.start_game()
          println("New game !")
          Board.display_board()

        case "4" => System.exit(0)

        case "5" => Board.print_pieces()

        case "6" => Board.rand_move()

        case default =>
          println("Wrong input")
      }

      //do while compute_moves != NULL
    } while(Board.compute_moves() != null)
    if(Board.human_turn)
      printf("Game finished\nYOU LOST!\n\n")
    else
      printf("Game finished\nYOU WIN!\n\n")

    System.exit(0)
  } // for startingMessage function

}
