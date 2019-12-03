object Main extends App {
  startingMessage()
  // IO interface

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
