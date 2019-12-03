object Main extends App {
  startingMessage()
  // IO interface

  def startingMessage(): Unit ={
    println("Welcome to the CHECKERS Game!\n")
    println("Instructions:")
    println("1- To move the piece, tape the original and the final position as the example above:")
    println("a,3-b,4")
    println("2- If you want to see all the possible moving, tape 'help'")
    println("3- If you are going to do multiple capture, just tape the first one.")
    println("4- If you want to reset the game, tape 'restart'")
    println("Now, lets start: Tape your command (restart, help or a move)")
  }

  Board.start_game()
  //Board.print_game_state()
  Board.display_board()
  Board.print_mov_list()
  print(Board.board_pos_states(7)(5), Board.board_pos_states(6)(6))
}
