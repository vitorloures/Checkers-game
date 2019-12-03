class Piece (human_team : Boolean, position : (Char, Int)) {
  var is_human_team = human_team
  var is_king : Boolean  = false
  var pos = position

  def get_is_human_team(): Boolean ={
    is_human_team
  }

  def get_is_king(): Boolean = {
    is_king
  }

  def get_pos():(Char, Int) = {
    pos
  }

}
