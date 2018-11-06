package com.rea.referentialtransparency

object RefTransExercises {

  // How would you push out the side-effects?  How would you test the logic
  // without performing the side-effect?
  def printWinner(p1: Int, p2: Int) =
    if (p1 > p2)
      println("player one is the winner!")
    else
      println("player two is the winner!")

  // No side effects?
  def returnWinner(p1: Int, p2: Int): GameResult =
    if (p1 == p2)
      Draw
    else if (p1 > p2)
      PlayerOneWins
    else
      PlayerTwoWins

  def printWinnerRefTrans(p1: Int, p2: Int): String = {
    returnWinner(p1, p2) match {
      case Draw => "no one wins"
      case PlayerOneWins => "player one is the winner"
      case PlayerTwoWins => "player two is the winner"
    }
  }

  sealed trait GameResult

  case object PlayerOneWins extends GameResult
  case object PlayerTwoWins extends GameResult
  case object Draw extends GameResult
}
