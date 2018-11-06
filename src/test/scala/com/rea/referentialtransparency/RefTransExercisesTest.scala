package com.rea.referentialtransparency

//  "Print winner referentially transparently :ghost:" in {
import RefTransExercises._
import org.specs2.mutable.Specification

class RefTransExercisesTest extends Specification {

  "Player one wins" in {
    returnWinner(2,1) === PlayerOneWins
  }

  "Player two wins" in {
    returnWinner(1,2) === PlayerTwoWins
  }

  "Draw" in {
    returnWinner(2,2) === Draw
  }

  "Print winner without side effects" in {
    printWinnerRefTrans(2, 3) === "player two is the winner"
  }

}
