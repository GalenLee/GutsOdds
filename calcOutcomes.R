calcOutcomes <- function(handRanks, ruleValues, totalPlayers, isInFun) {

  maxHand <- 0
  totalPlayerIn <- 0
  totalWinners <- 0
  isPlayerIn <- rep(FALSE, totalPlayers)
  playerOutcome <- rep(0, totalPlayers)
  
  for (idx in 1:totalPlayers) {
    
    # Determine if player is In given hand value
    handValue <- handRanks[[idx]][5]
    isIn <- isInFun(ruleValues, idx, handValue, isPlayerIn)  
    isPlayerIn[idx] <- isIn
    
    # Calc max hand value, total In players and total winners
    if (isIn) {
      totalPlayerIn <- totalPlayerIn + 1
      if (maxHand < handValue){
        maxHand <- handValue
        totalWinners <- 1
      }
      else if (maxHand == handValue) {
        totalWinners <- totalWinners + 1
      }
    }
  }
  
  # TODO: If beat deck and one player:
  # if deck wins or ties...  MaxHand updated+1 and totalWinners = 0
  # if deck lose...  ok
  
  # Calc player outcomes
  totalLosers <- totalPlayerIn - totalWinners
  for (idx in 1:totalPlayers) {
    
    if (isPlayerIn[idx]){
      handValue <- handRanks[[idx]][5]
      if (maxHand == handValue) {
        # Player won or tied.  Ties split.
        outcome <- totalPlayers / totalWinners  # 1 / totalWinners
      }
      else {
        # Player lost
        outcome <- -totalPlayers  # -1
      }
    }
    else {
      # Player not In.
      outcome <- 0
    }
    
    if (totalPlayerIn == 0)
      playerOutcome[idx] = 0
    else
      # First part takes into account ante
      # Second part takes into account outcome
      # Third part takes into account future expected value (i.e. share pot).
      playerOutcome[idx] <- -1 + outcome + totalLosers      #-1 / totalPlayers + outcome + totalLosers / totalPlayers
  }
  
  return (playerOutcome)
}