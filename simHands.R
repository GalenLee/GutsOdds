simHands <- function(totalSims, totalHands) {
  
  # Initialize results
  sims <- vector("list", totalSims)
  
  x <- 1
  while(x <= totalSims) {
    cards <- dealCards(deck, totalHands)
    handCards <- split(cards, rep(1:totalHands, each = 3))
    handRanks <- lapply(handCards, evalHand)
    sims[[x]] <- handRanks 
    x <- x + 1
  }  
  
  return (sims)
}

evalRule <- function(totalSims, rules, possibleHands, totalPlayers, sims, isInFun) {
  
  ruleValues <- possibleHands[rules, "HandValue"]
  
  results <- rep(0, totalPlayers)
  x <- 1
  while(x <= totalSims) {
    results <- results + calcOutcomes(sims[[x]], ruleValues, totalPlayers, isInFun) 
    x <- x + 1
  }

  return (results)
}