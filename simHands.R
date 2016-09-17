simHands <- function(totalSims) {
  
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

evalRule <- function(totalSims, rules, totalPlayers, sims, isInSimpleFun) {
  
  results <- rep(0, totalPlayers)
  x <- 1
  while(x <= totalSims) {
    results <- results + calcOutcomes(sims[[x]], rules, totalPlayers, isInSimpleFun) 
    x <- x + 1
  }

  return (results)
}



