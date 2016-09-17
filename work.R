
cards <- dealCards(deck, totalHands)
handCards <- split(cards, rep(1:totalHands, each = 3))
handRanks <- lapply(handCards, evalHand)
rules <- c(100, rep(100, totalPlayers - 1))
abc <- calcOutcomes(handRanks, rules, totalPlayers, isInSimpleFun)


x <- 1
y <- 0
while(x <= totalSims) {
  abc <- calcOutcomes(sims[[x]], rules, totalPlayers,isInSimpleFun) 
  y <- y + abc[1]
  x <- x + 1
}

lapply(sims, calcOutcomes, rules, totalPlayers,isInSimpleFun)

abc <- rep(0, 3)

# Simple rule with graph:
# Run Sims
# For each player Rule
#   For all other players Rule
#      eval
# Graph