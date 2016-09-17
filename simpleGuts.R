beatDeck <- FALSE
totalPlayers <- 2
totalHands <- totalPlayers + ifelse(beatDeck, 1, 0)
totalSims <- 10

sims <- simHands(totalSims)
rules <- c(200, rep(200, totalPlayers - 1))
abc <- evalRule(totalSims, rules, totalPlayers, sims, isInSimpleFun)
