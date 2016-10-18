simpleGuts <- function(rulesToTry, totalSims) {
  beatDeck <- FALSE
  totalPlayers <- 2
  totalHands <- totalPlayers + ifelse(beatDeck, 1, 0)
  sims <- simHands(totalSims, totalHands)
  df <- expand.grid(rulesToTry, rulesToTry)
  df$EV <- apply(df, 1, function(x) {
    rules <- c(x[1], x[2])
    evalRule(totalSims, rules, possibleHands, totalPlayers, sims, isInSimpleFun)[1] / totalSims
  })

  library(reshape2)
  meltData <- melt(df, id = c("Var1","Var2"))
  matDf <- dcast(meltData, Var1 ~ Var2, sum)
  rownames(matDf) <- matDf[,1]
  return(as.matrix( matDf[,-1]))
}