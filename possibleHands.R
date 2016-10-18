
getPossibleHands <- function(deck) {
  
  # Select all combinations of hands (indexes of cards in deck).
  # Then get hand and evaluate it. Returns matric (rows are group, card 1-3, hand value).
  m1 <- combn(1:nrow(deck), 3, function(indexes){ return(evalHand(deck[indexes,])) })
  
  # Transpose matrix and create dataframe.
  m2 <- as.data.frame(t(m1))
  
  # Group by hand values and count occurences.
  m3 <- aggregate(rep.int(1, nrow(m2)), list(m2$V5, m2$V1, m2$V2, m2$V3, m2$V4), sum)
  
  # Name columns
  colnames(m3) <- c("HandValue", "Group", "Card1", "Card2", "Card3", "Count")
  
  # Order by hand value and return.
  m4 <- m3[order(m3$HandValue),]
  
  m4 <- within(m4, accSum <- cumsum(Count)/sum(Count))
  rownames(m4) <- seq(length = nrow(m4))
  
  return (m4)
}

possibleHands <- getPossibleHands(deck)