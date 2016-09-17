evalHand <- function(h) {
  if (h$value[1] == h$value[2] && h$value[2] == h$value[3])
  {
    group <- 3  # Trips
    cards <- c(h$value[1], h$value[2], h$value[3])
  }
  else if (h$value[1] == h$value[2])
  {
    group <- 2  # A pair
    cards <- c(h$value[1], h$value[2], h$value[3])
  }
  else if (h$value[1] == h$value[3])
  {
    group <- 2  # A pair
    cards <- c(h$value[1], h$value[3], h$value[2])
  }
  else if (h$value[2] == h$value[3])
  {
    group <- 2  # A pair
    cards <- c(h$value[2], h$value[3], h$value[1])
  }  
  else
  {
    group <- 1  # High Card
    cards <- sort(h$value, decreasing = TRUE)
  }
  
  handValue <- 13 * 13 * 13 * (group - 1) +
    13 * 13 * (cards[[1]] - 1) + 
    13 * (cards[[2]] - 1) + 
    (cards[[3]] - 1)
  
  # return simple vector
  return (as.integer(c(group, cards, handValue)))
}

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
  return (m4)
}