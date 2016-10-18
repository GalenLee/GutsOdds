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


