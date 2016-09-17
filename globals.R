# Build single deck
suits <- c("D","H","S","C")
cards <- c(2:10,"J","Q","K","A")
values <- c(1:13)
deck <- expand.grid(card=cards, suit=suits)
deck$value <- values

# Build full deck given number of decks
# totalNumOfDecks <- 2
# deck <- deck[rep(seq(nrow(deck)), totalNumOfDecks),]

dealCards <- function(deck, totalHands) {
  sample <- sample(1:nrow(deck), 3 * totalHands);
  return(deck[sample,])
}
