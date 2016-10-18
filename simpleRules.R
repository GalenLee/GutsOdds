
isInSimpleFun <- function(ruleValues, idx, handValue, isPlayerIn) {
  if (idx == 1)
     return (ruleValues[1] <= handValue)
  else
     return (ruleValues[2] <= handValue)
}
