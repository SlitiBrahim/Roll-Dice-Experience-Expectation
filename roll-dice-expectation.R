# Get sum of multiple roll scores
getRollScoresSum <- function(rollDiceRepeat=2) {
  
  # result of addition of 2 roll dice
  result <- 0
  
  # loop through roll dice repeat number
  for(roll in 1:rollDiceRepeat) {
    # generate a pseudo-random number from first die value to last die value and keep it as its floor
    diceScore <- floor( runif(1, min=dice[1], max=dice[length(dice)]+1) )
    # add diceScore to roll dices' score
    result <- result + diceScore
  }
  
  # return the result of the roll of a dice
  return(result)
}

# Repeats a random experience which consists of adding multiple roll dice values
experience <- function(dice, repeatNumber=50) {
  
  # vector of random sums
  result <- c()
  
  # Repeat the experince n times
  for(roll in 1:repeatNumber) {
    # get 2 roll dices score
    rollsSum <- getRollScoresSum()
    
    # append rollSum in rsult
    result <- c(result, rollsSum)
  } 
  
  # return produced random sums
  return(result)
}


executeExperience <- function() {
  # A dice of values from 1 to 6
  dice <- 1:6
  
  experienceResult <- experience(dice)
  # mean expected value of an experience
  expectation <- floor( mean(experienceResult) )
  cat("This experience expectation value is", expectation)
  
  barplot(table(experienceResult), main="Expectation Value of dice scores' sum", ylab="Number of occurences", xlab="Dice scores' sum")
}

executeExperience()

