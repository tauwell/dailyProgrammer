#July 16 Reddit Daily Programming Challenge: https://www.reddit.com/r/dailyprogrammer/comments/3ddpms/20150715_challenge_223_intermediate_eel_of_fortune/
#Eel of Fortune
###SOLVED: 2 days

# You work on the popular game show Eel of Fortune, where contestants take turns fishing live eels out of an aquarium for the opportunity to solve a word puzzle. The word puzzle works like the game Hangman. A secret word is obscured on the board. A player guesses a letter of the alphabet, and if that letter appears anywhere in the secret word, all of the times it appears in the secret word are revealed.
# 
# An unfortunate incident occurred on yesterday's show. The secret word was SYNCHRONIZED, and at one point the board was showing:
# 
# S _ N _ _ _ O N _ _ _ D
# 
# As you can see, the letters on the board spelled "snond", which is of course an extremely offensive word for telemarketer in the Doldunian language. This incident caused ratings to immediately plummet in East Doldunia. The Eel of Fortune producers want the ability to identify "problem words" for any given offensive word.
# 
# Write a function that, given a secret word and an offensive word, returns true if the board could theoretically display the offensive word (with no additional letters) during the course of solving the secret word.




#My logic: see if word can be made. If yes, see what remaining letters are. Are any of them in the bad word?

#Logic:
#break both words into single letters
#see if bad word has repeat letters. If yes, special case
#check if bad word can be spelt, and in proper order

#User Function
problem <- function(queryWord, badWord){
  queryLetters <- breakWord(queryWord)
  badLetters <- breakWord(badWord)
  duplicates <- FALSE
  if (length(unique(badLetters)) < length(badLetters)){
    duplicates <- TRUE
  }
  badMatches <- lapply(badLetters, function(X) grep(X, queryLetters))
  orderCheck <- checkOrder(badMatches)
  if (!(orderCheck[[1]])){
    output <- "Safe!"
  }
  else{
    output <- overlapCheck(orderCheck[[2]], queryLetters)

  }
  return(output)
}


#Checks whether any letters used to build the word are duplicated elsewhere in the word
#Compares letters used to build to word to those that are not used, checks for uniqueness
overlapCheck <- function(badLetterIndex, queryLetters){
  badLettersRebuilt <-queryLetters[badLetterIndex]
  remainingLetters <- queryLetters[-badLetterIndex]
  duplicatedLetters <- duplicated(c(badLettersRebuilt, unique(remainingLetters)))
  duplicateIndex <- which(duplicatedLetters == TRUE)
  if (length(duplicateIndex) == 0){ #No duplicates anywhere, even in the bad word itself
    output <- "Do not use"
  }
  else {
  if (max(duplicateIndex) > length(badLettersRebuilt)){
    output <- "Safe"
  }
  else{
    output <- "Do not use."
  }
  }
  return(output)
}

#Makes sure that letters occur in correct order
#Returns TRUE if word appears and in right order, returns false if the word is not there
checkOrder <- function(badMatches){
  badMatches[[1]] <- badMatches[[1]][which.min(badMatches[[1]])]
  possibleOrder <- badMatches[[1]]
  possibleOrder <- c(possibleOrder,sapply(seq(2, length(badMatches)), function(X) {
    appropriatePosition <- which(badMatches[[X]] > badMatches[[(X-1)]])
    if (length(appropriatePosition) == 0){
      0
    }
    else{
    badMatches[[X]] <- badMatches[[X]][appropriatePosition[1]] #Save the lowest occurance of the right letter for next iteration 
    badMatches[[X]]
    }
    }))
  checkForFail <- which(possibleOrder == 0)
  if (length(checkForFail) == 0){ #If the word is there, and in the right order
    output <- TRUE
  }
  else{
    output <- FALSE
  }
  return(list(output, possibleOrder))
}


#Breaks word into vector of individual letters
breakWord <- function(word){
  word <- strsplit(word, "")[[1]]
  return(word)
}