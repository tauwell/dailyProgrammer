#Daily Challenge (done first in Java): https://www.reddit.com/r/dailyprogrammer/comments/3twuwf/20151123_challenge_242_easy_funny_plant/
#Works! And so much nicer than Java. 


funnyFruitCheck <- function(numPeople, startingFruit){
  fruitMatrix <- matrix(c(0,startingFruit),1,2, dimnames = list(c(),c('age','numFruit'))) #Matrix containing the initial 'coordinates'
  harvested <- startingFruit
  while (harvested < numPeople){
    fruitMatrix[,'age'] <- fruitMatrix[,'age'] + 1 #Increase age by one week
    updatedCounts <- fruitMatrix[,'age']*fruitMatrix[,'numFruit']
    harvested <- sum(updatedCounts)
    fruitMatrix <- rbind(matrix(c(0,harvested), 1, 2), fruitMatrix)
  }
  return(max(fruitMatrix[,'age']))
}

a <- funnyFruitCheck(200,15)
b <- funnyFruitCheck(50000,1)
d <- funnyFruitCheck(150000, 250)