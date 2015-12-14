#The VHS Recording Problem
#Solved! (not bonus, but it's simple to extend existing objects to return bonus)
#https://www.reddit.com/r/dailyprogrammer/comments/3u6o56/20151118_challenge_242_intermediate_vhs_recording/



scheduler <- function(){#Run to start planning your viewing schedule! Totally the most efficient way.
  input <- getInput()
  TVGuide <- data.frame()
  for (i in 1:length(input)){
    TVGuide <- tableBuilder(input[i], TVGuide)
  }
  colnames(TVGuide) <- c("start.hour","start.minute","end.hour","end.minute")
  rownames(TVGuide) <- NULL
  TVGuide <- showLength(TVGuide)
  matchList <- list()
  TVGuide <- cbind(TVGuide, "Doesnt Conflict" = 0, numCompat = 0)
  compatList <- list()
  for (j in 1:nrow(TVGuide)){
    #print(j)
    matchList[[j]] <- checkConflict(TVGuide, j)
    TVGuide[j,'Doesnt Conflict'] <- paste(which(matchList[[j]] == TRUE), collapse = "")
    compatList[[j]] <- which(matchList[[j]] == TRUE)
    TVGuide[j, 'numCompat'] <- length(compatList[[j]])
  }
  #browser()
  output <- 0
  candidateNumShows <- nrow(TVGuide)
    while (output == 0){
    compatiNum <- which(TVGuide[,'numCompat'] >= candidateNumShows)
    numNumCompats <- length(compatiNum)
    
    
    if (numNumCompats >= candidateNumShows){
      # browser()
      checkResult <- checkCandidateSchedule2(compatiNum, candidateNumShows, compatList)
      if (length(checkResult) > 0)  {#If we actually found a solution
        output <- candidateNumShows
      }
      else {
        candidateNumShows <- candidateNumShows - 1
      }
    }
    else {
      candidateNumShows <- candidateNumShows - 1
    }
    }
  
  #list(TVGuide,matchList, compatList, output)
  return(output)
  }


# dog <- 0
# for (j in 1:(sqrt(16))){
#   dog <- dog + j
# }


getInput <- function(){
  input <- c()
  showNum <- 1
  exitMarker <- 0
  while (exitMarker == 0) {
    input[showNum] <- readline("Whatchoshos? ")
    if (input[showNum] == ""){
      exitMarker <- 1
      input <- input[1:showNum-1]
    }
    showNum <- showNum + 1
  }
  return(input)
}

tableBuilder <- function(singleShow, TVGuide){ #input is start/end time for single show, in string form
  #Break into strings with start and end time
  splitString <- strsplit(singleShow, "")[[1]]
  divideIndex <- which(splitString == " ")
  startTime.str <- splitString[1:divideIndex-1]
  endTime.str <- splitString[divideIndex+1: length(splitString)]
  startTime.hour <- as.numeric(paste0(startTime.str[1],startTime.str[2], collapse = ""))
  startTime.minute <- as.numeric(paste0(startTime.str[3],startTime.str[4], collapse = ""))
  endTime.hour <- as.numeric(paste0(endTime.str[1],endTime.str[2], collapse = ""))
  endTime.minute <- as.numeric(paste0(endTime.str[3],endTime.str[4], collapse = ""))
  
  #Add to table
  #browser()
  TVGuide <- rbind(TVGuide, t(data.frame(c(startTime.hour, startTime.minute, endTime.hour, endTime.minute))))
  return(TVGuide)
}


showLength <- function(TVGuide){
  numShows <- nrow(TVGuide)
  lengths <- c()
  for (i in 1:numShows){
    show <- TVGuide[i,]
    hourLength <- show[,'end.hour'] - show[,'start.hour']
    minuteLength <- show[,'end.minute'] - show[,'start.minute']
    lengths[i] <- minuteLength + 60*hourLength
  }
  TVGuide <- cbind(TVGuide, lengths)
  return(TVGuide)
}


checkConflict <- function(TVGuide, queryRow){ #return row nums that query conflicts with
  queryStartHour <- TVGuide[queryRow, 'start.hour']
  queryStartMinute <- TVGuide[queryRow, 'start.minute']
  queryEndHour <- TVGuide[queryRow, 'end.hour']
  queryEndMinute <- TVGuide[queryRow, 'end.minute']
  startHours <- TVGuide[-queryRow,'start.hour']
  endHours <- TVGuide[-queryRow, 'end.hour']
  startMinutes <- TVGuide[-queryRow,'start.minute']
  endMinutes <- TVGuide[-queryRow,'end.minute']
  result <- c()
  for (j in 1:length(startHours)){
    result[j] <- FALSE
    startHour <- startHours[j]
    startMinute <- startMinutes[j]
    endHour <- endHours[j]
    endMinute <- endMinutes[j]
    #Case 1: Is query fully before the other show?
    if (queryStartHour <=  startHour){
      if (queryStartHour < startHour){
        if (queryEndHour < startHour){
          result[j] <- TRUE
        }
        else if (queryEndHour == startHour){
          if (queryEndMinute <= startMinute){
            result[j] <- TRUE
          }
        }
      }
      else if (queryStartMinute <=  startMinute){
        if (queryEndHour < startHour){
          result[j] <- TRUE
        }
        else if (queryEndHour == startHour){
          if (queryEndMinute <= startMinute){
            result[j] <- TRUE
          }
        }
      } 
    }
    
    #Case 2: query is fully after the show
    if (queryStartHour >=  endHour){
      if (queryStartHour > endHour){
        result[j] <- TRUE
      }
      else if (queryStartHour == endHour){
        if (queryStartMinute >= endMinute){
          result[j] <- TRUE
        }
      }
    }
  }
  result <- append(result, TRUE, queryRow-1)
  return(result) #Will be added directly to matchList above
  
}

checkCandidateSchedule2 <- function(compatiNum, candidateNumShows, compatList){
  #Start by removing any possibilities in compatList that are not in compatiNum
  candidateCompatList <- compatList[compatiNum]
  output <- c()
  for (i in 1:length(compatiNum)){#Iterate over every candidate show, removing non eligible shows in compatList
    
    candidateCompatList[[i]] <- vectorClean(candidateCompatList[[i]], compatiNum)
  }
  #browser()
  #Now remove any show that doesn't have a sufficient number of remaining compatibilities
  candidateCompatList <- candidateCompatList[sapply(candidateCompatList, function(X) length(X) >= candidateNumShows)]
  if (length(candidateCompatList) >= candidateNumShows){#If we have enough shows with enough compatibilities, we're done!
    output <- candidateCompatList[[1]] #Pretty sure that they should all be the same, so 1.1 is arbitrary.
  }
  return(output)
}

vectorClean <- function(vector, requiredVector){#removes any elements in a vector that are not in requiredVector
  output <- c()
  for (i in (1:length(requiredVector))){
    matchIndex <- which(vector == requiredVector[i])
    if (length(matchIndex) > 0){
      output <- c(output, requiredVector[i])
    }
  }
  return(output)
  }

# checkCandidateSchedule <- function(compatiNum, TVGuide, compatList){
#   output <- TRUE
#   for (j in 1:length(compatiNum)){
#     currentQueryRow <- compatiNum[j]
#     currentCompats <- compatList[[currentQueryRow]]
#     #browser()
#     #print(j)
#     for (k in 1:length(currentCompats)){
#       matchIndex <- which(currentCompats == currentQueryRow)
#       if (length(matchIndex) == 0){
#         output <- FALSE
#       }
#     }
#   }
#   #browser()
#   return(output)
# }
