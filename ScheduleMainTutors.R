#Main Work for MLC Scheduling

#Librarys
library("phonTools")
library("lpSolve")
library("plyr")
library("readr")
library("shiny")

rm(list = ls())


getDayPrefs <- function(signUp, classes, numHours, numDays){
  
  #Get tutor names
  tutorsNames <- rownames(classes)
  
  #Make a matrix with rows = number of tutor and cols = numHours * numDays
  #Start with all times being bad
  schedule <- matrix(-999, nrow = nrow(classes), ncol = numHours*numDays)
  rownames(schedule) = trimws(tutorsNames)
  
  #Remember that every row in this matrix represents one person's schedule for the week
  
  hour = 0
  for(row in 1:nrow(signUp)){
    
    #See if this row is a new hou, if it is, increase hour by 1
    if(!(signUp[row, 1] == "") && !(is.na(signUp[row, 1]))){
      hour = hour+1
    }
    
    #Look at every day and if a person in signed up there, update the schedule
    for(day in 2:ncol(signUp)){
      
      #Fix spaces in input
      name = gsub("^\\s+|\\s+$", "", signUp[row, day])
      
      
      #Check for perfered time slot
      if(grepl("\\*", name)){
        
        name = gsub("^\\s+|\\s+$", "", gsub("\\*", "", name))
        worth = rowSums(classes)[name] + 1
        #Assign hour of day to tutor = 100
        schedule[name, (day - 2)*numHours + hour] = worth*3
        
        
      #Normal requesting time
      } else if(name != ""){
        
        worth = rowSums(classes)[name] + 1
        #Assign hour of day to tutor = 20
        schedule[name, (day - 2)*numHours + hour] = worth

        
      } #Else no change
      
    }
    
  }
  
  
  return(schedule)
}

getMinPrioTutorsConstraints <- function(hours, numHours, numDays, prioStart, prioEnd, signUp){
  
  ConstraintMatrix <- matrix(0, nrow = (numDays*numHours), ncol = (numHours*numDays*nrow(hours)))
  
  
      
  for(day in 1:numDays) {
    
    for(hour in 1:numHours) {
      
      for(tutor in 0:(nrow(hours)-1)) {
        
        if(hour >= prioStart | hour <= prioEnd) {
          
          ConstraintMatrix[((day-1)*numHours)+(hour), ((tutor*(numHours*numDays))+((day-1)*numHours)+hour)] = 1
          
        }
      }
      
    }
    
  }
    
  
  
  #Remove hours who have noone signed up for it
  ConstraintMatrix <- ConstraintMatrix[apply(signUp,2,max) > 0,]

  return(ConstraintMatrix)
}

getTutorWeeklyConstraints <- function(hours, numHours, numDays){
  
  #List to hold contraint arrays
  ConstraintMatrix <- matrix(0, nrow = nrow(hours), ncol = (numHours*numDays*nrow(hours)))
  
  for(tutor in 0:(nrow(hours)-1)){
    
    for(hour in 1:((numHours*numDays))){
      
      ConstraintMatrix[tutor+1, (tutor*numHours*numDays) + hour] = 1
      
    }
    
  }
  
  
  return(ConstraintMatrix)
  
}

getDailyConstraints <- function(dailyHours, hours, numHours, numDays) {
  
  ConstraintMatrix <- matrix(0, nrow = numHours*numDays, ncol = (numHours*numDays*nrow(hours)))
  
  for(day in 0:(numDays-1)){
    
    for(hour in 1:numHours) {
      
      for(tutor in 0:(nrow(hours)-1)) {
        
        ConstraintMatrix[(day*numHours)+hour, (tutor*numHours*numDays)+(day*numHours)+hour] = 1
        
      }
      
    }
    
  }
  
  return(ConstraintMatrix)
}

adjustDailyValues <- function(dailyHours, signUp, numHours, numDays) {
  
  signedUpAtTime <- as.vector(colSums(signUp > 0))
  
  for(day in 0:(numDays-1)) {
    
    for(hour in 1:numHours) {
    
      totalAtTime <- signedUpAtTime[(day*numHours)+hour]
      
      if(totalAtTime < dailyHours[hour,day+1]) {
        
        dailyHours[hour,day+1] = totalAtTime
        
      }
      
    }
    
  }
  
  return(as.vector(t(t(dailyHours))))
  
}

getStatistics <- function() {
  
  
}

#Main work, call when user finishes input
getSchedule <- function(signUpScheduleFile, tutorHoursFile, classesFile, 
                        dailyHoursFile, openHour, closeHour, numDays){
  
  #Constants Calculated
  numHours = closeHour - openHour
  
  #Read files inputed
  signUp <- read.csv(file = signUpScheduleFile$datapath, header = TRUE)
  signUp[is.na(signUp)] <- ""
  classes <- read.csv(file = classesFile$datapath, header = TRUE, row.names = 1)
  hours <- read.csv(file = tutorHoursFile$datapath, header = TRUE, row.names = 1)
  dailyHours <- read.csv(file = dailyHoursFile$datapath, header = TRUE, row.names = 1)
  
  #Get rid of empty rows
  signUp <- signUp[!apply(is.na(signUp) | signUp == "", 1, all),]
  
  #Get signUp in usable format. One row per tutor with every col=day*numDays + hour
  weightedSignUp <- getDayPrefs(signUp, classes, numHours, numDays)
  weightsVector <- as.vector(t(weightedSignUp))

  #Make Constraints
  minConstraintMatrix <- getTutorWeeklyConstraints(hours, numHours, numDays)
  maxConstraintMatrix <- getTutorWeeklyConstraints(hours, numHours, numDays)
  dailyConstaintsMatrix <- getDailyConstraints(dailyHours, hours, numHours, numDays)
  
  #Combined Constraints
  constraints <- list(minConstraintMatrix, maxConstraintMatrix, dailyConstaintsMatrix)
  constraints <- do.call(rbind, constraints)
  
  #Make Directions
  directionMin <- rep(">=", nrow(minConstraintMatrix))
  directionMax <- rep("<=", nrow(maxConstraintMatrix))
  directionDaily <- rep("<=", nrow(dailyConstaintsMatrix))
  
  #Combine Directions
  directions <- c(directionMin, directionMax, directionDaily)
  
  #Make Values
  #hours$HoursMin <- 0
  hours$HoursMax[is.na(hours$HoursMax)] <- 5
  dailyValues <- adjustDailyValues(dailyHours, weightedSignUp, numHours, numDays)
  
  #Combine Values
  values <- c(hours$HoursMin, hours$HoursMax, dailyValues)
  
  #Solve maximize system
  solvedSystem <- lp("max", weightsVector, constraints, directions, values, all.bin=TRUE)
  
  if (sum(solvedSystem$solution) == 0) {
    return(NULL)
  }
  
  schedule <<- as.data.frame(matrix(solvedSystem$solution, nrow = nrow(weightedSignUp), ncol(weightedSignUp), byrow = T))
  rownames(schedule) <- rownames(classes)
  
  for (day in 0:(numDays-1)) {
    colnames(schedule)[((day*numHours)+1):((day+1)*numHours)] <- format(strptime(openHour:(closeHour-1), "%H"), "%I %p")
  }
  
  scheduleList = list()
  
  for (i in 1:numDays) {
    daySchedule <- schedule[,((i-1)*numHours+1):(i*numHours)]
    daySchedule <- daySchedule[rowSums(daySchedule)!=0,]
    if(nrow(daySchedule) > 0) {
      scheduleList[[i]] <- ifelse(daySchedule == 0, "", "======")
    } else {
      daySchedule <- schedule[,((i-1)*numHours+1):(i*numHours)]
      scheduleList[[i]] <- ifelse(daySchedule == 0, "", "======")
    }
  }
  
  scheduleList$Stats$HourCounts <- as.data.frame(rowSums(schedule))
  
  scheduleList$fullSchedule <- schedule
  
  readableSchedule <- matrix("", nrow = numHours, ncol = numDays)
  
  for(day in 1:numDays) {
    for(hour in 1:numHours) {
      readableSchedule[hour,day] <- paste(rownames(schedule)[schedule[,(((day-1)*numHours)+hour)] == 1], collapse = "<br/>")
    }
  }
  
  rownames(readableSchedule) <- paste(format(strptime(openHour:(closeHour-1), "%H"), "%I %p"),"-", format(strptime((openHour+1):(closeHour), "%H"), "%I %p"))
  colnames(readableSchedule) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Sunday")
  
  scheduleList$readableSchedule <- as.data.frame(readableSchedule)
  
  return(scheduleList)
}