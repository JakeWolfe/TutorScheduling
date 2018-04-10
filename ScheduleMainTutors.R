#Main Work for MLC Scheduling
# Reference: https://www.uwlax.edu/urc/jur-online/PDF/2016/Meyers-Morrison.Jack-Daniel.MTH.pdf

#Librarys
library("phonTools")
library("lpSolve")
library("plyr")
library("readr")
library("shiny")

rm(list = ls())

# Printf fuction for debugging
printf <- function(...) cat(sprintf(...))


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
        printf("looking at %s at hour %d, at day %d, with worth %d\n", name, hour, day, worth*3)
        #Assign hour of day to tutor = 100
        schedule[name, (day - 2)*numHours + hour] = worth*3
        
        
      #Normal requesting time
      } else if(name != ""){
        
        gsub("^\\s+|\\s+$", "", name)
        worth = rowSums(classes)[name] + 1
        printf("looking at %s at hour %d, at day %d, with worth %d\n", name, hour, day, worth)
        #Assign hour of day to tutor = 20
        schedule[name, (day - 2)*numHours + hour] = worth

        
      } #Else no change
      
    }
    
  }
  
  
  return(schedule)
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
      
      printf("At day %d, hour %d, looking at %d\n", day, hour, signedUpAtTime[(day*numHours)+hour])
    
      totalAtTime <- signedUpAtTime[(day*numHours)+hour]
      
      if(totalAtTime < dailyHours[hour,day+1]) {
        
        dailyHours[hour,day+1] = totalAtTime
        
      }
      
    }
    
  }
  
  print(dailyHours)
  
  return(as.vector(t(t(dailyHours))))
  
}

getStatistics <- function() {
  
  
}

#Main work, call when user finishes input
getSchedule <- function(signUpScheduleFile, tutorHoursFile, classesFile, 
                        dailyHoursFile, openHour, closeHour, numDays){
  
  #Constants Calculated
  print('Start Work')
  numHours = closeHour - openHour
  
  #Read files inputed
  print('Read File Signup')
  signUp <- read.csv(file = signUpScheduleFile$datapath, header = TRUE)
  signUp[is.na(signUp)] <- ""
  print('Read File Classes')
  classes <- read.csv(file = classesFile$datapath, header = TRUE, row.names = 1)
  print('Read File Hours')
  hours <- read.csv(file = tutorHoursFile$datapath, header = TRUE, row.names = 1)
  print('Read File DailyHours')
  dailyHours <- read.csv(file = dailyHoursFile$datapath, header = TRUE, row.names = 1)
  
  #Get rid of empty rows
  print('Remove empty rows')
  signUp <- signUp[!apply(is.na(signUp) | signUp == "", 1, all),]
  
  
  #Get signUp in usable format. One row per tutor with every col=day*numDays + hour
  print('Get weightedSignUp')
  weightedSignUp <- getDayPrefs(signUp, classes, numHours, numDays)
  print('Get weights vector')
  weightsVector <- as.vector(t(weightedSignUp))

  #Make Constraints
  print('Get weekly min Constraints Matrix')
  minConstraintMatrix <- getTutorWeeklyConstraints(hours, numHours, numDays)
  print('Get weekly max Constraints Matrix')
  maxConstraintMatrix <- getTutorWeeklyConstraints(hours, numHours, numDays)
  print('Get daily max Constraints Matrix')
  dailyConstaintsMatrix <- getDailyConstraints(dailyHours, hours, numHours, numDays)
  
  #Combined Constraints
  print('Combine Constraints')
  constraints <- list(minConstraintMatrix, maxConstraintMatrix, dailyConstaintsMatrix)
  constraints <- do.call(rbind, constraints)
  
  print('Get and combine directions')
  #Make Directions
  directionMin <- rep(">=", nrow(minConstraintMatrix))
  directionMax <- rep("<=", nrow(maxConstraintMatrix))
  directionDaily <- rep("<=", nrow(dailyConstaintsMatrix))
  
  #Combine Directions
  directions <- c(directionMin, directionMax, directionDaily)
  
  #Make Values
  #hours$HoursMin <- 0
  print('Adjust daily values and combine')
  hours$HoursMax[is.na(hours$HoursMax)] <- 5
  dailyValues <- adjustDailyValues(dailyHours, weightedSignUp, numHours, numDays)
  
  #Combine Values
  values <- c(hours$HoursMin, hours$HoursMax, dailyValues)
  
  #Solve maximize system
  print('Solve system')
  solvedSystem <- lp("max", weightsVector, constraints, directions, values, all.bin=TRUE)
  
  # If there is no solution, return null. Error Case.
  if (sum(solvedSystem$solution) == 0) {
    return(NULL)
  }
  
  print('Reformat and prepare master schedule')
  schedule <<- as.data.frame(matrix(solvedSystem$solution, nrow = nrow(weightedSignUp), ncol(weightedSignUp), byrow = T))
  print('Reformat master schedule rownames')
  rownames(schedule) <- rownames(classes)
  
  print('Convert master schedule times')
  for (day in 0:(numDays-1)) {
    colnames(schedule)[((day*numHours)+1):((day+1)*numHours)] <- format(strptime(openHour:(closeHour-1), "%H"), "%I %p")
  }
  
  scheduleList = list()
  # 
  # print('Create Daily schedules')
  # for (i in 1:numDays) {
  #   daySchedule <- schedule[,((i-1)*numHours+1):(i*numHours)]
  #   daySchedule <- daySchedule[rowSums(daySchedule)!=0,]
  #   if(nrow(daySchedule) > 0) {
  #     scheduleList[[i]] <- ifelse(daySchedule == 0, "", "======")
  #   } else {
  #     daySchedule <- schedule[,((i-1)*numHours+1):(i*numHours)]
  #     scheduleList[[i]] <- ifelse(daySchedule == 0, "", "======")
  #   }
  # }
  
  print('Start stats')
  scheduleList$Stats$HourCounts <- as.data.frame(rowSums(schedule))
  
  print('Create readable schedule')
  readableSchedule <- matrix("", nrow = numHours, ncol = numDays)
  
  print('Fix readble schedule empty new lines')
  for(day in 1:numDays) {
    for(hour in 1:numHours) {
      readableSchedule[hour,day] <- paste(rownames(schedule)[schedule[,(((day-1)*numHours)+hour)] == 1], collapse = "<br/>")
    }
  }
  
  print('Fix readble schedule times')
  rownames(readableSchedule) <- paste(format(strptime(openHour:(closeHour-1), "%H"), "%I %p"),"-", format(strptime((openHour+1):(closeHour), "%H"), "%I %p"))
  print('Fix readble schedule days')
  colnames(readableSchedule) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Sunday")
  
  print('Final assign')
  scheduleList$readableSchedule <- as.data.frame(readableSchedule)
  
  return(scheduleList)
}