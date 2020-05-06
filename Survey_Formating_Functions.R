#FUNCTIONS

#Split Column
splitColumn <- function(data, column) {
  names <- colnames(data)
  columnA <- paste(toString(column), "a", sep = "")
  columnB <- paste(toString(column), "b", sep = "")
  namesC <- append(names, columnA)
  namesC <- append(namesC, columnB)
  
  vA <- vector()
  vB <- vector()

  for(row in 1:nrow(data)){
    newValue = data[row, column]
    if(is.na(newValue)){
      vA <- append(vA, "Null")
      vB <- append(vB, "Null")
    }
    else {
      split <- strsplit(toString(newValue), ",")
      vA <- append(vA, split[[1]][1])
      vB <- append(vB, split[[1]][2])
    }
  }
  newDF <- cbind(data, vA, vB)
  names(newDF) <- namesC
  data$column <- NULL
  return(newDF)
}

#Split Columns
splitColumns <- function(data, index1, index2) {
  newSeq <- seq(index1, index2, by = 1)
  newDF <- data
  for(i in newSeq) {
    name <- names(newDF)[i]
    newDF <- splitColumn(newDF, name)
  }
  newDF <- newDF[-c(index1:index2)]
  return(newDF)
}

#Format Entry
NAConverter <- function(entry) {
  newEntry <- entry
  if(is.na(entry)) {
    newEntry <- ""
  }
  return(newEntry)
}

#Format Column
formatColumn <- function(data, index) {
  nameList <- names(data)
  name <- names(data)[index]
  nameList <- append(nameList, name)
  result <- vector()
  for(row in 1:nrow(data)){
    newValue = data[row, index]
    newValue = NAConverter(newValue)
    result <- append(result, newValue)
  }
  newDF <- cbind(data, result)
  names(newDF) <- nameList
  return(newDF)
}

#Format Many Columns
makeFormattedColumns <- function(data, index1, index2) {
  newSeq <- seq(index1, index2, by = 1)
  newDF <- data
  for(i in newSeq) {
    name <- names(newDF)[i]
    newDF <- formatColumn(newDF, i)
  }
  newDF <- newDF[-c(index1:index2)]
  return(newDF)
}

#ID Fix
fixIDColumn <- function(data, index) {
  nameList <- names(data)
  name <- names(data)[index]
  nameList <- append(nameList, name)
  result <- vector()
  for(row in 1:nrow(data)) {
    origString <- data[row, index]
    
    newCheck <- as.integer(gsub("[a-zA-z]", "", origString))
    
    if(newCheck < 10000) {
      newCheck <- 0
    }
    
    newString <- newCheck
    result <- append(result, newString)
  }
  newDF <- cbind(data, result)
  names(newDF) <- nameList
  newDF <- newDF[-c(index)]
  return(newDF)
}

#Build Survey Table
makeSurveyTable <- function(rawdata) {
  activeData <- rawdata
  activeData <- activeData[-c(1:3, 5:7)]
  activeData <- fixIDColumn(activeData, 1)
  activeData <- cbind(activeData[79], activeData[1:78])
  activeData <- splitColumns(activeData, 47, 70)
  activeData <- cbind(activeData[1:46], activeData[56:103], 
                      activeData[47:55])
  activeData <- makeFormattedColumns(activeData, 1, 103)
  return(activeData)
}

#Create blank name check list - Not Used
createBlankNameCheck <- function() {
  nameCheck <- data.frame("Student_ID" = c(""))
  write.csv(nameCheck,"nameCheck.csv", row.names = FALSE)
}

#Check names and build list of new names
checkNames <- function(rawdata) {
  nameCheck <- read.csv("nameCheck.csv", header=TRUE)
  fixed <- fixIDColumn(rawdata, 4)
  fixed <- cbind(fixed[1:3], fixed[85], fixed[4:84])
  namesOnly <- fixed[1:7]
  newNames <- namesOnly[0, ]
  idOnly <- data.frame(namesOnly[, 4])
  
  if (dim(nameCheck)[1] == 0) {
    newNames <- namesOnly
    nameCheck <- namesOnly[4]
  }
  else {
    for(ID in 1:nrow(namesOnly)) {
      repeatedID <- FALSE
      ID1 <- as.numeric(namesOnly[ID, 4])
      for(SID in 1:nrow(nameCheck)) {
        ID2 <- as.numeric(nameCheck[SID, 1])
        if(ID1 == ID2) {
          repeatedID <- TRUE
        }
      }
      if(repeatedID == FALSE) {
        tempFrame <- namesOnly[ID,]
        tempFrame2 <- data.frame(idOnly[ID,])
        names(tempFrame2) <- c("Student_ID")
        newNames <- rbind(newNames, tempFrame)
        nameCheck <- rbind(nameCheck, tempFrame2)
      }
    }
  }
  write.csv(nameCheck,"nameCheck.csv", row.names = FALSE)
  return(newNames)
}

#Make new student table
makeStudentTable <- function(rawdata) {
  newNames <- checkNames(rawdata)
  
  if(nrow(newNames) == 0) {
    print("There are no new students to add to the student table")
    MasterNameFrame <- cbind(newNames[4], newNames[1:3], newNames[5:7])
    return(MasterNameFrame)
  }
  
  newNames <- makeFormattedColumns(newNames, 1, 7)
  
  MasterNameFrame <- cbind(newNames[4], newNames[1:3], newNames[5:7])
  return(MasterNameFrame)
}
