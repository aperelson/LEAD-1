# function to merge columns into one with a space separator and then
# remove multiple spaces
returnValueFromVector <- function(df, cols) {
  exp <- paste('df[,', cols, ']', sep='', collapse=',' )
  nexp <- paste(" paste(", exp, ", sep=' ')")
  newcol <- eval(parse(text=nexp))
  newcol <- gsub('  *', ' ', newcol)
  newcol <- gsub('^ *', '', newcol)
  gsub(' *$', '', newcol)
}


loadLEADSurvey <- function() {
  ##dirToUse <- 'C:\\Users\\Andrew\\Dropbox\\Development\\LEAD 1'
  dirToUse <- 'C:\\Users\\perel_000\\Documents\\GitHub\\LEAD-1'

  setwd(dirToUse)
  folder.names <- list.dirs(full.names = TRUE, recursive = FALSE)
  
  result <- sapply(folder.names[-1], function(x) {
    file.names <- list.files(x, full.names = TRUE, pattern =".csv")  
    
    if (length(file.names))
    {
      for (j in 1:length(file.names)) {
        fileName <- file.names[j]
        
        file.contents <- read.csv(file=fileName, header=TRUE, sep=",")
        file.contents <- file.contents[-c(1),]
        
        ##Determine some basic info:
        weekNumber <- strftime(as.Date(substr(unique(file.contents[1,3]),1,10),"%m/%d/%Y"),format="%W") 
        
        if (grepl('The week to come', fileName))
        {
          surveyType <- 0
        }
        else
        {
          surveyType <- 1
        }
        
        teamSelection <- returnValueFromVector(file.contents, c(10:21))
        teamPosition <- returnValueFromVector(file.contents, c(22:30))
        tookTimeToReflect <- returnValueFromVector(file.contents, c(31:35))
        tookTimeToReflectComments <- file.contents[,c(36:36)]
      }
    }
  })
}