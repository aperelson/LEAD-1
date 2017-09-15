loadLEADSurvey <- function() {
  dirToUse <- 'C:\\Users\\Andrew\\Dropbox\\Development\\LEAD 1'
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
      }
    }
  })
}