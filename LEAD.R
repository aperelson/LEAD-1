loadLEADSurvey <- function() { 
  library(readxl)

  dirToUse <- 'C:\\Users\\EZ002683\\Desktop\\LEAD 1'
  setwd(dirToUse)
  folder.names <- list.dirs(full.names = TRUE)

  result <- sapply(folder.names[-1], function(x) {
    file.names <- list.files(x, full.names = TRUE, pattern =".csv")  

    if (length(file.names))
    {
      for (j in 1:length(file.names)) {
        fileName <- file.names[j]
        
        file.contents <- read.csv(file=fileName, header=TRUE, sep=",")
      }
    }
  })
}