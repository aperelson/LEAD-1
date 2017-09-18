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
        easyToKeepTrackOfThoughts <- returnValueFromVector(file.contents, c(37:41))
        easyToKeepTrackOfThoughtsComments <- file.contents[,c(42:42)]
        feltMembersOfTeamFocused <- returnValueFromVector(file.contents, c(43:47))
        feltMembersOfTeamFocusedComments <- file.contents[,c(48:48)]
        feltTeamRepHelped <- returnValueFromVector(file.contents, c(49:53))
        feltTeamRepHelpedComments <- file.contents[,c(54:54)]
        tookTimeToThinkAboutMyActions <- returnValueFromVector(file.contents, c(55:59))
        tookTimeToThinkAboutMyActionsComments <- file.contents[,c(60:60)]
        feelMyTeamThoughtAboutConsequences <- returnValueFromVector(file.contents, c(61:65))
        feelMyTeamThoughtAboutConsequencesComments <- file.contents[,c(66:66)]
        feltTeamRepTookTime <- returnValueFromVector(file.contents, c(67:71))
        feltTeamRepTookTimeComments <- file.contents[,c(72:72)]
        managedToAchieve <- returnValueFromVector(file.contents, c(73:77))
        managedToAchieveComments <- file.contents[,c(78:78)]
        feltMyTeamAchieved <- returnValueFromVector(file.contents, c(79:83))
        feltMyTeamAchievedComments <- file.contents[,c(84:84)]
        feltTeamRepAchieved <- returnValueFromVector(file.contents, c(85:89))
        feltTeamRepAchievedComments <- file.contents[,c(90:90)]
        feelIGaveMyBest <- returnValueFromVector(file.contents, c(91:95))
        feelIGaveMyBestComments <- file.contents[,c(96:96)] 
        feelMyTeamDidTheirBest <- returnValueFromVector(file.contents, c(97:101))
        feelMyTeamDidTheirBestComments <- file.contents[,c(102:102)] 
        feelTeamRepRecognition <- returnValueFromVector(file.contents, c(103:107))
        feelTeamRepRecognitionComments <- file.contents[,c(108:108)] 
        feelTeamRepHelped <- returnValueFromVector(file.contents, c(109:113))
        feelTeamRepHelpedComments <- file.contents[,c(114:114)] 
        providedFeedback <- returnValueFromVector(file.contents, c(115:116)) 
      }
    }
  })
}