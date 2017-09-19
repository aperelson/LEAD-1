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

returnTeamMemberId <- function(teamSelection, teamPosition) {
  switch(paste(teamSelection,teamPosition),
         'Red Coaching Team Rep' = 1,
         'Red Coaching Team Member 1' = 2,
         'Red Coaching Team Member 2' = 3,
         'Red Coaching Team Member 3' = 4,
         'Red Coaching Team Member 4' = 5,
         'Blue Coaching Team Rep' = 6,
         'Blue Coaching Team Member 1' = 7,
         'Blue Coaching Team Member 2' = 8,
         'Blue Coaching Team Member 3' = 9,
         'Blue Coaching Team Member 4' = 10,
         'Red Training Team Rep' = 11,
         'Red Training Team Member 1' = 12,
         'Red Training Team Member 2' = 13,
         'Red Training Team Member 3' = 14,
         'Red Training Team Member 4' = 15,
         'Blue Training Team Rep' = 16,
         'Blue Training Team Member 1' = 17,
         'Blue Training Team Member 2' = 18,
         'Blue Training Team Member 3' = 19,
         'Blue Training Team Member 4' = 20,
         'Red Coaching and Training Team Rep' = 21,
         'Red Coaching and Training Team Member 1' = 22,
         'Red Coaching and Training Team Member 2' = 23,
         'Red Coaching and Training Team Member 3' = 24,
         'Red Coaching and Training Team Member 4' = 25,
         'Blue Coaching and Training Team Rep' = 26,
         'Blue Coaching and Training Team Member 1' = 27,
         'Blue Coaching and Training Team Member 2' = 28,
         'Blue Coaching and Training Team Member 3' = 29,
         'Blue Coaching and Training Team Member 4' = 30,
         'Red Self-Managed Team Rep' = 31,
         'Red Self-Managed Team Member 1' = 32,
         'Red Self-Managed Team Member 2' = 33,
         'Red Self-Managed Team Member 3' = 34,
         'Red Self-Managed Team Member 4' = 35,
         'Blue Self-Managed Team Rep' = 36,
         'Blue Self-Managed Team Member 1' = 37,
         'Blue Self-Managed Team Member 2' = 38,
         'Blue Self-Managed Team Member 3' = 39,
         'Blue Self-Managed Team Member 4' = 40)
}


loadLEADSurvey <- function() {
  ##dirToUse <- 'C:\\Users\\Andrew\\Dropbox\\Development\\LEAD 1'
  dirToUse <- 'C:\\Users\\perel_000\\Documents\\GitHub\\LEAD-1'
  surveyType <- 0
  
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
        
        baseSurveyId <- paste0(weekNumber,'_',surveyType)
        
        if (surveyType == 1) {
          teamSelection <- returnValueFromVector(file.contents, c(10:21))
          teamPosition <- returnValueFromVector(file.contents, c(22:30))
          
          teamMemberId <- mapply(returnTeamMemberId, teamSelection, teamPosition)
          
          surveyId <- paste0(teamMemberId,'_',weekNumber,'_',surveyType)

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
          
          surveyFinal <- cbind(surveyId, teamMemberId, weekNumber)
          rownames(surveyFinal) <- c()
        }
      }
    }
  })
}