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

responseValue <- function(df) {
  switch(df,
         'Strongly Disagree' = 1,
         'Disagree'	= 2,
         'Neutral' = 3,
         'Agree' = 4,
         'Strongly Agree' = 5)
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

convert6ColumnsToRecordset <- function (sixCols, surveyId, surveyQId) {
  firstFive <- returnValueFromVector(sixCols, c(1:5))
  commentsCol <- sixCols[,c(6:6)]
  Value <- as.numeric(t(t(lapply(firstFive, responseValue))))
  returnDB <- data.frame(Value, commentsCol, surveyId)
  returnDB$SurveyQuestionID <- surveyQId
  returnDB
}


  dirToUse <- 'C:\\Development\\LEAD 1'
  surveyType <- 0
  Response <- data.frame()
  Feedback <- data.frame()
  Survey <- data.frame()
  
  setwd(dirToUse)
  folder.names <- list.dirs(full.names = TRUE, recursive = FALSE)

  if (length(folder.names))
  {
    for (k in 1:length(folder.names)) 
    {
      file.names <- list.files(folder.names[k], full.names = TRUE, pattern =".csv")  
      
      if (length(file.names))
      {
        for (j in 1:length(file.names)) 
        {
          fileName <- file.names[j]
          
          file.contents <- read.csv(file=fileName, header=TRUE, sep=",")
          file.contents <- file.contents[-c(1),]
          
          ##Determine some basic info:
          weekNumber <- strftime(as.Date(substr(unique(file.contents[1,3]),1,10),"%m/%d/%Y"),format="%W") 
          weekNumber <- toString(as.numeric(weekNumber) - 36)
          
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
          
          teamMemberId <- mapply(returnTeamMemberId, teamSelection, teamPosition)
          
          surveyId <- paste0(teamMemberId,'_',weekNumber,'_',surveyType)
          
          Survey <- rbind(Survey, cbind(surveyId, teamMemberId, weekNumber))

          if (surveyType == 1) {
            tookTimeToReflectDB <- convert6ColumnsToRecordset(file.contents[31:36], surveyId, 203)
            easyToKeepTrackOfThoughtsDB <- convert6ColumnsToRecordset(file.contents[37:42], surveyId, 204)
            feltMembersOfTeamFocusedDB <- convert6ColumnsToRecordset(file.contents[43:48], surveyId, 205)
            feltTeamRepHelpedDB <- convert6ColumnsToRecordset(file.contents[49:54], surveyId, 206)
            tookTimeToThinkAboutMyActionsDB <- convert6ColumnsToRecordset(file.contents[55:60], surveyId, 207)
            feelMyTeamThoughtAboutConsequencesDB <- convert6ColumnsToRecordset(file.contents[61:66], surveyId, 208)
            feltTeamRepTookTimeDB <- convert6ColumnsToRecordset(file.contents[67:72], surveyId, 209)
            managedToAchieveDB <- convert6ColumnsToRecordset(file.contents[73:78], surveyId, 210)
            feltMyTeamAchievedDB <- convert6ColumnsToRecordset(file.contents[79:84], surveyId, 211)
            feltTeamRepAchievedDB <- convert6ColumnsToRecordset(file.contents[85:90], surveyId, 212)
            feelIGaveMyBestDB <- convert6ColumnsToRecordset(file.contents[91:96], surveyId, 213)
            feelMyTeamDidTheirBestDB <- convert6ColumnsToRecordset(file.contents[97:102], surveyId, 214)
            feelTeamRepRecognitionDB <- convert6ColumnsToRecordset(file.contents[103:108], surveyId, 215)
            feelTeamRepHelpedDB <- convert6ColumnsToRecordset(file.contents[109:114], surveyId, 216)
  
            Response <-  rbind(Response, tookTimeToReflectDB, easyToKeepTrackOfThoughtsDB,
                                       feltMembersOfTeamFocusedDB, feltTeamRepHelpedDB, tookTimeToThinkAboutMyActionsDB,
                                       feelMyTeamThoughtAboutConsequencesDB, feltTeamRepTookTimeDB, managedToAchieveDB,
                                       feltMyTeamAchievedDB, feltTeamRepAchievedDB, feelIGaveMyBestDB, 
                                       feelMyTeamDidTheirBestDB, feelTeamRepRecognitionDB, feelTeamRepHelpedDB)
            
            Feedback <- rbind(Feedback, data.frame(surveyId, returnValueFromVector(file.contents, c(115:116))))
          }
          else if (surveyType == 0) {
            feelPositiveAndEnergisedDB <- convert6ColumnsToRecordset(file.contents[31:36], surveyId, 103)
            thisWeekIsGoingToBeGoodDB <- convert6ColumnsToRecordset(file.contents[37:42], surveyId, 104)
            feelTeamPositiveAndEnergisedDB <- convert6ColumnsToRecordset(file.contents[43:48], surveyId, 105)
            feelRepEnergisedAndMotivatedDB <- convert6ColumnsToRecordset(file.contents[49:54], surveyId, 106)
            knowWhatMeantToDoDB <- convert6ColumnsToRecordset(file.contents[55:60], surveyId, 107)
            believeICanAchieveDB <- convert6ColumnsToRecordset(file.contents[61:66], surveyId, 108)
            feelTeamKnowsWhatToDoDB <- convert6ColumnsToRecordset(file.contents[67:72], surveyId, 109)
            believeTeamCanAchieveDB <- convert6ColumnsToRecordset(file.contents[73:78], surveyId, 110)
            feelTeamRepHelpedDB <- convert6ColumnsToRecordset(file.contents[79:84], surveyId, 111)
            feelTeamRepSetRealisticGoalsDB <- convert6ColumnsToRecordset(file.contents[85:90], surveyId, 112)
            
            Response <-  rbind(Response, feelPositiveAndEnergisedDB, thisWeekIsGoingToBeGoodDB,
                                       feelTeamPositiveAndEnergisedDB, feelRepEnergisedAndMotivatedDB, knowWhatMeantToDoDB,
                                       believeICanAchieveDB, feelTeamKnowsWhatToDoDB, believeTeamCanAchieveDB,
                                       feelTeamRepHelpedDB, feelTeamRepSetRealisticGoalsDB)
          }
        }
      }
    }
  }
    
  rownames(Survey) <- c()
  rownames(Response) <- c()
  rownames(Feedback) <- c()
  x <- "SurveyQuestionID"
  Response <- Response[c(x, setdiff(names(Response), x))]
  colnames(Response) <- c("SurveyQuestionID", "Value", "Comment", "SurveyID")
  colnames(Feedback) <- c("SurveyID", "Provided")
  
  rm(tookTimeToReflectDB, easyToKeepTrackOfThoughtsDB, feltMembersOfTeamFocusedDB, feltTeamRepHelpedDB,
     tookTimeToThinkAboutMyActionsDB, feelMyTeamThoughtAboutConsequencesDB, feltTeamRepTookTimeDB,
     managedToAchieveDB, feltMyTeamAchievedDB, feltTeamRepAchievedDB, feelIGaveMyBestDB, 
     feelMyTeamDidTheirBestDB, feelTeamRepRecognitionDB, feelTeamRepHelpedDB, feelPositiveAndEnergisedDB,
     thisWeekIsGoingToBeGoodDB, feelTeamPositiveAndEnergisedDB, feelRepEnergisedAndMotivatedDB,
     knowWhatMeantToDoDB, believeICanAchieveDB, feelTeamKnowsWhatToDoDB, believeTeamCanAchieveDB, 
     feelTeamRepSetRealisticGoalsDB, file.contents)
