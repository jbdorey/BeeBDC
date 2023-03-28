

manualcorrectR <- function(
    HomePath = HomePath,
    SynFile = DLdf
    ){
# Import corrections data frame from excel sheet
Corrections_df <- readxl::read_excel( paste(HomePath, "/Corrections_df.xlsx", sep =""), sheet = "corrections")

# Find the row number of the matches in the original dataframe and add them to Corrections_df
Corrections_df$MatchTo_validName <- as.data.frame(match(Corrections_df$MatchTo_validName,DLdf$validName ))

# Check that all matches are valid — will throw an error and open the file OR tell you all's good.
if(sum(is.na(Corrections_df$MatchTo_validName)) != 0){
  print(paste("WARNING: there are", sum(is.na(Corrections_df$MatchTo_validName)), "non-matches", sep = " ") )
  View(Corrections_df)
}else{print("All matches well here, my dude")} # END if else match check

# Replace the rows that required human-interventions
for(i in 1:nrow(Corrections_df)){
  # Find the original row number for Corrections_df row "i"
  OriginalRowNum <- Corrections_df$MatchTo_validName[i,]
  SynFile[OriginalRowNum,] <- as.data.frame(Corrections_df[i,1:(ncol(Corrections_df)-2)])
} # END Replace correction rows FOR loop
} # END function