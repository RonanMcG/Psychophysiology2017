
# install and load required packages #
install.packages("reshape2")
install.packages("zoo")
install.packages("plyr")
library(plyr)
library(reshape2)
library(zoo)

# Loading 'Easy' and 'Hard' experimental blocks separately for by-block analysis #
dataEasy <- read.delim(file.path("C:", "Work", "Block study ppt1 Easy.txt")) # Loads 'Easy' data file  
dataHard <- read.delim(file.path("C:", "Work", "Block study ppt1 Hard.txt")) # Loads 'Hard' data file  

dataEasy <- read.delim(file.path("C:", "R Scripts and Data", "PhD Stuff", "Adult", "Block study ppt45 Easy.txt")) # Loads 'Easy' data file  
dataHard <- read.delim(file.path("C:", "R Scripts and Data", "PhD Stuff", "Adult", "Block study ppt45 Hard.txt")) # Loads 'Hard' data file  

# Mark missing values as NA #
dataEasy$RIGHT_PUPIL_SIZE[dataEasy$RIGHT_PUPIL_SIZE=="."] <- NA # marks missing values as NA
dataHard$RIGHT_PUPIL_SIZE[dataHard$RIGHT_PUPIL_SIZE=="."] <- NA # marks missing values as NA 

# creates new column ('TrialTime') that indexes the trial time (in ms) #
dataEasy$TrialTime <- dataEasy$TIMESTAMP - dataEasy$TRIAL_START_TIME 
dataHard$TrialTime <- dataHard$TIMESTAMP - dataHard$TRIAL_START_TIME 

# Baseline = 10000-11000 ms (10 seconds incorporated to allow pupil size to return to resting state). Subset to include first 13 seconds of listening data post-baseline only as this was the duration of the shortest passage #
dataEasy <- subset(dataEasy, TrialTime<24001) 
dataHard <- subset(dataHard, TrialTime<24001) 

# number each trial to allow 'by-trial' (i.e., time) analysis # 
dataEasy$Trial_no <-  rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46), each = 24001)
dataHard$Trial_no <-  rep(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46), each = 24001)

# any pupil size value not in fixation interest area is marked NA #
dataEasy$RIGHT_PUPIL_SIZE[dataEasy$RIGHT_INTEREST_AREA_LABEL=="."] <- NA
dataHard$RIGHT_PUPIL_SIZE[dataHard$RIGHT_INTEREST_AREA_LABEL=="."] <- NA

# Subsetting data to analyze either 1st or 2nd half of the experimental block only. For analysis of second half, Change to >23 #
dataEasy <- subset(dataEasy, Trial_no<24)
dataHard <- subset(dataHard, Trial_no<24)

# Analysing correct responses only #
EasyCorrectOnly <- subset(dataEasy, Response_accuracy=='1')
HardCorrectOnly <- subset(dataHard, Response_accuracy=='1')

# Getting rid of trials for which there were >50% missing data #

# Step 1: reshapes data into wide format with trial (audio) as the columns #
EasyAudio <- dcast(EasyCorrectOnly, TrialTime ~ audio, value.var = "RIGHT_PUPIL_SIZE") 
HardAudio <- dcast(HardCorrectOnly, TrialTime ~ audio, value.var = "RIGHT_PUPIL_SIZE")

# Step 2: create function which tells you proportion of NA values for each trial #
propmiss <- function(dataframe) lapply(dataframe,function(x) data.frame(nmiss=sum(is.na(x)), n=length(x), propmiss=sum(is.na(x))/length(x))) 

# Step 3: Returns proportion of NAs for each trial #
propmiss(EasyAudio)
propmiss(HardAudio)

# At this stage, manually remove columns for which propmiss > 0.50, for example: HardAudio$"129_hard.wav" <- NULL #

# Once >50% NA trials have been removed, create double matrix with interpolated NA values (rule=2 uses nearest value when nothing to interpolate between). Interpolates blinks, etc.
EasyNAapproxMatrix <- na.approx(EasyAudio, rule=2) 
EasyNAapprox <- as.data.frame(EasyNAapproxMatrix)
HardNAapproxMatrix <- na.approx(HardAudio, rule=2)
HardNAapprox <- as.data.frame(HardNAapproxMatrix)

# Data is rehaped into 'long' format. Adjust following lines of code according to how many trials are in NAapprox dataframe (i.e., how many trials remain after removal) #
EasyReshaped <- reshape(EasyNAapprox, direction="long", varying=list(names(EasyNAapprox)[2:24]), 
                        v.names="PupilSize", idvar="TrialTime", timevar="Trial", times=names(EasyNAapprox[2:24])) # reshaped data frame from wide to long format 
HardReshaped <- reshape(HardNAapprox, direction="long", varying=list(names(HardNAapprox)[2:20]), 
                        v.names="PupilSize", idvar="TrialTime", timevar="Trial", times=names(HardNAapprox[2:20])) # (change 2:46 to 2:*no. of trials*)

# Each pupil data point is scaled (subtracted from the mean and divided by sd - i.e., z-scored) #
EasyOnlyScaled <- transform(EasyReshaped, PupilScaled = ave(PupilSize, Trial, FUN = scale))
HardOnlyScaled <- transform(HardReshaped, PupilScaled = ave(PupilSize, Trial, FUN = scale))

# Adds 'baseline' column. 10000-11000 ms = 1 sec of noise-alone presentation #
EasyOnlyScaled$Baseline <- with(EasyOnlyScaled, ave(PupilScaled, Trial, FUN = function(x) mean(x[10000:11000])))
HardOnlyScaled$Baseline <- with(HardOnlyScaled, ave(PupilScaled, Trial, FUN = function(x) mean(x[10000:11000])))

# Pupil values are baseline-corrected #
EasyOnlyScaled$PupilBaseCorrect <- EasyOnlyScaled$PupilScaled - EasyOnlyScaled$Baseline
HardOnlyScaled$PupilBaseCorrect <- HardOnlyScaled$PupilScaled - HardOnlyScaled$Baseline

# Trimming down data frame to include only columns that are needed #
EasyScaled <- subset(EasyOnlyScaled, select= c(Trial, TrialTime, PupilBaseCorrect))
HardScaled <- subset(HardOnlyScaled, select= c(Trial, TrialTime, PupilBaseCorrect))

# Adds 'Second' variable which assigns each trial time to a second (1-14) #
EasyScaled$Second <- cut(EasyScaled$TrialTime,c(seq(10000,24000,500)),include.lowest=TRUE)
HardScaled$Second <- cut(HardScaled$TrialTime,c(seq(10000,24000,500)),include.lowest=TRUE)

# Subset to include data from baseline only #
EasyScaled <- subset(EasyScaled, TrialTime>9999)
HardScaled <- subset(HardScaled, TrialTime>9999)

# Calculate mean baseline-corrected pupil value for each second and each trial #
Easy <- aggregate(PupilBaseCorrect ~ Second + Trial, data = EasyScaled, mean)
Hard <- aggregate(PupilBaseCorrect ~ Second + Trial, data = HardScaled, mean)

# Merge Easy and Hard data. Adding identifying 'Condition' variable first #
Hard$Condition <- "Hard"
Easy$Condition <- "Easy"
pptMerged <- merge(Easy, Hard, all=TRUE)

# Change 'First' to 'Second' and '1st' to '2nd' below for 2nd half (trials 24-46) analysis only #
# Changes 'Second' into numeric variable marking seconds 1-28 (Note - because data is divided into 500 ms bins, 1 second = 500 ms) #
ppt45MergedFirst <- transform(pptMerged, Second = as.numeric(Second)) 
ppt45MergedFirst$Half <- "1st"

# After running each individual participant, merge all of their First versus Second half data frames #
ppt3Merged <- merge(ppt3MergedFirst, ppt3MergedSecond, all=TRUE) # Merges all dataframes
ppt11Merged <- merge(ppt11MergedFirst, ppt11MergedSecond, all=TRUE)
ppt21Merged <- merge(ppt21MergedFirst, ppt21MergedSecond, all=TRUE)
ppt27Merged <- merge(ppt27MergedFirst, ppt27MergedSecond, all=TRUE)
ppt29Merged <- merge(ppt29MergedFirst, ppt29MergedSecond, all=TRUE)
ppt31Merged <- merge(ppt31MergedFirst, ppt31MergedSecond, all=TRUE)
ppt33Merged <- merge(ppt33MergedFirst, ppt33MergedSecond, all=TRUE)
ppt35Merged <- merge(ppt35MergedFirst, ppt35MergedSecond, all=TRUE)
ppt37Merged <- merge(ppt37MergedFirst, ppt37MergedSecond, all=TRUE)
ppt41Merged <- merge(ppt41MergedFirst, ppt41MergedSecond, all=TRUE)
ppt43Merged <- merge(ppt43MergedFirst, ppt43MergedSecond, all=TRUE)
ppt45Merged <- merge(ppt45MergedFirst, ppt45MergedSecond, all=TRUE)
ppt6Merged <- merge(ppt6MergedFirst, ppt6MergedSecond, all=TRUE) # Merges all dataframes
ppt12Merged <- merge(ppt12MergedFirst, ppt12MergedSecond, all=TRUE)
ppt20Merged <- merge(ppt20MergedFirst, ppt20MergedSecond, all=TRUE)
ppt26Merged <- merge(ppt26MergedFirst, ppt26MergedSecond, all=TRUE)
ppt30Merged <- merge(ppt30MergedFirst, ppt30MergedSecond, all=TRUE)
ppt32Merged <- merge(ppt32MergedFirst, ppt32MergedSecond, all=TRUE)
ppt34Merged <- merge(ppt34MergedFirst, ppt34MergedSecond, all=TRUE)
ppt36Merged <- merge(ppt36MergedFirst, ppt36MergedSecond, all=TRUE)
ppt38Merged <- merge(ppt38MergedFirst, ppt38MergedSecond, all=TRUE)
ppt40Merged <- merge(ppt40MergedFirst, ppt40MergedSecond, all=TRUE)
ppt42Merged <- merge(ppt42MergedFirst, ppt42MergedSecond, all=TRUE)
ppt44Merged <- merge(ppt44MergedFirst, ppt44MergedSecond, all=TRUE)

# adding participant number information to each participant data frame #
ppt3Merged$Subject <- "3"
ppt6Merged$Subject <- "6"
ppt11Merged$Subject <- "11"
ppt12Merged$Subject <- "12"
ppt20Merged$Subject <- "20"
ppt21Merged$Subject <- "21"
ppt26Merged$Subject <- "26"
ppt27Merged$Subject <- "27"
ppt29Merged$Subject <- "29"
ppt30Merged$Subject <- "30"
ppt31Merged$Subject <- "31"
ppt32Merged$Subject <- "32"
ppt33Merged$Subject <- "33"
ppt34Merged$Subject <- "34"
ppt35Merged$Subject <- "35"
ppt36Merged$Subject <- "36"
ppt37Merged$Subject <- "37"
ppt38Merged$Subject <- "38"
ppt40Merged$Subject <- "40"
ppt41Merged$Subject <- "41"
ppt42Merged$Subject <- "42"
ppt43Merged$Subject <- "43"
ppt44Merged$Subject <- "44"
ppt45Merged$Subject <- "45"

# Create one data frame that includes all merged participants #
BlockStudyMerged <- merge(ppt3Merged, ppt6Merged, all=TRUE) # Merges all dataframes
BlockStudyMerged <- merge(BlockStudyMerged, ppt11Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt12Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt20Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt21Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt26Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt27Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt29Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt30Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt31Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt32Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt33Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt34Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt35Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt36Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt37Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt38Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt40Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt41Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt42Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt43Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt44Merged, all=TRUE)
BlockStudyMerged <- merge(BlockStudyMerged, ppt45Merged, all=TRUE)