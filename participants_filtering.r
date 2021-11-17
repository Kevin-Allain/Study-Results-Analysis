# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
library(cowplot)
library(patchwork)
library(stringr)

setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
source("functions_dataStudies.r") # beware if there are function calls within the file, during tests.

# data from participants who passed introduction questions
# file for fluent aggregated complete_measurement_aggregate-measurement-distractor-scaling1_2021_09_18_October_22_headerAdapted
d_alt <- read.table(file="data/transformed/survey_complete_measurement_all_2021_09_18_headerAdapted.csv",TRUE, ",")
d_distr_all <-  read.table(file="data/transformed/survey_complete_distractor_all_2021_09_18_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_sclAll <- read.table(file="data/transformed/survey_complete_scaling_all_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
# dataAnswers <- bind_rows(d_alt,d_distr_all,d_sclAll) # error with a trustA1 being a character!

# ====== data from first language
d_aggregate_firstLanguage <-  read.table(file="data/transformed/complete_measurement_aggregate-measurement-distractor-scaling1_2021_09_18_October_22_headerAdapted.csv",TRUE, ",")
dim(d_aggregate_firstLanguage)
aggregate_sep <- filterAccordingToMonth(d_aggregate_firstLanguage,"09")
aggregate_oct <- filterAccordingToMonth(d_aggregate_firstLanguage,"10")
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & d_aggregate_firstLanguage$Q11!="Down"]) # count number of errors for test question about direction of trajectory
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & d_aggregate_firstLanguage$Q12!="From time 10 to 30 and time 60 to 70"]) # count number of errors for test to assess quality
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & d_aggregate_firstLanguage$Q13!="Time 30"]) # count number of errors to identify time with minimum value
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & d_aggregate_firstLanguage$Q17!="On the first long straight section"]) # count number of errors to identify position with minimum value
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & d_aggregate_firstLanguage$Q14!="From a little before time 70 up to a little before time 80"]) # count number of errors to identify masks times 
length(d_aggregate_firstLanguage$Status[d_aggregate_firstLanguage$Finished=="True" & (d_aggregate_firstLanguage$Q11!="Down" | d_aggregate_firstLanguage$Q12!="From time 10 to 30 and time 60 to 70" | d_aggregate_firstLanguage$Q13!="Time 30" | d_aggregate_firstLanguage$Q17!="On the first long straight section" | d_aggregate_firstLanguage$Q14!="From a little before time 70 up to a little before time 80")])
length(d_aggregate_firstLanguage$RecordedDate[d_aggregate_firstLanguage$Finished=="True"])

length(aggregate_sep$RecordedDate[aggregate_sep$Finished=="True"])
length(aggregate_sep$Status[aggregate_sep$Finished=="True" & (aggregate_sep$Q11!="Down" | aggregate_sep$Q12!="From time 10 to 30 and time 60 to 70" | aggregate_sep$Q13!="Time 30" | aggregate_sep$Q17!="On the first long straight section" | aggregate_sep$Q14!="From a little before time 70 up to a little before time 80")])
length(aggregate_oct$RecordedDate[aggregate_oct$Finished=="True"])
length(aggregate_oct$Status[aggregate_oct$Finished=="True" & (aggregate_oct$Q11!="Down" | aggregate_oct$Q12!="From time 10 to 30 and time 60 to 70" | aggregate_oct$Q13!="Time 30" | aggregate_oct$Q17!="On the first long straight section" | aggregate_oct$Q14!="From a little before time 70 up to a little before time 80")])

# ======

# ====== data from noTikTok measurement study.
d_aggregate_noTikTok_firstLanguage <- read.table(file="Studies_2021_11_12/Results/measurement_aggregate-measurement-distractor-scaling1_headerAdapted.csv",TRUE, ",")
dim(d_aggregate_noTikTok_firstLanguage)
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Q11!="Down"]) # count number of errors for test question about direction of trajectory
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Q12!="From time 10 to 30 and time 60 to 70"]) # count number of errors for test to assess quality
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Q13!="Time 30"]) # count number of errors to identify time with minimum value
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Q17!="On the first long straight section"]) # count number of errors to identify position with minimum value
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Q14!="From a little before time 70 up to a little before time 80"]) # count number of errors to identify masks times 
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Finished=="True" & (d_aggregate_noTikTok_firstLanguage$Q11!="Down" | d_aggregate_noTikTok_firstLanguage$Q12!="From time 10 to 30 and time 60 to 70" | d_aggregate_noTikTok_firstLanguage$Q13!="Time 30" | d_aggregate_noTikTok_firstLanguage$Q17!="On the first long straight section" | d_aggregate_noTikTok_firstLanguage$Q14!="From a little before time 70 up to a little before time 80")])
length(d_aggregate_noTikTok_firstLanguage$ResponseId[d_aggregate_noTikTok_firstLanguage$Finished=="True"])
# ======

# ---- Load studies untransformed
full_distractor_n_headerAdapted_2021_11_03 <- read.table(file="data/participants_answers_headerAdapted_untransformed/full_distractor_n_headerAdapted.csv",TRUE,",")
full_distractor_h_headerAdapted_2021_11_03 <- read.table(file="data/participants_answers_headerAdapted_untransformed/full_distractor_h_headerAdapted.csv",TRUE,",")
full_scaling_0_headerAdapted_2021_11_03 <- read.table(file="data/participants_answers_headerAdapted_untransformed/full_scaling_0_headerAdapted.csv",TRUE,",")
full_scaling_1_headerAdapted_2021_11_03 <- read.table(file="data/participants_answers_headerAdapted_untransformed/full_scaling_1_headerAdapted.csv",TRUE,",")
full_scaling_2_headerAdapted_2021_11_03 <- read.table(file="data/participants_answers_headerAdapted_untransformed/full_scaling_2_headerAdapted.csv",TRUE,",")
complete_measurement_nf_2021_09_18_October_22_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_measurement_nf_2021_09_18_October_22_headerAdapted.csv",TRUE,",")
complete_measurement_f_2021_09_18_October_22_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_measurement_f_2021_09_18_October_22_headerAdapted.csv",TRUE,",")
complete_distractor_n_2021_09_18_headerAdapted_MMM_replaced <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_distractor_n_2021_09_18_headerAdapted_MMM_replaced.csv",TRUE,",")
complete_distractor_n_2021_09_18_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_distractor_n_2021_09_18_headerAdapted.csv",TRUE,",")
complete_distractor_h_2021_09_18_headerAdapted_MMM_replaced <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_distractor_h_2021_09_18_headerAdapted_MMM_replaced.csv",TRUE,",")
complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
complete_measurement_f_2021_09_18_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_measurement_f_2021_09_18_headerAdapted.csv",TRUE,",")
complete_measurement_nf_2021_09_18_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_measurement_nf_2021_09_18_headerAdapted.csv",TRUE,",")
complete_scaling_2_2021_09_19_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_2_2021_09_19_headerAdapted.csv",TRUE,",")
complete_scaling_1_2021_09_19_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_1_2021_09_19_headerAdapted.csv",TRUE,",")
complete_scaling_0_2021_09_19_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_scaling_0_2021_09_19_headerAdapted.csv",TRUE,",")
complete_distractor_h_2021_09_18_headerAdapted <- read.table(file="data/participants_answers_headerAdapted_untransformed/complete_distractor_h_2021_09_18_headerAdapted.csv",TRUE,",")

# Some datasets have to be filtered by month... Use the function filterAccordingToMonth
mergeComplete <- merge(complete_measurement_nf_2021_09_18_October_22_headerAdapted, complete_measurement_f_2021_09_18_October_22_headerAdapted,all=TRUE)
complete_sep <- filterAccordingToMonth(mergeComplete,"09")
complete_oct <- filterAccordingToMonth(mergeComplete,"10")

length(complete_sep$ResponseId[complete_sep$Finished=="True"])
length(complete_sep$ResponseId[complete_sep$Finished=="True" & (complete_sep$Q11!="Down" | complete_sep$Q12!="From time 10 to 30 and time 60 to 70" | complete_sep$Q13!="Time 30" | complete_sep$Q17!="On the first long straight section" | complete_sep$Q14!="From a little before time 70 up to a little before time 80")])
length(complete_oct$ResponseId[complete_oct$Finished=="True"])
length(complete_oct$ResponseId[complete_oct$Finished=="True" & (complete_oct$Q11!="Down" | complete_oct$Q12!="From time 10 to 30 and time 60 to 70" | complete_oct$Q13!="Time 30" | complete_oct$Q17!="On the first long straight section" | complete_oct$Q14!="From a little before time 70 up to a little before time 80")])


# how do we merge them? ... by hand
jointUntransformed <- merge(full_distractor_n_headerAdapted_2021_11_03, full_distractor_h_headerAdapted_2021_11_03,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,full_scaling_0_headerAdapted_2021_11_03,all=TRUE)
jointUntransformed <- merge(jointUntransformed,full_scaling_1_headerAdapted_2021_11_03,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,full_scaling_2_headerAdapted_2021_11_03,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_measurement_nf_2021_09_18_October_22_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_measurement_f_2021_09_18_October_22_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_distractor_n_2021_09_18_headerAdapted_MMM_replaced,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_distractor_n_2021_09_18_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_distractor_h_2021_09_18_headerAdapted_MMM_replaced,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_measurement_f_2021_09_18_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_measurement_nf_2021_09_18_headerAdapted,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,complete_scaling_2_2021_09_19_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_scaling_1_2021_09_19_headerAdapted,all=TRUE)
# jointUntransformed <- merge(jointUntransformed,complete_scaling_0_2021_09_19_headerAdapted,all=TRUE)
jointUntransformed <- merge(jointUntransformed,complete_distractor_h_2021_09_18_headerAdapted,all=TRUE)

dim(full_distractor_n_headerAdapted_2021_11_03);
dim(full_distractor_h_headerAdapted_2021_11_03);
dim(full_scaling_0_headerAdapted_2021_11_03)
dim(jointUntransformed);
# jointUntransformed <- merge(jointUntransformed, characterizations,by=c("flips","idc","focus","cntrQ"),all=TRUE)
length(jointUntransformed$Status[jointUntransformed$Q11!="Down"]) # count number of errors for test question about direction of trajectory
length(jointUntransformed$Status[jointUntransformed$Q12!="From time 10 to 30 and time 60 to 70"]) # count number of errors for test to assess quality
length(jointUntransformed$Status[jointUntransformed$Q13!="Time 30"]) # count number of errors to identify time with minimum value
length(jointUntransformed$Status[jointUntransformed$Q17!="On the first long straight section"]) # count number of errors to identify position with minimum value
length(jointUntransformed$Status[jointUntransformed$Q14!="From a little before time 70 up to a little before time 80"]) # count number of errors to identify masks times 

# ---- ResponsesId who passed the first test
length(unique( jointUntransformed$ResponseId[jointUntransformed$Finished=="True" & ( jointUntransformed$Q11!="Down" | jointUntransformed$Q12!="From time 10 to 30 and time 60 to 70" | jointUntransformed$Q13!="Time 30" | jointUntransformed$Q17!="On the first long straight section" | jointUntransformed$Q14!="From a little before time 70 up to a little before time 80") ] ) )

length(unique ( jointUntransformed$ResponseId[jointUntransformed$Finished=="True"] ) )

joint_sep <- filterAccordingToMonth(jointUntransformed,"09")
length (unique (joint_sep$ResponseId[joint_sep$Finished=="True"] ) )
length( unique ( joint_sep$ResponseId[ joint_sep$Finished=="True" & ( joint_sep$Q11!="Down" | joint_sep$Q12!="From time 10 to 30 and time 60 to 70" | joint_sep$Q13!="Time 30" | joint_sep$Q17!="On the first long straight section" | joint_sep$Q14!="From a little before time 70 up to a little before time 80" ) ] ) )
joint_oct <- filterAccordingToMonth(jointUntransformed,"10")
length (unique (joint_oct$ResponseId[joint_sep$Finished=="True"] ) )
length( unique ( joint_oct$ResponseId[ joint_oct$Finished=="True" & ( joint_oct$Q11!="Down" | joint_oct$Q12!="From time 10 to 30 and time 60 to 70" | joint_oct$Q13!="Time 30" | joint_oct$Q17!="On the first long straight section" | joint_oct$Q14!="From a little before time 70 up to a little before time 80" ) ] ) )
# ----

# d_participants <- read.table(file="prolific_exports/2021_11_03/prolific_export_aggregated_2021_11_03.csv",TRUE,",")
arrProlificFilesNames <- strsplit(paste("prolific_exports/2021_11_03/",list.files(path="prolific_exports/2021_11_03"),sep="" ),split=" ")
arrProlificFilesNames
fileTrial1 <- read.table(file=toString(arrProlificFilesNames[1]),header=TRUE,fill=TRUE,sep = ",")
fileTrial1
# arrProlificFilesNames <- strsplit(paste("prolific_exports/",list.files(path="prolific_exports"),sep="" ),split=" ")
class(toString(arrProlificFilesNames[1]))
manualSum <- 0;
manualSumUniqueness <- 0
wholeProlificId <- c();
manualProlificInfo <- data.frame()
for (i in 1:length(arrProlificFilesNames)){
  fileProlific <- read.table(file=toString(arrProlificFilesNames[i]),header=TRUE,fill=TRUE,sep = ",")
  cat("\nthis prolific file number of participants is: ",length(fileProlific$participant_id))
  manualSum <- manualSum + length(fileProlific$participant_id)
  manualSumUniqueness <- manualSumUniqueness + length(unique(fileProlific$participant_id))
  wholeProlificId <- c(wholeProlificId, fileProlific$participant_id)
  manualProlificInfo <- bind_rows(manualProlificInfo,fileProlific)
  numMale <- length(fileProlific$Sex[fileProlific$Sex=="Male"])
  numFemale <- length(fileProlific$Sex[fileProlific$Sex=="Female"])
  # cat("\nwith ",numMale," males and ",numFemale," females");
}
View(manualProlificInfo)
dim(manualProlificInfo)
manualSum
manualSumUniqueness
length(unique(wholeProlificId))
length(unique(manualProlificInfo$participant_id[manualProlificInfo$status=="APPROVED"]))
dim(manualProlificInfo)
dim(manualProlificInfo[manualProlificInfo$First.Language=="English",])
dim(manualProlificInfo[manualProlificInfo$First.Language=="French",])
dim(manualProlificInfo[manualProlificInfo$First.Language=="Spanish",])
dim(manualProlificInfo[manualProlificInfo$First.Language=="Chinese",])
dim(manualProlificInfo[manualProlificInfo$First.Language=="Portuguese",])

listFiles_participant_prolific <- paste("prolific_exports/",list.files(path="prolific_exports"),sep="" )
prolific_exports <- do.call(rbind, lapply(listFiles, function(x) read.table(file=x,header=TRUE,fill=TRUE,sep = ",")))
length(prolific_exports$participant_id)
length(unique(prolific_exports$participant_id))

listFilesAnswersUnaltered <- c("data/participants_answers_headerAdapted_untransformed/complete_measurement_nf_2021_09_18_headerAdapted.csv", "data/participants_answers_headerAdapted_untransformed/complete_measurement_f_2021_09_18_headerAdapted.csv", "data/participants_answers_headerAdapted_untransformed/complete_distractor_h_2021_09_18_headerAdapted_MMM_replaced.csv", "data/participants_answers_headerAdapted_untransformed/complete_distractor_n_2021_09_18_headerAdapted_MMM_replaced.csv", "data/participants_answers_headerAdapted_untransformed/complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced.csv", "data/participants_answers_headerAdapted_untransformed/complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced.csv",  "data/participants_answers_headerAdapted_untransformed/complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced.csv")
fileUnalterated_1 <- read.table(file=listFilesAnswersUnaltered[1],TRUE,",");fileUnalterated_2 <- read.table(file=listFilesAnswersUnaltered[2],TRUE,",");fileUnalterated_3 <- read.table(file=listFilesAnswersUnaltered[3],TRUE,",");fileUnalterated_4 <- read.table(file=listFilesAnswersUnaltered[4],TRUE,",");fileUnalterated_5 <- read.table(file=listFilesAnswersUnaltered[5],TRUE,",");fileUnalterated_6 <- read.table(file=listFilesAnswersUnaltered[6],TRUE,",");fileUnalterated_7 <- read.table(file=listFilesAnswersUnaltered[7],TRUE,",")
colsOfInterest <- colnames(fileUnalterated_1)[1:24]
reducedFileIntro_1 <- fileUnalterated_1 %>% select(colsOfInterest);reducedFileIntro_2 <- fileUnalterated_2 %>% select(colsOfInterest);reducedFileIntro_3 <- fileUnalterated_3 %>% select(colsOfInterest);reducedFileIntro_4 <- fileUnalterated_4 %>% select(colsOfInterest);reducedFileIntro_5 <- fileUnalterated_5 %>% select(colsOfInterest);reducedFileIntro_6 <- fileUnalterated_6 %>% select(colsOfInterest);reducedFileIntro_7 <- fileUnalterated_7 %>% select(colsOfInterest)
bindFiles <- data.frame()
bindFiles<- bind_rows(reducedFileIntro_1,reducedFileIntro_2,reducedFileIntro_3,reducedFileIntro_4,reducedFileIntro_5,reducedFileIntro_6,reducedFileIntro_7)
length(unique(bindFiles$participant_id[bindFiles$Progress==100]))
bindFiles$Q15
names(bindFiles)[names(bindFiles)=="Q15"] <- "participant_id"
length(unique(prolific_exports$participant_id))
bindFiles$participant_id
bindFiles_merged <- merge(bindFiles,prolific_exports)
bindFiles_merged_manuals <- merge(bindFiles,manualProlificInfo)

length(unique(d_alt$ResponseId))
length(unique(d_sclAll$ResponseId))
length(unique(d_distr_all$ResponseId))
cat("\ntotal unique responses that passed the introduction: ", (length(unique(d_alt$ResponseId)) + length(unique(d_sclAll$ResponseId)) + length(unique(d_distr_all$ResponseId)) ))
length(unique( filter_filter_someTrust0or5_impossibleQualAnswer(d_alt)$ResponseId) )
length(unique(bindFiles_merged$ResponseId))
# manualProlificInfo$participant_id
cat("\nnumber of unique participants with automated merge: ",length(unique(bindFiles_merged$participant_id)),", and manual: ",length(unique(bindFiles_merged_manuals$participant_id)))
cat("\nnumber of unique responses with automated merge: ",length(unique(bindFiles_merged$ResponseId)),", and manual: ",length(unique(bindFiles_merged_manuals$ResponseId)))
# bindFiles_merged$ResponseId[1]
# bindFiles_merged[4,]$ResponseId
# dim(d_alt[d_alt$ResponseId=="R_0Oi839NoZ9fKwCt",])
# dim(d_alt[d_alt$ResponseId == bindFiles_merged$ResponseId[1],])
# dim(d_sclAll[d_sclAll$ResponseId == bindFiles_merged$ResponseId[1],])[1]
# d_distr_all[d_distr_all$ResponseId == bindFiles_merged$ResponseId[1],]
testFilter <- filter_filter_someTrust0or5_impossibleQualAnswer(d_alt)

arrParticipantsIdPass <- c()
arrFirstLanguagePass <- c()
notMatchingFirstLanguage <- c()
length(bindFiles_merged_manuals$ResponseId)
for(i in 1:length(bindFiles_merged_manuals$ResponseId)){
  curBind <-bindFiles_merged_manuals[i,]
  curResponseId <- curBind$ResponseId
  matchResponseDB <- NULL;
  if ( dim(d_sclAll[d_sclAll$ResponseId == curResponseId,])[1] > 0 ){
    matchResponseDB <- d_sclAll
  } else if ( dim(d_distr_all[d_distr_all$ResponseId == curResponseId,])[1] > 0 ){
    matchResponseDB <- d_distr_all
  } else if ( dim(d_alt[d_alt$ResponseId == curResponseId,])[1] > 0 ){
    matchResponseDB <- d_alt
  } else {
    # cat("\nNo match. Is it an issue? Not sure what to do with that.\n")
    notMatchingFirstLanguage <- c(notMatchingFirstLanguage, curBind$First.Language)
  }
  if (!is.null(matchResponseDB)){
    curParticipantId <- curBind$participant_id
    participantAnswersDataFrame <- matchResponseDB[matchResponseDB$ResponseId==curResponseId,]
    # filteredResponsesFromParticipant...
    # filter_filter_someTrust0or5_impossibleQualAnswer
    filteredAnswersParticipant <- filter_filter_someTrust0or5_impossibleQualAnswer(participantAnswersDataFrame)
    # cat("\ndim of filtered: ",dim(filteredAnswersParticipant))
    if (dim(filteredAnswersParticipant)[1]>0){
      arrParticipantsIdPass <- c(arrParticipantsIdPass, curParticipantId)
      arrFirstLanguagePass <- c(arrFirstLanguagePass, curBind$First.Language)
    }
  }
}
cat("\nParticipants in total: ",length(bindFiles_merged_manuals$participant_id),", and the filtered ones: ", length(arrParticipantsIdPass))
# arrayInd("English",arrFirstLanguagePass)
manualEnglishCount_Matching <- 0
manualEnglishCount_NotMatching <- 0
for (language in arrFirstLanguagePass){
  if (language == "English"){manualEnglishCount_Matching<- manualEnglishCount_Matching + 1 }
}
for (language in notMatchingFirstLanguage){
  if (language == "English"){manualEnglishCount_NotMatching<- manualEnglishCount_NotMatching + 1 }
}
cat("\nmanualEnglishCount_Matching: ",manualEnglishCount_Matching,", manualEnglishCount_NotMatching: ",manualEnglishCount_NotMatching,"\n")

# ####Unreliable
length(which(arrFirstLanguagePass %in% "English"))
length(arrParticipantsIdPass)
length(arrFirstLanguagePass)
length(which(arrFirstLanguagePass %in% "Spanish"))

notMatchingFirstLanguage
length(notMatchingFirstLanguage)
length(notMatchingFirstLanguage %in% "English")
