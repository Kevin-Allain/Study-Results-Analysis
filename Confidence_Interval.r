# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.cyclismo.org/tutorial/R/confidence.html#id1 and https://www.cyclismo.org/tutorial/R/pValues.html#t-test and https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/ and http://www.mayin.org/ajayshah/KB/R/documents/boot.html and https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals#14069837

# File:    Confidence_Interval.r
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-17

library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)

# load file # Remember to move in the right folder, based on your own computer
setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
# d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")
# d <- read.table(file="data/transformed/survey_precise-study_1618325511833.csv", TRUE, ",")
# Example of file with randomly generated data to fill all the categories
# d <- read.table(file="data/transformed/survey_precise-study_randomlyfilled_1618325521753.csv", TRUE, ",")
# Added a new data file with neither randomly generated data nor empty lines
# d <- read.table(file="data/transformed/survey_precise-study_1618754171875.csv", TRUE, ",")

# Example with only 3 answers generated with a slider # No more answerA and correctA. Now it is answerA1,answerA2,answerA3, diffA1,diffA2,diffA3
# d <- read.table(file="data/transformed/survey_precise-study_1620402561191.csv", TRUE, ",")
# survey_precise-study_1628072432964
d <- read.table(file="data/transformed/survey_precise-study_1628960451341.csv", TRUE, ",")

d_alt <- read.table(file="data/transformed/alt_survey_precise-study_1628960451341.csv", TRUE, ",")


# Confidence interval: average +- z score * standard error #### Issue here: our data as it is now doesn't have 
# 95% confidence interval means 2.5% on each side
#todo: add usage of bootstrap instead of d
mean(d$t)
# dim(d$t) # NULL?
dim(d)

samplemean <- function(x, d) {
  return(mean(x[d]))
}
bootDuration_in_seconds = boot(d$t, samplemean, R=1000) # 1000 replications
plot(bootDuration_in_seconds)

boot_diffA1 = boot(d$diffA1, samplemean, R=1000) # 1000 replications

bootCorrect_B = boot(d$correctB, samplemean, R=1000) # 1000 replications
plot(bootCorrect_B)
# Seems okay up to here

arrMaskDiffs = array(c("easy","medium","hard"))

# Storing all the boots
boot_d_focus_what_qn_DiffA1_mask_easy = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_mask_medium = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_mask_hard = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_mask_easy = boot(d$diffA2[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_mask_easy = boot(d$diffA3[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)






# TIME-global Function to find the bootstrap Confidence Intervals 
timeCI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca")) 
lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
vecTimeCI <- c(lowerTimeCI, higherTimeCI)

bootDuration_in_seconds_WHAT_Qn = boot(d$t[d$focus=="WHAT_Qn"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHAT_Ql = boot(d$t[d$focus=="WHAT_Ql"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHERE = boot(d$t[d$focus=="WHERE"], samplemean, R=1000) # 1000 replications
timeCI_WHAT_Qn <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Qn, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHAT_Ql <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Ql, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHERE <- boot.ci(boot.out = bootDuration_in_seconds_WHERE, type = c("norm", "basic", "perc", "bca"))

indexesV <- 1:length(c(timeCI_WHAT_Qn$normal[2],timeCI_WHAT_Qn$normal[3]))
indexesCategories <- 1:3
meansCategories <- c(mean(d$t[d$focus=="WHAT_Qn"]), mean(d$t[d$focus=="WHAT_Ql"]), mean(d$t[d$focus=="WHERE"]))

timeCI_WHAT_Qn$normal[2]

# Display of confidence intervals based on the focus, with the y axis displaying the time taken to answer. Currently using randomly generated data
dfCategories <- data.frame(x= c("WHAT_Qn","WHAT_Ql","WHERE") ,
                           F = meansCategories, 
                           L = c(timeCI_WHAT_Qn$normal[2], timeCI_WHAT_Ql$normal[2], timeCI_WHERE$normal[2]),
                           U = c(timeCI_WHAT_Qn$normal[3], timeCI_WHAT_Ql$normal[3], timeCI_WHERE$normal[3])
                           )

# ?geom_errorbar
# dfCategories doesn't have all the categories of d
ggplot(dfCategories, aes(x = F, y = x)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmax = U, xmin = L) #+ facet_wrap(~dMask)
  )



d$orderFocusAllComplex <- factor( d$info_focus_dComplex_dMask,c("WHAT_Qn_E_E","WHAT_Qn_E_M","WHAT_Qn_E_H","WHAT_Qn_M_E","WHAT_Qn_M_M","WHAT_Qn_M_H","WHAT_Qn_H_E","WHAT_Qn_H_M","WHAT_Qn_H_H", "WHAT_Ql_E_E","WHAT_Ql_E_M","WHAT_Ql_E_H","WHAT_Ql_M_E","WHAT_Ql_M_M","WHAT_Ql_M_H","WHAT_Ql_H_E","WHAT_Ql_H_M","WHAT_Ql_H_H", "WHERE_E_E","WHERE_E_M","WHERE_E_H","WHERE_M_E","WHERE_M_M","WHERE_M_H","WHERE_H_E","WHERE_H_M","WHERE_H_H"))
d$orderAllComplex <- factor(d$info_dComplex_dMask,c("E_E","E_M","E_H","M_E","M_M","M_H","H_E","H_M","H_H") )
# Test for display with facetting
ggplot(d, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_wrap(~orderAllComplex,ncol=1)


# Test for display with facetting
d$orderFocusComplex <- factor(d$dComplex_focus,c("E","M","H"))
d$orderMaskComplex <- factor(d$dMask,c("easy","medium","hard"))
diffA1CI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca"))
lowerdiffA1CI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
ggplot(d, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_grid(orderMaskComplex ~ orderFocusComplex)



# ---- Error bars for questions A1, A2, A3 and trust levels

groupedData <- d_alt %>%
  group_by(focus,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
            ) 
# %>%
#   mutate(percCorrectB = count/sum(count))


groupedData$dMask = as.character(groupedData$dMask)
groupedData$dMask[groupedData$dMask == "easy"] = "Mask Easy"
groupedData$dMask[groupedData$dMask == "medium"] = "Mask Medium"
groupedData$dMask[groupedData$dMask == "hard"] = "Mask Hard"
groupedData$dComplex_focus = as.character(groupedData$dComplex_focus)
groupedData$dComplex_focus[groupedData$dComplex_focus == "E"] = "Focus Easy"
groupedData$dComplex_focus[groupedData$dComplex_focus == "M"] = "Focus Medium"
groupedData$dComplex_focus[groupedData$dComplex_focus == "H"] = "Focus Hard"

# groupedData$orderFocusComplex <- factor(groupedData$dComplex_focus,c("E","M","H"))
# groupedData$orderMaskComplex <- factor(groupedData$dMask,c("easy","medium","hard"))
groupedData$orderFocusComplex <- factor(groupedData$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
groupedData$orderMaskComplex <- factor(groupedData$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

groupedPlotDiffA1 <- ggplot(groupedData, aes(x=mean_diffA1,y=focus)) +
  geom_errorbar(aes(xmin=mean_diffA1-se_diffA1, xmax=mean_diffA1+se_diffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

# d_alt[d_alt$dComplex_focus=="E" & d_alt$dMask=="easy"]
# View(groupedData)
groupedPlotDiffA1

# ---- Stacked bar charts for question B and trust levels

d_alt$dMask = as.character(d_alt$dMask)
d_alt$dMask[d_alt$dMask == "easy"] = "Mask Easy"
d_alt$dMask[d_alt$dMask == "medium"] = "Mask Medium"
d_alt$dMask[d_alt$dMask == "hard"] = "Mask Hard"
d_alt$dComplex_focus = as.character(d_alt$dComplex_focus)
d_alt$dComplex_focus[d_alt$dComplex_focus == "E"] = "Focus Easy"
d_alt$dComplex_focus[d_alt$dComplex_focus == "M"] = "Focus Medium"
d_alt$dComplex_focus[d_alt$dComplex_focus == "H"] = "Focus Hard"

d_alt$orderFocusComplex <- factor(d_alt$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
d_alt$orderMaskComplex <- factor(d_alt$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
groupedPlotCorrectB <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(correctB)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotCorrectB

