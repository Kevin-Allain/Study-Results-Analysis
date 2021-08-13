# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.cyclismo.org/tutorial/R/confidence.html#id1 and https://www.cyclismo.org/tutorial/R/pValues.html#t-test and https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/ and http://www.mayin.org/ajayshah/KB/R/documents/boot.html and https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals#14069837

# File:    Confidence_Interval.r
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-17

library(boot) 
library(ggplot2) 
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
d <- read.table(file="data/transformed/survey_precise-study_1628871568368.csv", TRUE, ",")




# Confidence interval: average +- z score * standard error #### Issue here: our data as it is now doesn't have 
# 95% confidence interval means 2.5% on each side
#todo: add usage of bootstrap instead of d
mean(d$t)
# dim(d$t) # NULL?
dim(d)

# d$t
#meanForColumn <- function(data,columnName){
  #df <- data$columnName
  #df
  #colOther <- df["ComplexityGroup"]
  #colOther
  #col <- df[columnName]
  #col
  #c(mean(col))
#}

samplemean <- function(x, d) {
  return(mean(x[d]))
}
bootDuration_in_seconds = boot(d$t, samplemean, R=1000) # 1000 replications
plot(bootDuration_in_seconds)


bootCorrect_B = boot(d$correctB, samplemean, R=1000) # 1000 replications
plot(bootCorrect_B)
# Seems okay up to here


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


# Display of confidence intervals based on the focus, with the y axis displaying the time taken to answer. Currently using randomly generated data
dfCategories <- data.frame(x= c("WHAT_Qn","WHAT_Ql","WHERE") ,
                           F = meansCategories, L = c(timeCI_WHAT_Qn$normal[2], timeCI_WHAT_Ql$normal[2], timeCI_WHERE$normal[2]),
                           U = c(timeCI_WHAT_Qn$normal[3], timeCI_WHAT_Ql$normal[3], timeCI_WHERE$normal[3])
                           )

# ?geom_errorbar
# dfCategories doesn't have all the categories of d
# ggplot(dfCategories, aes(x = x, y = F)) + 
#   geom_point(size = 4) + 
#   geom_errorbar(aes(ymax = U, ymin = L) + 
#   facet_wrap(dMask)
#   )


# # AverageCorrect-global Function to find the bootstrap Confidence Intervals 
# timeCI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca")) 
# lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
# vecTimeCI <- c(lowerTimeCI, higherTimeCI)
# 
# #Buggy, don't remember why. Disregard.
# bootDuration_in_secondsEEE = boot(d$t[d$ComplexityGroup=="EEE"], samplemean, R=1000) # 1000 replications
# bootDuration_in_secondsMMM = boot(d$t[d$ComplexityGroup=="MMM"], samplemean, R=1000) # 1000 replications
# bootDuration_in_secondsHHH = boot(d$t[d$ComplexityGroup=="HHH"], samplemean, R=1000) # 1000 replications
# timeCIEEE <- boot.ci(boot.out = bootDuration_in_secondsEEE, type = c("norm", "basic", "perc", "bca"))
# timeCIMMM <- boot.ci(boot.out = bootDuration_in_secondsMMM, type = c("norm", "basic", "perc", "bca"))
# timeCIHHH <- boot.ci(boot.out = bootDuration_in_secondsHHH, type = c("norm", "basic", "perc", "bca"))
# indexesV <- 1:length(c(timeCIEEE$normal[2],timeCIEEE$normal[3]))
# indexesCategories <- 1:3
# # Is that the right means?
# meansCategories <- c(mean(d$t[d$ComplexityGroup=="EEE"]), mean(d$t[d$ComplexityGroup=="MMM"]), mean(d$t[d$ComplexityGroup=="HHH"]))
# 
# dfCategories <- data.frame(x= c("EEE","MMM","HHH") ,
#                            F = meansCategories, L = c(timeCIEEE$normal[2], timeCIMMM$normal[2], timeCIHHH$normal[2]),
#                            U = c(timeCIEEE$normal[3], timeCIMMM$normal[3], timeCIHHH$normal[3])
# )
# ggplot(dfCategories, aes(x = x, y = F)) + geom_point(size = 4) + geom_errorbar(aes(ymax = U, ymin = L))
 


