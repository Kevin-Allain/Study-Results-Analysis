# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.cyclismo.org/tutorial/R/confidence.html#id1 and https://www.cyclismo.org/tutorial/R/pValues.html#t-test and https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/ and http://www.mayin.org/ajayshah/KB/R/documents/boot.html and https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals#14069837

# File:    Confidence_Interval.r
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-17

# Import library for bootstrap methods 
library(boot) 

# Import library for plotting 
library(ggplot2) 
library(lattice)



# load file # Remember to move in the right folder, based on your own computer
setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
# d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")
# d <- read.table(file="data/transformed/survey_precise-study_1618325511833.csv", TRUE, ",")
# Example of file with randomly generated data to fill all the categories
d <- read.table(file="data/transformed/survey_precise-study_randomlyfilled_1618325521753.csv", TRUE, ",")

# The header of the file:
# | ResponseId | Progress | RecordedDate | StartDate | EndDate | Finished | Duration_in_seconds | filename | idc | drawnQn | drawnQl | queryString | flips | nMasks | dMask | dComplex_Qn | dComplex_Ql | dComplex_Where | focus | bslnA | bslnB | cntrQ | FirstClick | LastClick | PageSubmit | QuestionA | correctA | TrustA | QuestionB | TrustB | correctB |


### Bootstrapping for testing hypotheses
set.seed(112358)
n <- length(d$ResponseId) # The number of observations to sample
n.Qn_E <- length(d$ResponseId[d$dComplex_Qn=="E"]) # Number of samples with data complexity Qn as E
n.Qn_M <- length(d$ResponseId[d$dComplex_Qn=="M"]) # Number of samples with data complexity Qn as M
n.Qn_H <- length(d$ResponseId[d$dComplex_Qn=="H"]) # Number of samples with data complexity Qn as H
# ... todo the other categories?
n.focus_WHAT_Qn <- length(d$ResponseId[d$focus=="WHAT_Qn"]) # Number of samples with the focus being WHAT_Qn
n.focus_WHAT_Ql <- length(d$ResponseId[d$focus=="WHAT_Ql"]) # Number of samples with the focus being WHAT_Ql
n.focus_WHERE <- length(d$ResponseId[d$focus=="WHERE"]) # Number of samples with the focus being WHERE
B <- 10000 # The number of bootstraps samples
variable <- d$PageSubmit

BootstrapSamples <- matrix( sample(variable, size=n*B, replace=TRUE), nrow=n, ncol=B)
dimBoot <- dim(BootstrapSamples)

# now, get those bootstrap samples (without loops!) ### IMPORTANT PART 1
# stick each Boot-sample in a column... # Let's make a set of groups of time selection depending on the focus of the question
Boot.focus_WHAT_Qn <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Qn"], size= B*n.focus_WHAT_Qn, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Qn)
Boot.focus_WHAT_Ql <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Ql"], size= B*n.focus_WHAT_Ql, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Ql)
Boot.focus_WHERE <- matrix( sample(d$PageSubmit[d$focus=="WHERE"], size= B*n.focus_WHERE, replace=TRUE), ncol=B, nrow=n.focus_WHERE)


Boot.t_focus_WHAT_Qn <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Qn"], size= B*n.focus_WHAT_Qn, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Qn)
Boot.t_focus_WHAT_Ql <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Ql"], size= B*n.focus_WHAT_Ql, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Ql)
Boot.t_focus_WHERE <- matrix( sample(d$PageSubmit[d$focus=="WHERE"] , size= B*n.focus_WHERE, replace=TRUE), ncol=B, nrow=n.focus_WHERE)

# Boot.EEE[1:5,1:5]; Boot.WHERE[1:5,1:5]; # I don't remember why I did this

# Code okay up to that point

# Confidence interval: average +- z score * standard error #### Issue here: our data as it is now doesn't have 
# 95% confidence interval means 2.5% on each side
#todo: add usage of bootstrap instead of d
mean(d$PageSubmit)
# dim(d$PageSubmit) # NULL?
dim(d)

# d$PageSubmit
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
bootDuration_in_seconds = boot(d$PageSubmit, samplemean, R=1000) # 1000 replications
plot(bootDuration_in_seconds)

# Seems ok up to here

bootCorrect_A = boot(d$correctA, samplemean, R=1000) # 1000 replications
plot(bootCorrect_A)

bootCorrect_B = boot(d$correctB, samplemean, R=1000) # 1000 replications
plot(bootCorrect_B)
# Seems okay up to here


# TIME-global Function to find the bootstrap Confidence Intervals 
timeCI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca")) 
lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
vecTimeCI <- c(lowerTimeCI, higherTimeCI)

bootDuration_in_seconds_WHAT_Qn = boot(d$PageSubmit[d$focus=="WHAT_Qn"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHAT_Ql = boot(d$PageSubmit[d$focus=="WHAT_Ql"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHERE = boot(d$PageSubmit[d$focus=="WHERE"], samplemean, R=1000) # 1000 replications
timeCI_WHAT_Qn <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Qn, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHAT_Ql <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Ql, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHERE <- boot.ci(boot.out = bootDuration_in_seconds_WHERE, type = c("norm", "basic", "perc", "bca")) 
indexesV <- 1:length(c(timeCI_WHAT_Qn$normal[2],timeCI_WHAT_Qn$normal[3]))
indexesCategories <- 1:3
meansCategories <- c(mean(d$PageSubmit[d$focus=="WHAT_Qn"]), mean(d$PageSubmit[d$focus=="WHAT_Ql"]), mean(d$PageSubmit[d$focus=="WHERE"]))

#describe(d) # Entire data frame # bug now... no idea what is happening here

# Display of confidence intervals based on the focus, with the y axis displaying the time taken to answer. Currently using randomly generated data
dfCategories <- data.frame(x= c("WHAT_Qn","WHAT_Ql","WHERE") ,
                           F = meansCategories, L = c(timeCI_WHAT_Qn$normal[2], timeCI_WHAT_Ql$normal[2], timeCI_WHERE$normal[2]),
                           U = c(timeCI_WHAT_Qn$normal[3], timeCI_WHAT_Ql$normal[3], timeCI_WHERE$normal[3])
                           )
ggplot(dfCategories, aes(x = x, y = F)) + geom_point(size = 4) + geom_errorbar(aes(ymax = U, ymin = L))
# Seems fine up to here.



# AverageCorrect-global Function to find the bootstrap Confidence Intervals 
timeCI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca")) 
lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
vecTimeCI <- c(lowerTimeCI, higherTimeCI)



# bootDuration_in_secondsEEE = boot(d$PageSubmit[d$ComplexityGroup=="EEE"], samplemean, R=1000) # 1000 replications
# bootDuration_in_secondsMMM = boot(d$PageSubmit[d$ComplexityGroup=="MMM"], samplemean, R=1000) # 1000 replications
# bootDuration_in_secondsHHH = boot(d$PageSubmit[d$ComplexityGroup=="HHH"], samplemean, R=1000) # 1000 replications
# timeCIEEE <- boot.ci(boot.out = bootDuration_in_secondsEEE, type = c("norm", "basic", "perc", "bca")) 
# timeCIMMM <- boot.ci(boot.out = bootDuration_in_secondsMMM, type = c("norm", "basic", "perc", "bca")) 
# timeCIHHH <- boot.ci(boot.out = bootDuration_in_secondsHHH, type = c("norm", "basic", "perc", "bca")) 
# indexesV <- 1:length(c(timeCIEEE$normal[2],timeCIEEE$normal[3]))
# indexesCategories <- 1:3
# # Is that the right means?
# meansCategories <- c(mean(d$PageSubmit[d$ComplexityGroup=="EEE"]), mean(d$PageSubmit[d$ComplexityGroup=="MMM"]), mean(d$PageSubmit[d$ComplexityGroup=="HHH"]))
# 
# dfCategories <- data.frame(x= c("EEE","MMM","HHH") ,
#                            F = meansCategories, L = c(timeCIEEE$normal[2], timeCIMMM$normal[2], timeCIHHH$normal[2]),
#                            U = c(timeCIEEE$normal[3], timeCIMMM$normal[3], timeCIHHH$normal[3])
# )
# ggplot(dfCategories, aes(x = x, y = F)) + geom_point(size = 4) + geom_errorbar(aes(ymax = U, ymin = L))



