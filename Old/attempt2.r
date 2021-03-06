# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.cyclismo.org/tutorial/R/confidence.html#id1 and https://www.cyclismo.org/tutorial/R/pValues.html#t-test and https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/ and http://www.mayin.org/ajayshah/KB/R/documents/boot.html and https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals#14069837

# File:    attempt2.R
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-17

# Import library for bootstrap methods 
library(boot) 

# Import library for plotting 
library(ggplot2) 
library(lattice)



# load file
d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")

### Bootstrapping for testing hypotheses
set.seed(112358)
n <- length(d$ResponseId) # The number of observations to sample
n.EEE <- length(d$ResponseId[d$ComplexityGroup=="EEE"]) # Number of samples in categories EEE
n.MMM <- length(d$ResponseId[d$ComplexityGroup=="MMM"]) # Number of samples in categories EEE
n.HHH <- length(d$ResponseId[d$ComplexityGroup=="HHH"]) # Number of samples in categories HHH
n.WHAT_Qn <- length(d$ResponseId[d$ConditionType=="WHAT_Qn"]) # Number of samples in categories EEE
n.WHAT_Ql <- length(d$ResponseId[d$ConditionType=="WHAT_Ql"]) # Number of samples in categories EEE
n.WHERE <- length(d$ResponseId[d$ConditionType=="WHERE"]) # Number of samples in categories HHH
B <- 10000 # The number of bootstraps samples
variable <- d$TimeSubmitSeconds

BootstrapSamples <- matrix( sample(variable, size=n*B, replace=TRUE), nrow=n, ncol=B)
dimBoot <- dim(BootstrapSamples)

# now, get those bootstrap samples (without loops!) ### IMPORTANT PART 1
# stick each Boot-sample in a column...
Boot.EEE <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="EEE"], size= B*n.EEE, replace=TRUE), ncol=B, nrow=n.EEE)
Boot.MMM <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="MMM"], size= B*n.MMM, replace=TRUE), ncol=B, nrow=n.MMM)
Boot.HHH <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="HHH"] , size= B*n.HHH, replace=TRUE), nrow=n.HHH, ncol=B)

Boot.WHAT_Qn <- matrix( sample(d$TimeSubmitSeconds[d$ConditionType=="WHAT_Qn"], size= B*n.WHAT_Qn, replace=TRUE), ncol=B, nrow=n.WHAT_Qn)
Boot.WHAT_Ql <- matrix( sample(d$TimeSubmitSeconds[d$ConditionType=="WHAT_Ql"], size= B*n.WHAT_Ql, replace=TRUE), ncol=B, nrow=n.WHAT_Ql)
Boot.WHERE <- matrix( sample(d$TimeSubmitSeconds[d$ConditionType=="WHERE"] , size= B*n.WHERE, replace=TRUE), ncol=B, nrow=n.WHERE)

Boot.EEE[1:5,1:5]; Boot.WHERE[1:5,1:5];

# Confidence interval: average +- z score * standard error
# 95% confidence interval means 2.5% on each side
#todo: add usage of bootstrap instead of d

dim(d$TimeSubmitSeconds)
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
bootTimeSubmitSeconds = boot(d$TimeSubmitSeconds, samplemean, R=1000) # 1000 replications
plot(bootTimeSubmitSeconds)

bootCorrectAvgEstimation = boot(d$Bool_Avg.Higher, samplemean, R=1000) # 1000 replications
plot(bootCorrectAvgEstimation)

bootCorrectVarEstimation = boot(d$Bool_More.Variation, samplemean, R=1000) # 1000 replications
plot(bootCorrectVarEstimation)


# Function to find the  
# bootstrap Confidence Intervals 
timeCI <- boot.ci(boot.out = bootTimeSubmitSeconds, type = c("norm", "basic", "perc", "bca")) 
lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
vecTimeCI <- c(lowerTimeCI, higherTimeCI)

bootTimeSubmitSecondsEEE = boot(d$TimeSubmitSeconds[d$ComplexityGroup=="EEE"], samplemean, R=1000) # 1000 replications
bootTimeSubmitSecondsMMM = boot(d$TimeSubmitSeconds[d$ComplexityGroup=="MMM"], samplemean, R=1000) # 1000 replications
bootTimeSubmitSecondsHHH = boot(d$TimeSubmitSeconds[d$ComplexityGroup=="HHH"], samplemean, R=1000) # 1000 replications
timeCIEEE <- boot.ci(boot.out = bootTimeSubmitSecondsEEE, type = c("norm", "basic", "perc", "bca")) 
timeCIMMM <- boot.ci(boot.out = bootTimeSubmitSecondsMMM, type = c("norm", "basic", "perc", "bca")) 
timeCIHHH <- boot.ci(boot.out = bootTimeSubmitSecondsHHH, type = c("norm", "basic", "perc", "bca")) 
indexesV <- 1:length(c(timeCIEEE$normal[2],timeCIEEE$normal[3]))
indexesCategories <- 1:3
meansCategories <- c(mean(d$TimeSubmitSeconds[d$ComplexityGroup=="EEE"]), mean(d$TimeSubmitSeconds[d$ComplexityGroup=="MMM"]), mean(d$TimeSubmitSeconds[d$ComplexityGroup=="HHH"]))
#plot( indexesCategories, c(timeCIEEE$normal[2],timeCIEEE$normal[3]), c(timeCIMMM$normal[2],timeCIMMM$normal[3]), c(timeCIHHH$normal[2],timeCIHHH$normal[3]),)

dfCategories <- data.frame(x= c("EEE","MMM","HHH") ,
                           F = meansCategories,
                           L = c(timeCIEEE$normal[2], timeCIMMM$normal[2], timeCIHHH$normal[2]),
                           U = c(timeCIEEE$normal[3], timeCIMMM$normal[3], timeCIHHH$normal[3])
                           )
ggplot(dfCategories, aes(x = x, y = F)) + geom_point(size = 4) + geom_errorbar(aes(ymax = U, ymin = L))


