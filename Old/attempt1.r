# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.youtube.com/watch?v=Zet-qmEEfCU and https://www.youtube.com/watch?v=Om5TMGj9td4&t=2s

# File:    attempt1.R
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-09


### Install and load packages ################################

library(datasets)  # Load base packages manually
library(lattice)

#########################
### Confidence Interval
#########################
# load file
d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")

### Bootstrapping for testing hypotheses
set.seed(112358)
n <- length(d$ResponseId) # The number of observations to sample

B <- 10000 # The number of bootstraps samples
variable <- d$TimeSubmitSeconds

BootstrapSamples <- matrix( sample(variable, size=n*B, replace=TRUE), nrow=n, ncol=B)
dimBoot <- dim(BootstrapSamples)
dimBoot[1]

# Original means and medians for our two groups
#diff in means EEE and MMM VS HHH
test.stat1 <- abs(mean(d$TimeSubmitSeconds[d$Question_Block<=6]) - mean(d$TimeSubmitSeconds[d$Question_Block>6]))  
#diff in medians EEE and MMM VS HHH
test.stat2 <- abs(median(d$TimeSubmitSeconds[d$Question_Block<=6]) - median(d$TimeSubmitSeconds[d$Question_Block>6]))  

# Let's put the differences of means and medians in a new variable # Is this right?!
Obs.Diff.In.Means <-  test.stat1   #diff in means EEE and MMM VS HHH
Obs.Diff.In.Medians <- test.stat2 #diff in medians EEE and MMM VS HHH

# set up a tad dirty, but that should work for testing the code
set.seed(13759)
#n.c <- 88 # Number of samples in categories EEE and MMM
#n.m <- dimBoot[1] - 88 # Number of samples in categories HHH
n.EEE <- length(d$ResponseId[d$ComplexityGroup=="EEE"]) # Number of samples in categories EEE
n.MMM <- length(d$ResponseId[d$ComplexityGroup=="MMM"]) # Number of samples in categories EEE
n.HHH <- length(d$ResponseId[d$ComplexityGroup=="HHH"]) # Number of samples in categories HHH

B <- 10000 # number of bootstrap samples

# now, get those bootstrap samples (without loops!) ### IMPORTANT PART 1
# stick each Boot-sample in a column...
Boot.EEE <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="EEE"], size= B*n.EEE, 
                              replace=TRUE), ncol=B, nrow=n.EEE)
Boot.MMM <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="MMM"], size= B*n.MMM, 
                           replace=TRUE), ncol=B, nrow=n.MMM)
Boot.HHH <- matrix( sample(d$TimeSubmitSeconds[d$ComplexityGroup=="HHH"] , size= B*n.HHH, 
                                replace=TRUE), nrow=n.HHH, ncol=B)
#check dimensions
dim(Boot.EEE); dim(Boot.MMM);dim(Boot.HHH)
#check what's inside
Boot.EEE[1:5,1:5]; Boot.MMM[1:5,1:5] ;Boot.HHH[1:5,1:5]


# calculate the difference in MEANS for each of the bootsamples
Boot.Diff.In.Means <- colMeans(Boot.EEE) - colMeans(Boot.HHH)
Boot.Diff.In.Means
# check that
length(Boot.Diff.In.Means)
# and, look at the first 10 diff in means
Boot.Diff.In.Means[1:10]

# calculate the difference in MEDIANS for each of the bootsamples
Boot.Diff.In.Medians <- apply(Boot.EEE, MARGIN=2, FUN=median) - apply(Boot.HHH, MARGIN=2, FUN=median)
# check that
length(Boot.Diff.In.Medians)
# and, look at the first 10 diff in medians
Boot.Diff.In.Medians[1:10]


#### MAKE THE CONFIDENCE INTERVALS (using 95% confidence)

# let's look at the PERCENTILE METHOD
# the "PERCENTILE" bootstrap confidence interval
# first, for the difference in MEANS
lowerEEEvsHHH = quantile(Boot.Diff.In.Means, prob=0.025)
upperEEEvsHHH = quantile(Boot.Diff.In.Means, prob=0.975)
# We are 95% confident that the true/population difference in means is somewhere between 9.54s and 48.96s
# We are 95% confident that the mean time for EEEMMM is somewhere between  9.54s lower than HHH and up to 48.96s higher than HHH
# Something we noticed, but are still surprised with...

# and then, the difference in MEDIANS
quantile(Boot.Diff.In.Medians, prob=0.025)
quantile(Boot.Diff.In.Medians, prob=0.975)
# We are 95% confident that the median time for EEE is somewhere between 11.31s lower than HHH and 34.79s higher than HHH

##### Not in the video
### What do you make of the fact that these both cross 0?

### Apart from "statistical significance", what do you think about
###    "scientific significance" here?

# below is code to calculate confidence interval using the BASIC method

# let's look at the BASIC METHOD
# first, for the difference in MEANS
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.975)
2*Obs.Diff.In.Means - quantile(Boot.Diff.In.Means, prob=0.025)
# and then, the difference in MEDIANS
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.975)
2*Obs.Diff.In.Medians - quantile(Boot.Diff.In.Medians, prob=0.025)

########
### Code for confidence interval for difference in 80th percentiles
########

# calculate the observed difference in 80th percentiles
Obs.Diff.In.80per <- (quantile(d$TimeSubmitSeconds[d$ComplexityGroup=="EEE"], prob=0.80) - quantile(d$TimeSubmitSeconds[d$Question_Block=="HHH"], prob=0.80))
Obs.Diff.In.80per

# calculate the difference in 80th percentile for each of the bootsamples
Boot.Diff.In.80per <- apply(Boot.EEE, MARGIN=2, FUN=quantile, prob=0.80) - apply(Boot.HHH, MARGIN=2, FUN=quantile, prob=0.80)

# let's look at the PERCENTILE METHOD for the difference in 80th percentile
quantile(Boot.Diff.In.80per, prob=0.025)
quantile(Boot.Diff.In.80per, prob=0.975)



