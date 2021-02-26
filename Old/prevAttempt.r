# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio) 

getwd()

d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")

View(d)

names(d)

levels(d$Question_Block) # not working, as it is not a factor

table(d$TimeSubmitSeconds)

### boxplot examples
#boxplot(d$TimeSubmitSeconds~d$Confidence_Avg.Higher, las=1, ylab="TimeSubmitSeconds",xlab="Confidence_Avg.Higher",main="Confidence and time")
boxplot(d$Bool_Avg.Higher~d$Question_Block, las=1, xlab="Question_Block",ylab="Bool_Avg.Higher",main="Correct and Question block")
boxplot(d$TimeSubmitSeconds~d$Bool_Avg.Higher, las=1, ylab="TimeSubmitSeconds",xlab="Bool_Avg.Higher",main="Correct and TimeSubmitSeconds")

### mean examples
mean(d$Bool_More.Variation[d$Question_Block==7])
mean(d$Bool_More.Variation[d$Question_Block==1])
mean(d$Bool_Avg.Higher[d$Question_Block==1])
mean(d$TimeSubmitSeconds[d$Question_Block==1])
mean(d$TimeSubmitSeconds[d$Question_Block==2])
mean(d$TimeSubmitSeconds[d$Question_Block==3])
mean(d$TimeSubmitSeconds[d$Question_Block<=3])
mean(d$TimeSubmitSeconds[d$Question_Block>3 & d$Question_Block<=6])
mean(d$TimeSubmitSeconds[d$Question_Block>6])
mean(d$TimeSubmitSeconds[d$ConditionType=="WHAT_Ql"])
mean(d$TimeSubmitSeconds[d$ConditionType=="WHAT_Qn"])
mean(d$TimeSubmitSeconds[d$ConditionType=="WHERE"])

t.test(d$Bool_Avg.Higher~d$Bool_More.Variation,paired=F,var.eq=F)


### Bootstrapping for testing hypotheses
set.seed(112358)
n <- length(d$ResponseId) # The number of observations to sample
n

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
test.stat1
test.stat2

# Calculate the means (Yc and Ym) for each of the bootstraps samples
# Initialize vectors to store the test-stats
Boot.test.stat1 <- rep(0,B)
Boot.test.stat2 <- rep(0,B)
for (i in 1:B){
  #calculate the boot-test-stat1 and save it 
  # For pilot and warm-up, let's try EEE and MMM versus HHH (Need to ensure ordering in such a way that we are confident in data selection... or add more attributes to the table)
  Boot.test.stat1[i] <- abs( mean(BootstrapSamples[1:88,i]) - mean(BootstrapSamples[89:dimBoot[1],i])) 
  
  #calculate the boot-test-stat1 and save it 
  # For pilot and warm-up, let's try EEE and MMM versus HHH (Need to ensure ordering in such a way that we are confident in data selection... or add more attributes to the table)
  Boot.test.stat2[i] <- abs( median(BootstrapSamples[1:88,i]) - median(BootstrapSamples[89:dimBoot[1],i])) 
}

test.stat1;test.stat2

# and, take a look at the first 20 Bootstrap-TEST STATS for 1 and 2
round(Boot.test.stat1[1:20], 1)
round(Boot.test.stat2[1:20], 1)

# and, let's calculate the bootstrap p-value...
# notice how we can ask R a true/false question...(for the first 20)
(Boot.test.stat1 >= test.stat1)[1:20]
# and if we ask for the mean of all of those, it treats 0=FALSE, 1=TRUE
#...calculate the p-value
mean( Boot.test.stat1 >= test.stat1)

# let's calculate the p-value for test statistic 2 (abs diff in medians)
mean( Boot.test.stat2 >= test.stat2)

# now, recall the difference between "statistical significance" and 
# "scientific significance"
### in a "real-world" what would you want to conclude here
table(d$feed)

# let's take a look at a density plot of all the Bootstrap test-stats, and 
# add in our Observed test stat
plot(density(Boot.test.stat1), 
     xlab=expression( group("|", bar(Yc) - bar(Ym), "|") ) , 
     main="Bootstrap Test Stats", las=1)
abline(v=test.stat1, col="blue", lty="dotted")
text(60,0.0005, "p-value", col="blue", cex=0.7)

###########################
### Code to run the analysis, using a test stat of diff in 90th percentiles
###########################

# lets calculate the absolute diff in 90th percentiles
test.stat3 <- abs(quantile(d$TimeSubmitSeconds[d$Question_Block<=6], prob=0.9) - quantile(d$TimeSubmitSeconds[d$Question_Block>6], prob=0.9))  #diff in medians
test.stat3

# initialize a vector to save the bootstrap test stats in
Boot.test.stat3 <- rep(0,B)

# run thru a loop calculating the bootstrap test statistics
for (i in 1:B){
  # calculate the boot-test-stat3 and save it
  Boot.test.stat3[i] <- abs( quantile(BootstrapSamples[1:88,i], prob=0.9) - 
                               quantile(BootstrapSamples[89:dimBoot[1],i], prob=0.9) )
}

# and, calculate the p-value
mean( Boot.test.stat3 >= test.stat3)

