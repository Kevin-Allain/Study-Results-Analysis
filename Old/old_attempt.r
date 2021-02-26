#plot(vecTimeCI)
#points(x=1, y=vecTimeCI, col="red" )
#plot("timeCI",lowerTimeCI, higherTimeCI)

###test plotting

#dim(col(d$"TimeSubmitSeconds"))
#meanForColumn(d,"TimeSubmitSeconds")
#bootstrap <- boot(d, mean(d$TimeSubmitSeconds), R=1000)

### global
# time
t.test(d$TimeSubmitSeconds)
sd.glbl.timesubmitSeconds <- sd(d$TimeSubmitSeconds)
a.glbl.timesubmitSeconds <- mean(d$TimeSubmitSeconds)
n.glbl.timesubmitSeconds <- length(d$ResponseId)
error.glbl.timesubmitSeconds <- qnorm(0.975) * sd.glbl.timesubmitSeconds/sqrt(n.glbl.timesubmitSeconds)
lowerbound.glbl.timesubmitSeconds <- a.glbl.timesubmitSeconds-error.glbl.timesubmitSeconds
upperbound.glbl.timesubmitSeconds <- a.glbl.timesubmitSeconds+error.glbl.timesubmitSeconds
# correct answer for average estimation
t.test(d$Bool_Avg.Higher)
sd.glbl.Bool_Avg.Higher <- sd(d$Bool_Avg.Higher)
a.glbl.Bool_Avg.Higher <- mean(d$Bool_Avg.Higher)
n.glbl.Bool_Avg.Higher <- length(d$ResponseId)
error.glbl.Bool_Avg.Higher <- qnorm(0.975) * sd.glbl.Bool_Avg.Higher/sqrt(n.glbl.Bool_Avg.Higher)
lowerbound.glbl.Bool_Avg.Higher <- a.glbl.Bool_Avg.Higher-error.glbl.Bool_Avg.Higher
upperbound.glbl.Bool_Avg.Higher <- a.glbl.Bool_Avg.Higher+error.glbl.Bool_Avg.Higher
# correct answer for variation estimation
t.test(d$Bool_More.Variation)
sd.glbl.Bool_More.Variation <- sd(d$Bool_More.Variation)
a.glbl.Bool_More.Variation <- mean(d$Bool_More.Variation)
n.glbl.Bool_More.Variation <- length(d$ResponseId)
error.glbl.Bool_More.Variation <- qnorm(0.975) * sd.glbl.Bool_More.Variation/sqrt(n.glbl.Bool_More.Variation)
lowerbound.glbl.Bool_Avg.Higher <- a.glbl.Bool_More.Variation-error.glbl.Bool_More.Variation
upperbound.glbl.Bool_Avg.Higher <- a.glbl.Bool_More.Variation+error.glbl.Bool_More.Variation
# confidence for average estimation
t.test(d$Confidence_Avg.Higher)
sd.glbl.Confidence_Avg.Higher <- sd(d$Confidence_Avg.Higher)
a.glbl.Confidence_Avg.Higher <- mean(d$Confidence_Avg.Higher)
n.glbl.Confidence_Avg.Higher <- length(d$ResponseId)
error.glbl.Confidence_Avg.Higher <- qnorm(0.975) * sd.glbl.Confidence_Avg.Higher/sqrt(n.glbl.Confidence_Avg.Higher)
lowerbound.glbl.Bool_Avg.Higher <- a.glbl.Confidence_Avg.Higher-error.glbl.Confidence_Avg.Higher
upperbound.glbl.Bool_Avg.Higher <- a.glbl.Confidence_Avg.Higher+error.glbl.Confidence_Avg.Higher
# confidence for variation estimation
t.test(d$Confidence_More.Variation)
sd.glbl.Confidence_More.Variation <- sd(d$Confidence_More.Variation)
a.glbl.Confidence_More.Variation <- mean(d$Confidence_More.Variation)
n.glbl.Confidence_More.Variation <- length(d$ResponseId)
error.glbl.Confidence_More.Variation <- qnorm(0.975) * sd.glbl.Confidence_More.Variation/sqrt(n.glbl.Confidence_More.Variation)
lowerbound.glbl.Bool_Avg.Higher <- a.glbl.Confidence_More.Variation-error.glbl.Confidence_More.Variation
upperbound.glbl.Bool_Avg.Higher <- a.glbl.Confidence_More.Variation+error.glbl.Confidence_More.Variation



### task dependent

### category dependent

### task and category dependent

### displays