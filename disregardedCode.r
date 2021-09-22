

# Used to work, now buggy
# dfwc_new <- summarySEwithin(d, measurevar="diffA1", withinvars="dMask", idvar="idc", na.rm=FALSE, conf.interval=.95)
# dfwc_new

# ggplot(dfwc, aes(x=diffA1, y=dMask, group=1)) +
#   # geom_line() +
#   geom_errorbar(width=.1, aes(xmin=diffA1-ci, xmax=diffA1+ci)) +
#   geom_point(shape=21, size=3, fill="white") +
#   facet_wrap(~ focus)
# #+ ylim(40,60)


testStructure <-
  structure(
    list(
      Estimate = c(0.1784, 0.073, 0.0619, 0.1367, 0.1795, 0.087),
      name = structure( c(1L, 6L, 5L, 4L, 3L, 2L),
        .Label = c("Intercept", "Doctor spouse", "8 years experience", "3 years experience", "1 year experience", "Female" ),
        class = "factor"
      ),
      group = structure(
        c(1L, 2L, 3L, 3L, 3L, 4L),
        .Label = c("Intercept", "Male to", "0 Years Experience to", "No Spouse to"),
        class = "factor"
      ),
      upper.95 = c(0.209, 0.0899, 0.0858, 0.1606, 0.2034, 0.1077),
      lower.95 = c(0.1478, 0.0561, 0.038, 0.1129, 0.1556, 0.0662),
      resp_type = c( "Legislator", "Legislator", "Legislator", "Legislator", "Legislator", "Legislator" )
    ),
    row.names = c(NA,-6L),
    class = c("tbl_df", "tbl", "data.frame")
  )

testStructure

# ggplot(dfwc, aes( xmin=-.10, xmax = .20, x=diffA1, y=interaction(dMask, N))) +
#        geom_point(size = 4) +
#        geom_errorbar(aes(x=diffA1,xmin = diffA1-ci, xmax = diffA1+ci, y=interaction(dMask, N))) #+ facet_wrap(~dMask)
       
# 
# ggplot(data = d, aes(x=diffA1,y=dMask))+
#   geom_errorbar(width=.1,aes(x=diffA1, xmin=diffA1-ci,xmax=diffA1+ci)) +
#   geom_point(mapping = x=diffA1)#+facet_wrap(~focus)



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
 


