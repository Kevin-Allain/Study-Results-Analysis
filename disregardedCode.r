

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
 
# #######################
if (dMask == "" & dComplex_focus ==""){
  if (question == "diffA1"){
    if (length(d2$diffA1[d2$focus==focus])==0){
      boot_d <- boot(d$diffA1[d$focus==focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d2$diffA1[d2$focus==focus])==0){
      boot_d2 <- boot(d2$diffA1[d2$focus==focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      print("should reach here")
      boot_d <- boot(d$diffA1[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus],samplemean,R)
      sampleSize <- length(d$diffA1[d$focus==focus]); sampleSize2 <- length(d2$diffA1[d2$focus==focus]);
      print("ran the boot")
    }
  }else if (question == "diffA2"){
    if (length(d2$diffA2[d2$focus==focus])==0){
      boot_d <- boot(d$diffA2[d$focus==focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA2[d$focus==focus])==0){
      boot_d2 <- boot(d2$diffA2[d2$focus==focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA2[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus],samplemean,R)
      sampleSize <- length(d$diffA2[d$focus==focus]); sampleSize2 <- length(d2$diffA2[d2$focus==focus]);
    }
  } else {
    if (length(d2$diffA3[d2$focus==focus])==0){
      boot_d <- boot(d$diffA3[d$focus==focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA3[d$focus==focus])==0){
      boot_d2 <- boot(d2$diffA3[d2$focus==focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus],samplemean,R)
      sampleSize <- length(d$diffA3[d$focus==focus]); sampleSize2 <- length(d2$diffA3[d2$focus==focus]);
    }
  }
} 
else if (dMask != "" & dComplex_focus ==""){
  if (question == "diffA1"){
    if (length(d2$diffA1[d2$focus==focus & d2$dMask==dMask])==0){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA1[d$focus==focus & d$dMask==dMask])==0) {
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    }  else{
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sampleSize <- length(d$diffA1[d$focus==focus & d$dMask==dMask]); sampleSize2 <- length(d2$diffA1[d2$focus==focus & d2$dMask==dMask]);
    }
  }else if (question == "diffA2"){
    if (length(d2$diffA2[d2$focus==focus & d2$dMask==dMask])==0){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA2[d$focus==focus & d$dMask==dMask])==0) {
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    }  else{
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sampleSize <- length(d$diffA2[d$focus==focus & d$dMask==dMask]); sampleSize2 <- length(d2$diffA2[d2$focus==focus & d2$dMask==dMask]);
    }
  } else {
    if (length(d2$diffA3[d2$focus==focus & d2$dMask==dMask])==0){
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA3[d$focus==focus & d$dMask==dMask])==0) {
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    }  else{
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask],samplemean,R)
      sampleSize <- length(d$diffA3[d$focus==focus & d$dMask==dMask]); sampleSize2 <- length(d2$diffA3[d2$focus==focus & d2$dMask==dMask]);
    }
  }
} 
else if (dMask == "" & dComplex_focus !=""){
  if (question == "diffA1"){
    if (length(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_focus]) == 0){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus]) == 0){
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_focus]);
    }
  }else if (question == "diffA2"){
    if (length(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_focus]) == 0){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus]) == 0){
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_focus]);
    }
  } else {
    if (length(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_focus]) == 0){
      boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus]) == 0){
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_focus]);
    }
  }
} 
else {
  if (question == "diffA1"){
    if(length(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus])==0){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus])==0) {
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus]);
    }
  }else if (question == "diffA2"){
    if(length(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus])==0){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus])==0) {
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus]);
    }
  } else {
    if(length(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus])==0){
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      sumD <- getMean_lowCI_highCI(boot_d)
      return(sumD)
    } else if (length(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus])==0) {
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sumD2 <- getMean_lowCI_highCI(boot_d2)
      return(sumD2)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
      sampleSize <- length(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]); sampleSize2 <- length(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus]);
    }
  }
}

# ############################################
# groupedData_all$mean_t0_DiffA1[groupedData_all$focus=="WHAT_Qn"][groupedData_all$scaling==0] <- summ_sclAll_diffA1_WHAT_Qn[1]; # groupedData_all$low_ci_DiffA1[groupedData_all$focus=="WHAT_Qn"][groupedData_all$scaling==0] <- summ_sclAll_diffA1_WHAT_Qn[2];# groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn"][groupedData_all$scaling==0] <- summ_sclAll_diffA1_WHAT_Qn[3];
print("pre changes"); groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling== 1]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling== 2]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling== 1]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling== 2]
# TODO add also summary according to difficulty of the focus and to the mask difficulty. By putting it in a function...
# a function like init_groupedData_all
for (i in 1:3){
  groupedData_all$mean_t0_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA1_WHAT_Qn[1+(3*(i-1))];
  groupedData_all$low_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA1_WHAT_Qn[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA1_WHAT_Qn[3+(3*(i-1))];
  groupedData_all$mean_t0_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA1_WHAT_Ql[1+(3*(i-1))]; 
  groupedData_all$low_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA1_WHAT_Ql[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA1_WHAT_Ql[3+(3*(i-1))];
  groupedData_all$mean_t0_DiffA2[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA2_WHAT_Qn[1+(3*(i-1))]; 
  groupedData_all$low_ci_DiffA2[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA2_WHAT_Qn[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA2[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA2_WHAT_Qn[3+(3*(i-1))];
  groupedData_all$mean_t0_DiffA2[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA2_WHAT_Ql[1+(3*(i-1))]; 
  groupedData_all$low_ci_DiffA2[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA2_WHAT_Ql[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA2[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA2_WHAT_Ql[3+(3*(i-1))];
  groupedData_all$mean_t0_DiffA3[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA3_WHAT_Qn[1+(3*(i-1))]; 
  groupedData_all$low_ci_DiffA3[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA3_WHAT_Qn[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA3[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA3_WHAT_Qn[3+(3*(i-1))];
  groupedData_all$mean_t0_DiffA3[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA3_WHAT_Ql[1+(3*(i-1))]; 
  groupedData_all$low_ci_DiffA3[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1) ] <- summ_sclAll_diffA3_WHAT_Ql[2+(3*(i-1))]; 
  groupedData_all$high_ci_DiffA3[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling == (i-1)] <- summ_sclAll_diffA3_WHAT_Ql[3+(3*(i-1))];
}
print ("post changes");groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling== 1]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Qn" & groupedData_all$scaling== 2]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling== 1]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql" & groupedData_all$scaling== 2]
# groupedData_all$mean_t0_DiffA1[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA1_sclAll_WHAT_Ql[1]; groupedData_all$low_ci_DiffA1[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA1_sclAll_WHAT_Ql[2]; groupedData_all$high_ci_DiffA1[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA1_sclAll_WHAT_Ql[3]; groupedData_all$mean_t0_DiffA2[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA2_sclAll_WHAT_Qn[1];  groupedData_all$low_ci_DiffA2[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA2_sclAll_WHAT_Qn[2];  groupedData_all$high_ci_DiffA2[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA2_sclAll_WHAT_Qn[3]; groupedData_all$mean_t0_DiffA2[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA2_sclAll_WHAT_Ql[1];  groupedData_all$low_ci_DiffA2[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA2_sclAll_WHAT_Ql[2];  groupedData_all$high_ci_DiffA2[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA2_sclAll_WHAT_Ql[3]; groupedData_all$mean_t0_DiffA3[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA3_sclAll_WHAT_Qn[1];  groupedData_all$low_ci_DiffA3[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA3_sclAll_WHAT_Qn[2];  groupedData_all$high_ci_DiffA3[groupedData_all$focus=="WHAT_Qn"] <- summ_diffA3_sclAll_WHAT_Qn[3]; groupedData_all$mean_t0_DiffA3[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA3_sclAll_WHAT_Ql[1];  groupedData_all$low_ci_DiffA3[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA3_sclAll_WHAT_Ql[2];  groupedData_all$high_ci_DiffA3[groupedData_all$focus=="WHAT_Ql"] <- summ_diffA3_sclAll_WHAT_Ql[3];


# ##########################################
groupedData_scl0 <- d_scl0 %>%
  group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
  )
groupedData_scl0["low_ci_DiffA1"] <-NA; groupedData_scl0["high_ci_DiffA1"] <-NA; groupedData_scl0["low_ci_DiffA2"] <-NA; groupedData_scl0["high_ci_DiffA2"] <-NA; groupedData_scl0["low_ci_DiffA3"] <-NA; groupedData_scl0["high_ci_DiffA3"] <-NA;
groupedData_scl0["mean_t0_DiffA1"]<-NA;groupedData_scl0["mean_t0_DiffA2"]<-NA;groupedData_scl0["mean_t0_DiffA3"]<-NA;

groupedData_scl1 <- d_scl1 %>%
  group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
  )
groupedData_scl1["low_ci_DiffA1"] <-NA; groupedData_scl1["high_ci_DiffA1"] <-NA; groupedData_scl1["low_ci_DiffA2"] <-NA; groupedData_scl1["high_ci_DiffA2"] <-NA; groupedData_scl1["low_ci_DiffA3"] <-NA; groupedData_scl1["high_ci_DiffA3"] <-NA;
groupedData_scl1["mean_t0_DiffA1"]<-NA;groupedData_scl1["mean_t0_DiffA2"]<-NA;groupedData_scl1["mean_t0_DiffA3"]<-NA;

groupedData_scl2 <- d_scl2 %>%
  group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
  )
groupedData_scl2["low_ci_DiffA1"] <-NA; groupedData_scl2["high_ci_DiffA1"] <-NA; groupedData_scl2["low_ci_DiffA2"] <-NA; groupedData_scl2["high_ci_DiffA2"] <-NA; groupedData_scl2["low_ci_DiffA3"] <-NA; groupedData_scl2["high_ci_DiffA3"] <-NA;
groupedData_scl2["mean_t0_DiffA1"]<-NA;groupedData_scl2["mean_t0_DiffA2"]<-NA;groupedData_scl2["mean_t0_DiffA3"]<-NA;

# ###############
# if (i=="diffA1"){
#   groupedPlotCI_1 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor3)) +
#     geom_vline(xintercept = 0) +
#     geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#     geom_point(size=3,col="black",fill="white", shape=1) +
#     xlim(c(-absGraphEdge,absGraphEdge)) +
#     facet_wrap( as.formula(paste("~",factor1,"+",factor2)) , dir="v", ncol=1) +
#     ggtitle(strSentence)
#   cat("\n____plot1 should NOT be null: ",(is.null(groupedPlotCI_1)))
# } 
# else if (i=="diffA2"){
#   groupedPlotCI_2 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor3)) +
#     geom_vline(xintercept = 0) +
#     geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#     geom_point(size=3,col="black",fill="white", shape=1) +
#     xlim(c(-absGraphEdge,absGraphEdge)) +
#     facet_wrap( as.formula(paste("~",factor1,"+",factor2)) , dir="v", ncol=1) +
#     ggtitle(strSentence)
#   cat("\n____plot2 should NOT be null: ",(is.null(groupedPlotCI_1)))
# } 
# else {
#   groupedPlotCI_3 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor3)) +
#     geom_vline(xintercept = 0) +
#     geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#     geom_point(size=3,col="black",fill="white", shape=1) +
#     xlim(c(-absGraphEdge,absGraphEdge)) +
#     facet_wrap( as.formula(paste("~",factor1,"+",factor2)) , dir="v", ncol=1) +
#     ggtitle(strSentence)
#   cat("\n____plot3 should NOT be null: ",(is.null(groupedPlotCI_3)))
# }


# cat("\nwhat of dfCI_global$orderCategoryCombination? ",toString(unique(dfCI_global$orderCategoryCombination)))
# cat("\nwhat of dfCI_global$focus? ",toString(unique(dfCI_global$focus)))
# cat("\nwhat of dfCI_global$scaling? ",toString(unique(dfCI_global$scaling)))
# cat("\nwhat of dfCI_global$orderedScaling? ",toString(unique(dfCI_global$orderedScaling)))
# cat("\n----colnames: ",toString(colnames(dfCI_global)))
# cat("\n----dim: ",toString(dim(dfCI_global)))  
# cat("\n----first line: ",toString(dfCI_global[1,]))




# dfCI_global_distractorStudy_dMask_scaling
# 
# toString(dfCI_global_scalingXfocus_dMask[0,])
# 
# arrQuestions <- c("diffA1","diffA2","diffA3")
# for (i in arrQuestions){ cat("\n",i,", ",arrQuestions[i]) }
# 
# renamedTest_scalingXdMask_focus <- renameGroupedData(dfCI_global_scalingXdMask_focus)
# 
# groupedData_all <- generateGroupedData(d_sclAll)


# TODO: FIX THE ERROR RATE CODE adapt for other categories combinations... and other studies?
# double check & display according to confidence interval differences...
# genAndPlot_differencesBoot_scaling(d_scl0,d_scl1,d_scl2,"correctB")
# genAndPlot_errorRate_correctB_scaling(d_scl0, d_scl1, d_scl2)
# df_errorRate <- data.frame(res_scl0,res_scl1,res_scl2)

# ---- Test calls
# class(d_scl0); d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecA <- d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecB <- d_scl1[d_scl1$focus=="WHAT_Qn",]; d_selecC <- d_scl2[d_scl2$focus=="WHAT_Qn",]; d_selecA[10,]$cntrQ; dim(d_selecA %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecB %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecC %>% distinct(cntrQ,.keep_all=TRUE)) ; unique(d_selecB$cntrQ); dim(d_selecA[d_selecA$cntrQ==381,]); dim(d_selecB[d_selecB$cntrQ==381,]); dim(d_selecC[d_selecC$cntrQ==381,]); 
# boot_dTest <- boot(d_alt$diffA1,samplemean,10000); # boot_dEmpty <- boot(d_selecA$diff42,samplemean,10000); stdErrorTest <- sd(boot_dTest$t); stdErrorTest  # stdErrorTest <- boot_dTest$statistic(stderr())
# ciTest <- boot.ci(boot.out = boot_dTest, type = c("norm", "basic", "perc", "bca"));
# ci_Diff_scl0_scl2 <-bootQuestionsDifferences_unorthodox(d_scl0,d_scl2,"diffA1","WHAT_Qn")
# ci_Diff_scl0_scl2
# boot_diffA1_scl0 <-genBoot(d_scl0,"diffA1","WHAT_Qn")
# sum_boot_diffA1_scl0 <- getMean_lowCI_highCI((boot_diffA1_scl0))
# sum_boot_diffA1_scl0
# make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA3","WHAT_Ql")
# boot_diffA1_scl1 <-genBoot(d_scl1,"diffA1","WHAT_Qn")
# sum_boot_diffA1_scl1 <- getMean_lowCI_highCI((boot_diffA1_scl1))
# diffsTest <- getDifferencesBoot(boot_diffA1_scl0,boot_diffA1_scl1)
# diffsTest
# sum_boot_diffA1_scl0
# sum_boot_diffA1_scl1
# diffsTest
# genBoot(d_sclAll,"diffA2","WHAT_Ql","easy","H")
# boot(d_sclAll[["diffA3"]][d_sclAll$dMask == "medium"],samplemean,10000)
# boot(d_sclAll$diffA1[d_sclAll$dMask=="easy" & d_sclAll$dComplex_focus=="M" & d_sclAll$scaling==1 & d_sclAll$focus=="WHAT_Ql"],samplemean,10000)
# summBoot_scl_diffA1 <- make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA1","WHAT_Ql","","M") # works! # arrData <- list(d_scl0,d_scl1,d_scl2); groupedData_scl0; boots_diffA1_focus_WHAT_Qn <- list(scl_0=genBoot(d_scl0,"diffA1","WHAT_Qn"),scl_1=genBoot(d_scl1,"diffA1","WHAT_Qn"),scl_2=genBoot(d_scl2,"diffA1","WHAT_Qn")); boots_diffA1_focus_WHAT_Qn; as.data.frame(boots_diffA1_focus_WHAT_Qn); ggplot(as.data.frame(vals_scl0_fcs_WHAT_Qn),(aes(x=vals_scl0_fcs_WHAT_Qn[1],y=10))) + geom_point() +  geom_point(data=as.data.frame(vals_scl1_fcs_WHAT_Qn),colour='red') + xlim(-30, 30)
# summBoot_scl_diffA1
# d_s1 <- d_sclAll[d_sclAll$scaling==1,]
# dim(d_s1)
# d_sclAll[d_sclAll$info_focus_dComplex_dMask == "WHAT_Ql_M_E" & d_sclAll$focus=="WHAT_Ql" & d_sclAll$scaling==1,]
# d_sclAll[d_sclAll$focus=="WHAT_Ql" & d_sclAll$dMask=="easy" & d_sclAll$dComplex_focus == "M" & d_sclAll$scaling==1,]
# boot(d_sclAll$diffA1,samplemean,10000)
# bootQuestionsDifferences_unorthodox(d_scl0,d_scl1,"diffA1")
# groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,scaling=FALSE,distractor=TRUE,focus=TRUE,dMask=FALSE,dComplex_focus=TRUE)
# d_sclAll[d_sclAll$scaling==0 & d_sclAll$focus=="WHAT_Ql" & d_sclAll$dComplex_focus == "M",] 
# groupedData_all$mean_diffA3[groupedData_all$scaling==0 & groupedData_all$focus=="WHAT_Ql" & groupedData_all$dComplex_focus == "M"]
# groupedData_all <- generateGroupedData(d_sclAll)
# # dim(groupedData_all[groupedData_all$scaling == 0,])[1]; dim(groupedData_all[groupedData_all$scaling == 1,])[1]; dim(groupedData_all[groupedData_all$scaling == 2,])[1]
# groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll)
# groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,TRUE,FALSE,FALSE,TRUE,FALSE)
# ---- Data transform
# # d_scl0<- orderData(d_scl0); d_scl1 <- orderData(d_scl1); d_scl2 <- orderData(d_scl2);
# 
# # diffAx coded next to its plot
# 
# diffBoot_0_1 <- bootQuestionsDifferences_unorthodox(d_scl0,d_scl1,question="diffA1")
# diffBoot_0_2 <- bootQuestionsDifferences_unorthodox(d_scl0,d_scl2,question="diffA1")
# diffBoot_1_2 <- bootQuestionsDifferences_unorthodox(d_scl1,d_scl2,question="diffA1")
# dfTest <- data.frame();
# diffBoot_0_1 <- c(diffBoot_0_1 ,"diff scale 0 and 1",1)
# diffBoot_0_2<- c(diffBoot_0_2 ,"diff scale 0 and 2",2)
# diffBoot_1_2<- c(diffBoot_1_2 ,"diff scale 1 and 2",3)
# diffBoot_1_2[1] <- as.numeric(diffBoot_1_2[1])
# dfTest <- data.frame(diffBoot_0_1,diffBoot_0_2,diffBoot_1_2);
# dfTest <- data.frame(t(dfTest))
# dfTest <- rename(dfTest,mean_CI=X1)
# dfTest <- rename(dfTest,low_CI=X2)
# dfTest <- rename(dfTest,high_CI=X3)
# dfTest <- rename(dfTest,difference_name=X4)
# dfTest <- rename(dfTest,index=X5)
# # cbind(dfTest$mean_CI, apply(dfTest$mean_CI, length(dfTest$mean_CI),as.numeric) )
# dfTest <- convert_columns(dfTest,'character|logical','numeric')
# groupedPlotDiffA1 <- ggplot(dfTest, aes(x=mean_CI,y=difference_name)) +
#   geom_vline(xintercept = 0) +
#   geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#   geom_point(size=3,col="black",fill="white", shape=1) +
#   xlim(c(-5,5))
# groupedPlotDiffA1
# 
# 
# questions <- c("diffA1","diffA2","diffA3")
# diffsOfBoots_1_2_A1 <- bootQuestionsDifferences_unorthodox(d_scl0,d_scl1,question=questions[1])
# diffsOfBoots_1_3_A1 <- bootQuestionsDifferences_unorthodox(d_scl0,d_scl2,question=questions[1])
# diffsOfBoots_2_3_A1 <- bootQuestionsDifferences_unorthodox(d_scl1,d_scl2,question=questions[1])
# diffsOfBoots_1_2_A1 <- c(diffsOfBoots_1_2_A1, "diff scale 0 and 1")
# diffsOfBoots_1_3_A1 <- c(diffsOfBoots_1_3_A1, "diff scale 0 and 2")
# diffsOfBoots_2_3_A1 <- c(diffsOfBoots_2_3_A1, "diff scale 1 and 2")
# df_A1 <- data.frame(diffsOfBoots_1_2_A1,diffsOfBoots_1_3_A1,diffsOfBoots_2_3_A1);
# df_A1 <- data.frame(t(df_A1))
# df_A1 <- rename(df_A1,mean_CI=X1);df_A1 <- rename(df_A1,low_CI=X2);df_A1 <- rename(df_A1,high_CI=X3);df_A1 <- rename(df_A1,difference_name=X4)
# cols <- c("mean_CI","low_CI","high_CI");
# df_A1[,cols] <- lapply( df_A1[,cols],as.numeric)
# 



# differences_CI_diffA1_focus_traditional <- bootQuestionsDifferences_conservative(d_alt[d_alt$focus=="WHAT_Qn",],d_alt[d_alt$focus=="WHERE",],"diffA1")
# differences_CI_diffA1_focus_traditional
# differences_CI_diffA1_focus <- bootQuestionsDifferences_unorthodox(d_alt[d_alt$focus=="WHAT_Qn",],d_alt[d_alt$focus=="WHERE",],"diffA1")
# differences_CI_diffA1_focus
# factor1 <- "scaling"
# factor2 <- "focus"
# factor3 <- "dMask"
# factor4 <- "dComplex_focus"
# d_sclAll[factor1][d_sclAll[factor1] == "WHAT_Qn",]
# a <- (42>12)? "hello" : "goodbye"
# dfCI_global_scalingXfocus_dMask[1,]$orderCategoryCombination
# testFactors <- returnFactorsCombination(factorScaling=TRUE,factorDistractor=FALSE,factorFocus = TRUE,factorDMask = TRUE,factorDComplex_focus=TRUE)
# factor1 <- testFactors[1]; factor2 <- testFactors[2]; factor3 <- testFactors[3]; factor4 <- testFactors[4]
# cat(dim(d_sclAll[d_sclAll[factor1]=="0" & d_sclAll[factor2]=="WHAT_Qn" & d_sclAll[factor3]=="easy" & d_sclAll[factor4]=="E",]))
# selecTest <- d_sclAll[d_sclAll[factor1]=="0" & d_sclAll[factor2]=="WHAT_Qn" & d_sclAll[factor3]=="easy" & d_sclAll[factor4]=="E",]
# selecTest
# testFactors
# testFactors <- returnFactorsCombination()
# testFactors <- returnFactorsCombination(factorScaling=FALSE,factorFocus = TRUE, factorDMask = TRUE)
# testFactors
# length(testFactors)
# is.na(testFactors[2])
# d_sclAll <- renameGroupedData(d_sclAll)
# selecTest <- renameGroupedData(selecTest)
# groupedPlotCI_1 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor3)) +
#   geom_vline(xintercept = 0) +
#   geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#   geom_point(size=3,col="black",fill="white", shape=1) +
#   xlim(c(-absGraphEdge,absGraphEdge)) +
#   facet_wrap( as.formula(paste("~",factor1,"+",factor2)) , dir="v", ncol=1) +
#   ggtitle(strSentence)
# dfCI_global <- data.frame()
# dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;


# ---- formerly part of combine_genPlot_CIandDifferences
if (numFactor>3){
  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
  dfTest_CI <- NULL;
  dfTest_CI_differences <- NULL;
  if (length(arrFactorVariations)== 2){
    # cat("length(arrFactorVariations)== 2")
    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]                    
    
    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
    
    dfTest_CI <- data.frame(group1_CI,group2_CI);
    dfTest_CI_differences <- data.frame(group_differences1_CI);
  } 
  else {
    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]
    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3],]
    
    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
    
    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="") 
    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
    
    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
  }
  # is.numeric(dfTest_CI$mean_CI[2])
  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); 
  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
  dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
  
  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
  
  dfTest_CI$question <- i
  dfTest_CI_differences$question <- i
  
  cols <- c("mean_CI","low_CI","high_CI");
  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
  strSentence <- paste("Confidence intervals, ",curQuestion)
  dfCI_global <- rbind(dfCI_global, dfTest_CI)
  
  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)                  
} 




# error rate. 
combine_genPlot_ErrorRate_CIandDifferences_old <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorDifference="dMask", logFunction=FALSE){
  # d <- filter_allTrust0or5_impossibleQualAnswer(d)
  d$reverseB <- abs(d$correctB -1);
  
  factorVariation <- factorDifference
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
  if (factorScaling | factorDifference =="scaling" | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("reverseB");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor:length(arrFactorVariations)== ",numFactor)
  cat('\nfactorDifference: ',factorDifference)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  arrFactorVariations <- c()
  if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  arrFactorDifferences <- c()
  if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}  
  cat("\narrFactorDifferences: ",arrFactorDifferences)
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  dfCI_global_differences <- data.frame()
  dfCI_global_differences$mean_CI[0] <- 0; dfCI_global_differences$low_CI[0] <- 0;dfCI_global_differences$high_CI[0] <- 0;dfCI_global_differences$category_combination[0] <- 0; dfCI_global_differences$question[0] <- 0;
  
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor1=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor2=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor3=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor4=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  
  cat("\n about to loop arrQuestions. arrFactor1: ",arrFactor1,", arrFactor2: ",arrFactor2)
  # generations of boot according to the number of factors for each question
  for (i in arrQuestions){
    cat("\nloop questions. i: ",i)
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                  dfTest_CI <- NULL;
                  dfTest_CI_differences <- NULL;
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]                    
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3],]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="") 
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                  cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                } 
                else {
                  # numFactor == 3 # should be fine, a) but testing necessary b) adaptation in cases where there 
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  # TODO consider that there could potentially be only 2 selec, for a factor like distractor!
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                    
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="");
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="");
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="");
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              cat("\n !!! numFactor==2 ");
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              dfTest_CI_differences <- NULL
              if (length(arrFactorVariations)== 2){
                cat("\n !!! length(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                cat("\nmade selec_differences for selec1 and selec2")
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                cat("\ngroup1_CI: ",toString(group1_CI),", group2_CI: ",toString(group2_CI))
                
                if (toString(group1_CI) == "1" ){
                  cat("\n very wrong group1_CI but we try to set a small variation that won't be visible")
                  group1_CI <- c(0.999,0.998,1)
                }
                if (toString(group2_CI) == "1") {
                  cat("\n very wrong group2_CI but we try to set a small variation that won't be visible")
                  group2_CI <- c(0.999,0.998,1)
                }
                
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                cat("\n{{{ booted group_differences1_CI")
                if ( is.na( as.numeric(group1_CI[2] ) ) ){ 
                  cat("\n\n\n\t need to modify group1_CI, because it returned a single value. We will simply reproduce it")
                  group1_CI <- c(group1_CI[1],as.numeric(group1_CI[1]),as.numeric(group1_CI[1]),group1_CI[2],group1_CI[3])
                }
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI);
              } 
              else {
                # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
              }
              # is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
              
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI_differences[factor1] <- curFactor1; 
              dfTest_CI_differences[factor2] <- curFactor2;
              
              dfTest_CI$question <- i
              dfTest_CI_differences$question <- i
              
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
              
              # cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            dfTest_CI_differences <- NULL
            if (length(arrFactorVariations)== 2){
              cat("\n!!!length(arrFactorVariations)== 2, factor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", factorDifference: ",factorDifference)
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              cat("\ndim selec1: ", toString(dim(selec1)),", dim selec2: ", toString(dim(selec2)))
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
              
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              dfTest_CI <- data.frame(group1_CI,group2_CI);
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              dfTest_CI_differences <- data.frame(group_differences1_CI); 
            } 
            else {
              cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              selec_differences3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
              # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
              group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
              group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
              # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
              cat("\n{{{{group1_CI: ",group1_CI,", group2_CI: ",group2_CI,", group3_CI: ",group3_CI,"\t\n")
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
              group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
              cat("\n{{{{group_differences1_CI: ",group_differences1_CI,", group_differences2_CI: ",group_differences2_CI,", group_differences3_CI: ",group_differences3_CI,"\t\n")
              
              # cat("\nand added the strings. Might be a typo in all the cases of this code...")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
            }
            # is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4); dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
            dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
            cat("\n||||||||",toString(head(dfTest_CI_differences)))
            if (any(grepl('X4', colnames(dfTest_CI_differences)))){
              dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
            }
            dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences$question <- i;
            
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Differences of confidence intervals, ",curQuestion)
            dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
            
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
      
    }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorVariation can be distractor
      dfTest_CI <- NULL
      dfTest_CI_differences <- NULL
      
      if (length(arrFactorVariations)== 2){
        cat("length(arrFactorVariations)== 2, factorVariation: ",factorVariation,", factorDifference: ",factorDifference,", arrFactorVariations[1]: ",arrFactorVariations[1],", arrFactorDifferences[1]: ",arrFactorDifferences[1])
        cat("\nlength selec1: ",length(d[factorDifference]==arrFactorDifferences[1])," length selec2: ",length(d[factorDifference]==arrFactorDifferences[2]))
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        cat("\n__selec1, selec2, selec_differences1 and selec_differences2 made. Also, curQuestion is: ",curQuestion);
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        cat("\n__group1_CI and group2_CI");
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
        cat("\n__bootQuestionsDifferences_conservative passed");
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI);
      } 
      else {
        cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", curQuestion: ",curQuestion)
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        selec_differences3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
        
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
        group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
        group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
        
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
        group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
      }
      dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);dfTest_CI$question <- i
      # cat("\n****length(dfTest_CI$question): ",length(dfTest_CI$question))
      # cat("\n****length(dfTest_CI_differences$question): ",length(dfTest_CI_differences$question))
      # dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
      dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
      cat("\n||||||||",toString(head(dfTest_CI_differences)))
      if (any(grepl('X4', colnames(dfTest_CI_differences)))){
        dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
      }
      dfTest_CI_differences$question <- i
      
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Differences of confidence intervals, ",curQuestion)
      dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
      
      cat("\ngenerated the data to display, factorVariation-",factorVariation)
    }
  }
  
  # we should have the dfCI_global loaded now, but still need to display it.
  cat("\n####about to draw")
  cat("\nwhat of the global structure variations... ",dim(dfCI_global),", and their questions: ",length(dfCI_global$question));
  cat("\nwhat of the global structure differences... ",dim(dfCI_global_differences),", and their questions: ",length(dfCI_global_differences$question));
  
  class(dfCI_global$category_combination)
  class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
  class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
  class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
  class(dfCI_global_differences$category_combination);
  class(dfCI_global_differences$mean_CI); dfCI_global_differences$mean_CI <- as.numeric(dfCI_global_differences$mean_CI); class(dfCI_global_differences$mean_CI);
  class(dfCI_global_differences$low_CI); dfCI_global_differences$low_CI <- as.numeric(dfCI_global_differences$low_CI); class(dfCI_global_differences$low_CI);
  class(dfCI_global_differences$high_CI); dfCI_global_differences$high_CI <- as.numeric(dfCI_global_differences$high_CI); class(dfCI_global_differences$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global);
  dfCI_global_differences <- renameGroupedData(dfCI_global_differences);
  
  cat("\ndid the renaming mess up dimensions of dfCI_global: ",dim(dfCI_global),", and how many items in question? ",length(dfCI_global$question));
  cat("\nand what about dfCI_global_differences: ",dim(dfCI_global_differences),", and how many items in question? ",length(dfCI_global_differences$question));
  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  minLow_cI_differences <- max(abs(dfCI_global_differences$low_CI));maxHigh_CI_differences <- max(abs(dfCI_global_differences$high_CI)); edgeSize_differences <- max(0.1+abs(minLow_cI_differences),0.1+abs(maxHigh_CI_differences)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize,",minLow_cI_differences: ",minLow_cI_differences,", maxHigh_CI_differences: ",maxHigh_CI_differences,",edgeSize_differences: ",edgeSize_differences)
  
  # cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
  cat("\n^^^^what's the length of question for dfCI_global now? ",length(dfCI_global$question))
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
    
  }
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  } 
  # TODO fix the question missing in dfCI_global when numFactor == 0
  groupedPlotCI_1 <- NULL; groupedPlotCI_2 <- NULL;groupedPlotCI_3<- NULL;
  group_differencesedPlotCI_1<-NULL;group_differencesedPlotCI_2<-NULL;group_differencesedPlotCI_3<-NULL;
  dfCI_global_differences <- addInfoCiDifferenceSignificant(dfCI_global_differences)
  cat("\n:::: dfCI_global colNames: ",colnames(dfCI_global))
  cat("\n:::: dfCI_global_differences colNames: ",colnames(dfCI_global_differences))
  
  strTitleTotal <- NULL;
  if (numFactor!=0){ 
    strTitleTotal <- paste("Error rates and differences for ",factorDifference,", factored by ",toString(factorArr),sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax= high_CI )) + #seriously concerned with this
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(  min(0, min(dfCI_global$low_CI) ) , max(1, max(dfCI_global$high_CI)  ) ) ) + 
      ggtitle("Error Rate") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+ 
      labs(title = 'Error Rate', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
      )
    # 
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax= high_CI )) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Error Rate") +
      labs(title = 'Differences for Error Rate', y = "" ) +
      theme(
        # strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") 
    
  } 
  else {
    cat("\n))))numFactor==0. dim(dfCI_global):  ",dim(dfCI_global) )
    strTitleTotal <- paste("Error rates and differences for ",factorDifference,sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(0,1)) + 
      ggtitle("Error Rate")+
      theme(
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      ) +
      labs(title = "Error Rate",y="")
    # 
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Error Rate") +
      labs(title = 'Differences for Error Rate', y = "" ) +
      theme(
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      )
    
  }
  
  cat("\nThe plots are generated. But are they fine?\n")
  grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_differences1), ncol=2,top=textGrob( strTitleTotal ) )
  # cat("not sure what to return")
  return (dfCI_global)
}


# dfCI_errorRate_B_differences_focus_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorDifference = "focus")
# dfCI_errorRate_B_differences_dMask_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_all, factorDifference = "dMask")

# dfCI_errorRate_B_differences_dMask_factoredby_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dMask")
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDComplex_focus = TRUE, factorDifference = "dMask")
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus

# # this combination is buggy!
# dfCI_errorRate_B_differences_focus_factoredby_scaling <- combine_genPlot_ErrorRate_CIandDifferences(d_sclFiltered, factorScaling =TRUE, factorDifference = "focus")
# # d_sclFiltered[d_sclFiltered$focus=="WHAT_Ql" & d_sclFiltered$scaling==2,]
# dfCI_errorRate_B_differences_scaling_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_sclFiltered, factorScaling =FALSE, factorDifference = "scaling")
# dfCI_errorRate_B_differences_distractor_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_distrFiltered, factorScaling =FALSE, factorDifference = "distractor")

# dfCI_differences_distractor_noFactor <- combine_genPlot_CIandDifferences(d_distrFiltered, factorScaling =FALSE, factorDifference = "distractor")

# bug with this combination (probably an issue with rename function)
# dfCI_differences_dComplex_focus_factoredby_focus <- combine_genPlot_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dComplex_focus")
# dfCI_differences_dMask_factoredby_focus <- combine_genPlot_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dMask")
# 
# # TODO!!! function that makes those graphs without making factorDifference and factorVariations
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDComplex_focus = TRUE, factorDifference = "")
# # TODO potentially look at reordering?!


# strFormulaTest <- "~ orderMaskComplex"
# ggplot(dfCI_errorRate_B_test2, aes(x=mean_CI,y=orderCategoryCombination)) +
#   geom_vline(xintercept = 0) +
#   geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#   geom_point(size=3,col="black",fill="white", shape=1) # +
#   # facet_wrap( as.formula(strFormulaTest) , dir="v", ncol=1)


