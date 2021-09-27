

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

