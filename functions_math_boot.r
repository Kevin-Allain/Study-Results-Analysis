library(gridExtra)
library(grid)
library(boot) 
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lattice)
library(scales)
library(cowplot)
library(patchwork)
library(stringr)
library(rlist)
# library(simpleaffy)
library(rlang)
library(skimr)
library(agricolae)
# library(plotly) # not our version?!


setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")

samplemean <- function(x, d) {
  return(mean(x[d]))
}
orderData <- function(d){
  d$dMask <- factor(d$dMask, levels=c("easy", "medium", "hard"))
  d$dComplex_focus <- factor(d$dComplex_focus, levels=c("E", "M", "H"))
  return (d)
}
allSame <- function(x) length(unique(x)) == 1

genBoot <- function(d,question,focus="",dMask="",dComplex_focus="",R=10000){
  # cat("\ngenboot: question: ",question,", focus: ", focus," dMask: ", dMask,", dComplex_focus: ", dComplex_focus) # cat("\n\t\t\t\td[[question]]: ",d[[question]])
  boot_d <- c();
  if (focus=="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]],samplemean,R);
  } else if (focus=="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$dMask == dMask],samplemean,R);
  } else if (focus=="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus],samplemean,R);
  } else if (focus=="" & dMask != "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus & d$dMask == dMask],samplemean,R);
  } else if (focus!="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus],samplemean,R);
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask],samplemean,R);
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dComplex_focus == dComplex_focus],samplemean,R);
  } else {
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask & d$dComplex_focus == dComplex_focus],samplemean,R);
  }
  return (boot_d);
}

getMean_lowCI_highCI <- function (boot_d){
  # cat("\n--getMean_lowCI_highCI--")
  ci <- boot.ci(boot.out = boot_d, type = c("norm", "basic", "perc", "bca"));
  # cat("\nci: ",toString(ci))
  mean <- boot_d$t0;
  l_ci <- ci$normal[2];
  h_ci <- ci$normal[3];
  res <- c(mean,l_ci,h_ci)
  # cat("\n in getMean_lowCI_highCI, mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  return (res);
}

make_gensMean_lowCI_highCI <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  # cat("\n-- make_gensMean_lowCI_highCI; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus,", dim(d): ",dim(d));
  if (dim(d)[1] == 0){
    return (NA)
  } else {
    # boot
    boot_s0 <- genBoot(d,question,focus,dMask,dComplex_focus,R)
    # cat("\nboot_s0: ",toString(boot_s0));
    # call the summary
    gens_s0 <- getMean_lowCI_highCI(boot_s0)
    return( c(gens_s0) )
  }
}

make_gensMean_trustBased <- function (d, question, focus="",dMask="",dComplex_focus="",R=10000){
  boot_s0 <- genBoot_trust(d,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  return( c(gens_s0) )
}

make_gensMean_lowCI_highCI_sclDependent <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  # cat("\n-- make_gensMean_lowCI_highCI_sclDependent; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
  # select the data with scale 0
  d_s0 <- d[d$scaling==0,]
  # boot
  boot_s0 <- genBoot(d_s0,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  # select the data with scale 1
  d_s1 <- d[d$scaling==1,]
  # boot
  boot_s1 <- genBoot(d_s1,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s1 <- getMean_lowCI_highCI(boot_s1)
  # select the data with scale 2
  d_s2 <- d[d$scaling==2,]
  # boot
  boot_s2 <- genBoot(d_s2,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s2 <- getMean_lowCI_highCI(boot_s2)
  return( c(gens_s0,gens_s1,gens_s2) )
}

make_gensMean_lowCI_highCI_distractorDependent <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  # cat("\n-- make_gensMean_lowCI_highCI_distractorDependent; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
  # select the data with distractor h
  d_s0 <- d[d$distractor=="h",]
  # boot
  boot_s0 <- genBoot(d_s0,question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  # select the data with distractor n
  d_s1 <- d[d$distractor=="n",]
  # boot
  boot_s1 <- genBoot(d_s1,question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R)
  # call the summary
  gens_s1 <- getMean_lowCI_highCI(boot_s1)
  return( c(gens_s0,gens_s1) )
}

bootQuestionsDifferences_conservative <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  if (focus=="" & dMask=="" & dComplex_focus==""){
    boot_d <- boot(d[[question]],samplemean,R)
    boot_d2 <- boot(d2[[question]],samplemean,R)
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$dMask==dMask],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$dMask==dMask],samplemean,R)
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus==dComplex_focus],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$dComplex_focus==dComplex_focus],samplemean,R)
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus],samplemean,R)
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus==focus],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$focus==focus],samplemean,R)
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus==focus & d$dMask==dMask],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dMask==dMask],samplemean,R)
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
  } else {
    boot_d <- boot(d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
    boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
  }
  
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  # sumAbsDiffs <- c(abs(sumD[1]-sumD2[1]), abs(sumD[2]-sumD2[2]), abs(sumD[3]-sumD2[3]) ) # Important note: doubt about the point in using an absolute value... Probably better not to.
  sumAbsDiffs <- c(sumD[1]-sumD2[1], sumD[2]-sumD2[2], sumD[3]-sumD2[3] )  
  return (sumAbsDiffs)
}

# This approach is the same as Pena-Araya. (They don't use the logFunction)
bootQuestionsDifferences_directSubstract <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000, logFunction = FALSE ) {
  boot_d <- c();boot_d2 <- c();
  dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    dSelect1 <- d[[question]]
    dSelect2 <- d2[[question]]
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]
    dSelect2 <- d2[[question]][d2$dMask==dMask]
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus]
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus]
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]
    dSelect2 <- d2[[question]][d2$focus==focus]
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask]
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus]
  } else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus]
  }
  
  # To deal with selections of different sizes, we use the smallest It means loss of data but at least results are true (letting R loop over is wrong)...
  minSelecLength <- min(length(dSelect1),length(dSelect2));
  # cat("\nminSelecLength: ",minSelecLength,", length(dSelect1): ",length(dSelect1),", length(dSelect2): ",length(dSelect2), ', min(length(dSelect1),length(dSelect2)): ',min(length(dSelect1),length(dSelect2)))
  if (!logFunction){
    diffSelec <- dSelect1[1:minSelecLength] - dSelect2[1:minSelecLength];
  } 
  else {
    diffSelec <- log2( abs( dSelect1[1:minSelecLength] - dSelect2[1:minSelecLength] ) +1/8 ) # Cleveland and Gills approach
  }
  # cat("\nlength of diffSelec: ",length(diffSelec)) # diffSelec <- dSelect1 - dSelect2;
  bootDiff <- boot(diffSelec,samplemean,R)
  # structureD <- make_gensMean_lowCI_highCI(d=diffSelec,question=question,R=R);
  
  # sumAbsDiffs <- c(structureD[1], structureD[2], structureD[3])
  res <- getMean_lowCI_highCI(bootDiff)
  # cat("\n~~bootQuestionsDifferences_directSubstract res: ",toString(res))
  return (res)
}

# not used in the end...
bootQuestionsDifferences_TukeyHSD <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000){
  
  a1_measurement_dMask <- aov(correctB ~ dMask, data = d_measurement_filtered)
  
  boot_d <- c();boot_d2 <- c();
  dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    dSelect1 <- d[[question]]
    dSelect2 <- d2[[question]]
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]
    dSelect2 <- d2[[question]][d2$dMask==dMask]
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus]
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus]
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]
    dSelect2 <- d2[[question]][d2$focus==focus]
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask]
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus]
  } else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]
    dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus]
  }
  
  diffSelec <- dSelect1 - dSelect2;
  structureD <- make_gensMean_lowCI_highCI(d=diffSelec,question=question,R=R);
  
  sumAbsDiffs <- c(structureD[1], structureD[2], structureD[3])
  
  return (sumAbsDiffs)
}



getDifferencesBoot <- function (boot_d,boot_d2){
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  sumAbsDiffs <- c(abs(sumD[1]-sumD2[1]), abs(sumD[2]-sumD2[2]), abs(sumD[3]-sumD2[3]) )
  return (sumAbsDiffs)
}

# we need a function to display differences between scaling groups...! With confidence intervals... get the values according to groups, make boot for each group, make the sample proportion, and then put it as the middle
# https://online.stat.psu.edu/stat100/lesson/9/9.3
bootQuestionsDifferences_unorthodox <- function(d,d2,question="",focus="",dMask="",dComplex_focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  sampleSize <- -1; sampleSize2 <- -1;
  
  if (focus=="" & dMask == "" & dComplex_focus ==""){
    sampleSize <- length(d[[question]]); 
    sampleSize2 <- length(d2[[question]]); 
  } 
  else if (focus=="" & dMask != "" & dComplex_focus ==""){
    sampleSize <- length(d[[question]][d$dMask == dMask]); 
    sampleSize2 <- length(d2[[question]][d$dMask == dMask]); 
  } 
  else if (focus=="" & dMask == "" & dComplex_focus !=""){
    sampleSize <- length(d[[question]][d$dComplex_focus == dComplex_focus]); 
    sampleSize2 <- length(d2[[question]][d$dComplex_focus == dComplex_focus]); 
  } 
  else if (focus=="" & dMask != "" & dComplex_focus !=""){
    sampleSize <- length(d[[question]][d$dComplex_focus == dComplex_focus & d$dMask == dMask]); 
    sampleSize2 <- length(d2[[question]][d2$dComplex_focus == dComplex_focus & d2$dMask == dMask]); 
  } 
  else if (focus!="" & dMask == "" & dComplex_focus ==""){
    sampleSize <- length(d[[question]][d$focus]);
    sampleSize2 <- length(d2[[question]][d2$focus]);
  } 
  else if (focus!="" & dMask != "" & dComplex_focus ==""){
    sampleSize <- length(d[[question]][d$focus == focus & d$dMask == dMask]); 
    sampleSize2 <- length(d2[[question]][d2$focus == focus & d2$dMask == dMask]);    
  } 
  else if (focus!="" & dMask == "" & dComplex_focus !=""){
    sampleSize <- length(d[[question]][d$focus == focus & d$dComplex_focus == dComplex_focus]); 
    sampleSize2 <- length(d2[[question]][d2$focus == focus & d2$dComplex_focus == dComplex_focus]);    
  } 
  else {
    sampleSize <- length(d[[question]][d$focus == focus & d$dComplex_focus == dComplex_focus & d$dMask == dMask]); 
    sampleSize2 <- length(d2[[question]][d2$focus == focus & d2$dComplex_focus == dComplex_focus & d2$dMask == dMask]);
  }
  
  if (sampleSize == 0 & sampleSize2 == 0){ return (-1) }
  else if (sampleSize == 0) {
    boot_d2 <- genBoot(d2,question,focus,dMask,dComplex_focus,R)
    return (boot_d2);    
  } 
  else if (sampleSize2 == 0) {
    boot_d <- genBoot(d,question,focus,dMask,dComplex_focus,R)
    return (boot_d)
  } 
  else {
    boot_d <- genBoot(d,question,focus,dMask,dComplex_focus,R)
    boot_d2 <- genBoot(d2,question,focus,dMask,dComplex_focus,R)
    # calculate the differences and sample proportions...
    sumD <- getMean_lowCI_highCI(boot_d)
    sumD2 <- getMean_lowCI_highCI(boot_d2)
    meanDiff <- sumD[1]-sumD2[1]
    # print(meanDiff)
    stdErr_d <- -1;stdErr_d2 <- -1
    stdErr_d <- sd(boot_d$t);stdErr_d2 <- sd(boot_d2$t);
    # print(stdErr_d)
    SEM_d <- stdErr_d/sqrt(sampleSize); SEM_d2 <- stdErr_d2/sqrt(sampleSize2);
    # print(SEM_d);print(SEM_d2);
    std_Error_Difference <- sqrt(SEM_d*SEM_d + SEM_d2*SEM_d2)
    res <- c(meanDiff, meanDiff - std_Error_Difference, meanDiff + std_Error_Difference)
    return (res)
  }
}


testDistribReal <- function(d_measurement_all_noTikTok_filteredSemiRigorous, strFormula = "~focus+dMask"){
  groupedPlotCI_3 <- ggplot(d_measurement_all_noTikTok_filteredSemiRigorous, aes(x=log_diffA3,y=dComplex_focus, show.legend = FALSE )) +
    geom_vline(xintercept = -3) +
    geom_violin( aes (x= log_diffA3 , y = dComplex_focus,alpha = 0.3), show.legend = FALSE ) +
    geom_point(  aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +
    xlim(c(-3,6)) +
    ggtitle("Test display responses log_diffA3") +
    facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
    labs(title = 'The test display responses log_diffA3', y = "" ) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none") + 
    guides(fill = FALSE) + 
    guides(col = FALSE)

  groupedPlotCI_3
}
