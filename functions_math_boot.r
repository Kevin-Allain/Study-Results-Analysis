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
library(GGally, quietly = TRUE)

# library(plotly) # not our version?!

# adapt this section according to the computer ran on (potential TODO adapt according to current posiiton)
# setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
setwd("C:/Users/keval/Documents/Programming/R/Study-Results-Analysis")

samplemean <- function(x, d) {
  return(mean(x[d]))
}
orderData <- function(d){
  if (!is.null(d$dMask)){d$dMask <- factor(d$dMask, levels=c("easy", "medium", "hard"));}
  if (!is.null(d$dComplex_focus)){d$dComplex_focus <- factor(d$dComplex_focus, levels=c("E", "M", "H"));}
  if (!is.null(d$focus)){d$focus <- factor(d$focus, levels=c("WHAT_Ql", "WHAT_Qn", "WHERE"));}
  if (grepl("Focus ",d$dComplex_focus[1], fixed=TRUE)){d$dComplex_focus <- factor(d$dComplex_focus, levels=c("Focus Easy", "Focus Medium", "Focus Hard"));}
  if (!is.null(d$orderMaskComplex)){ d$orderMaskComplex <- factor(d$orderMaskComplex, levels=c("Mask Easy", "Mask Medium", "Mask Hard"));}
  if (!is.null(d$orderFocusComplex)){d$orderFocusComplex <- factor(d$orderFocusComplex, levels=c("Focus Easy", "Focus Medium", "Focus Hard"));}
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
  cat("\n--getMean_lowCI_highCI--")
  ci <- boot.ci(boot.out = boot_d, type = c("norm", "basic", "perc", "bca"));
  cat("\nci: ",toString(ci))
  mean <- boot_d$t0;
  l_ci <- ci$normal[2];
  h_ci <- ci$normal[3];
  res <- c(mean,l_ci,h_ci)
  cat("\n in getMean_lowCI_highCI, mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
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
    boot_d <- boot(d[[question]],samplemean,R); boot_d2 <- boot(d2[[question]],samplemean,R);
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$dMask==dMask],samplemean,R); boot_d2 <- boot(d2[[question]][d2$dMask==dMask],samplemean,R);
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus==dComplex_focus],samplemean,R); boot_d2 <- boot(d2[[question]][d2$dComplex_focus==dComplex_focus],samplemean,R);
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R); boot_d2 <- boot(d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus],samplemean,R);
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus==focus],samplemean,R); boot_d2 <- boot(d2[[question]][d2$focus==focus],samplemean,R);
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus==focus & d$dMask==dMask],samplemean,R); boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dMask==dMask],samplemean,R);
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R); boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R);
  } else {
    boot_d <- boot(d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R); boot_d2 <- boot(d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R);
  }
  
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  # sumAbsDiffs <- c(abs(sumD[1]-sumD2[1]), abs(sumD[2]-sumD2[2]), abs(sumD[3]-sumD2[3]) ) # Important note: doubt about the point in using an absolute value... Probably better not to.
  sumAbsDiffs <- c(sumD[1]-sumD2[1], sumD[2]-sumD2[2], sumD[3]-sumD2[3] )  
  return (sumAbsDiffs)
}

# This approach is the same as Pena-Araya. (They don't use the logFunction)
bootQuestionsDifferences_directSubstract <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000, logFunction = FALSE ) {
  cat("\nlength(d): ",length(d),", length(d2): ",length(d2),", question: ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus)
  
  boot_d <- c();boot_d2 <- c(); dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    # cat('\nfocus=="" & dMask=="" & dComplex_focus==""')
    dSelect1 <- d[[question]]; dSelect2 <- d2[[question]];
  } 
  else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]; dSelect2 <- d2[[question]][d2$dMask==dMask];
  } 
  else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus];
  } 
  else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus];
  } 
  else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]; dSelect2 <- d2[[question]][d2$focus==focus];
  } 
  else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask];
  } 
  else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus];
  } 
  else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus];
  }
  
  # cat("\nin the bootQuestionsDifferences_directSubstract function. question: ",question,", focus:",focus ,", dSelect1: ",dSelect1,", dSelect2: ",dSelect2)
  
  # To deal with selections of different sizes, we use the smallest It means loss of data but at least results are true (letting R loop over is wrong)...
  minSelecLength <- min(length(dSelect1),length(dSelect2));
  if (!logFunction){
    diffSelec <- dSelect1[1:minSelecLength] - dSelect2[1:minSelecLength];
  }
  else {
    diffSelec <- log2( abs( dSelect1[1:minSelecLength] - dSelect2[1:minSelecLength] ) +1/8 ) # Cleveland and Gills approach
  }
  
  # cat("\ndiffSelec: ",diffSelec)
  
  if(all(diffSelec==0) || all(diffSelec==1)){
    diffSelec[1]<- 0.00000001
    # cat("\nnew diffSelec: ",diffSelec)
  }
  
  bootDiff <- boot(diffSelec,samplemean,R)
  
  # cat("\nin the bootQuestionsDifferences_directSubstract function, bootDiff[[1]]: ",bootDiff[[1]])
  
  res <- getMean_lowCI_highCI(bootDiff)
  return (res)
}


# Randomizing the differences
bootQuestionsDifferences_directSubstract_random_differences <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000, logFunction = FALSE ) {
  boot_d <- c();boot_d2 <- c(); dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    dSelect1 <- d[[question]]; dSelect2 <- d2[[question]];
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]; dSelect2 <- d2[[question]][d2$dMask==dMask];
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus];
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus];
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]; dSelect2 <- d2[[question]][d2$focus==focus];
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask];
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus];
  } else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus];
  }
  
  cat("\nin the bootQuestionsDifferences_directSubstract_random_differences function, dSelect1: ",dSelect1,", dSelect2: ",dSelect2)
  
  # To deal with selections of different sizes, we use the smallest It means loss of data but at least results are true (letting R loop over is wrong)...
  # maxSelecLength <- max(length(dSelect1),length(dSelect2));
  dSelect1 <- sample(dSelect1)
  dSelect2 <- sample(dSelect2)
  if (!logFunction){
    diffSelec <- dSelect1 - dSelect2;
  }
  else {
    diffSelec <- log2( abs( dSelect1 - dSelect2 ) +1/8 ) # Cleveland and Gills approach
  }
  
  cat("\ndiffSelec: ",diffSelec)
  if(all(diffSelec==0) || all(diffSelec==1)){
    diffSelec[1]<- 0.00000001
    cat("\nnew diffSelec: ",diffSelec)
  }
  
  bootDiff <- boot(diffSelec,samplemean,R)
  
  cat("\nin the bootQuestionsDifferences_directSubstract function, bootDiff[[1]]: ",bootDiff[[1]])
  
  res <- getMean_lowCI_highCI(bootDiff)
  return (res)
}


# Squaring the approach of Pena-Araya.
bootQuestionsDifferences_directSubstract_squared <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000, logFunction = FALSE ) {
  boot_d <- c();boot_d2 <- c(); dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    dSelect1 <- d[[question]]; dSelect2 <- d2[[question]];
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]; dSelect2 <- d2[[question]][d2$dMask==dMask];
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus];
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus];
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]; dSelect2 <- d2[[question]][d2$focus==focus];
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask];
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus];
  } else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus];
  }
  
  # To deal with selections of different sizes, we use the smallest It means loss of data but at least results are true (letting R loop over is wrong)...
  minSelecLength <- min(length(dSelect1),length(dSelect2));
  diffSelec_squared <- c()
  if (!logFunction){
    for (diff1 in dSelect1) {
      for (diff2 in dSelect2) {
        diffSelec_squared <- append(diffSelec_squared, diff1-diff2)
      }
    }
  }
  else {
    for (diff1 in dSelect1) {
      for (diff2 in dSelect2) {
        diffSelec_squared <- append(diffSelec_squared, log2( abs( diff1 - diff2 ) +1/8 )) # Cleveland and Gills approach)
      }
    }
  }
  cat("\nsize of list 1: ",length(dSelect1),", size of list 2: ",length(dSelect2),", size of squared list: ",length(diffSelec_squared))
  bootDiff <- boot(diffSelec_squared,samplemean,R)
  
  res <- getMean_lowCI_highCI(bootDiff)
  return (res)
}



# not used in the end... probably entirely wrong! selec1 - selec2 can result in groups with different lengths...
bootQuestionsDifferences_TukeyHSD <- function(d,d2,question,focus="",dMask="",dComplex_focus="",R=10000){
  # a1_measurement_dMask <- aov(correctB ~ dMask, data = d)
  boot_d <- c();boot_d2 <- c(); dSelect1 <- NULL; dSelect2 <- NULL;
  if (focus=="" & dMask=="" & dComplex_focus==""){
    dSelect1 <- d[[question]]; dSelect2 <- d2[[question]];
  } else if (focus =="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$dMask==dMask]; dSelect2 <- d2[[question]][d2$dMask==dMask];
  } else if (focus =="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dComplex_focus==dComplex_focus];
  } else if (focus =="" & dMask != "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$dMask==dMask &d$dComplex_focus==dComplex_focus];
  } else if (focus !="" & dMask == "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus]; dSelect2 <- d2[[question]][d2$focus==focus];
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask];
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    dSelect1 <- d[[question]][d$focus==focus & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dComplex_focus==dComplex_focus];
  } else {
    dSelect1 <- d[[question]][d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus]; dSelect2 <- d2[[question]][d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus];
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


addCountColumn <- function (d, strFormula="~focus+dMask", factorVariation = "dComplex_focus"){
  arr_focus <- c("WHAT_Ql","WHAT_Qn","WHERE"); arr_dMask <- c("easy","medium","hard"); arr_dComplex_focus <- c("E","M","H"); arr_scaling <- c(0,1,2); arr_distractor <- c("h","n");
  arr_orderMaskComplex <- c("Mask Easy","Mask Medium","Mask Hard"); arr_orderFocusComplex <- c("Focus Easy","Focus Medium","Focus Hard")
  arr_reverseB <- c(0,1)
  # get the factors from formula, and last one is made by distractor...
  
  # # for testing...
  # arrFactor1 <- arr_dMask; arrFactor2 <- arr_focus; arrVariation <- arr_dComplex_focus;
  # factor1<- "dMask"; factor2 <- "focus"; variant <- "dComplex_focus";
  
  variant <- factorVariation
  
  strFormula <- as.character(str_remove_all(strFormula," "));
  tildePresence <- grepl("~" ,strFormula, fixed=TRUE); # can we ever finish without the tilde in strFormula? Don't think so.
  plusPresence <- grepl("+" ,strFormula, fixed=TRUE);
  # if + it means there are two factors.
  strFormula <- substring(strFormula, first =2);
  cat("\nstrFormula: ",strFormula,", factorVariation: ",factorVariation,", variant: ",variant);
  splitFormula <-  strsplit(as.character(strFormula), "+", fixed=TRUE);
  # cat("\nsplitFormula: ",toString(splitFormula))
  
  if (factorVariation == "focus"){arrVariation <- arr_focus} else if (factorVariation == "dMask"){ arrVariation <- arr_dMask } else if (factorVariation == "dComplex_focus"){arrVariation <- arr_dComplex_focus}
  if (factorVariation == "orderFocusComplex"){arrVariation <- arr_orderFocusComplex} else if (factorVariation == "orderMaskComplex"){arrVariation <- arr_orderMaskComplex}
  if (factorVariation == "distractor"){arrVariation <- arr_distractor} else if (factorVariation == "scaling"){arrVariation <- arr_scaling}
  if (factorVariation == "reverseB"){arrVariation <- arr_reverseB}
  
  if (nchar(strFormula) > 0){
    factor1 <- splitFormula[[1]][1]
    if (factor1 == "focus"){arrFactor1 <- arr_focus} else if (factor1 == "dMask"){ arrFactor1 <- arr_dMask } else if (factor1 == "dComplex_focus"){arrFactor1 <- arr_dComplex_focus}
    if (factor1 == "orderFocusComplex"){arrFactor1 <- arr_orderFocusComplex} else if (factor1 == "orderMaskComplex"){arrFactor1 <- arr_orderMaskComplex}
    
    if(!plusPresence){
      factor2 <- factor1; arrFactor2 <- arrFactor1;
    } 
    else {
      factor2 <- splitFormula[[1]][2];
      # cat("\nfactor2: ",factor2)
      if (factor2 == "focus"){arrFactor2 <- arr_focus} else if (factor2 == "dMask"){ arrFactor2 <- arr_dMask } else if (factor2 == "dComplex_focus"){arrFactor2 <- arr_dComplex_focus}
      if (factor2 == "orderFocusComplex"){arrFactor2 <- arr_orderFocusComplex} else if (factor2 == "orderMaskComplex"){arrFactor2 <- arr_orderMaskComplex}
    }
    
  } else {
    factor1 <- factorVariation;factor2 <- factorVariation; arrFactor1 <- arrVariation; arrFactor2 <- arrVariation;
  }
  
  cat("\naddCountColumn--\nstrFormula: ",strFormula,", plusPresence: ",plusPresence,", factor1: ",factor1,", arrFactor1: ",arrFactor1,", factor2: ",factor2,", arrFactor2: ",arrFactor2,", factorVariation: ",factorVariation,", arrVariation: ",arrVariation)
  cat("\nlength(d$log_diffA3[d[[",factor1, "]]== ",arrFactor1[1]," & d[[ ",factor2," ]]== ",arrFactor2[1]," & d[[",variant,"]]==",arrVariation[1],"]): ", 
      length(d$log_diffA3[ d[[factor1]]==arrFactor1[1] & d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]]) )
  cat("\nlength(d$log_diffA3[d[[",factor1, "]]== ",arrFactor1[1]," & d[[ ",factor2," ]]== ",arrFactor2[1]," & d[[",variant,"]]==",arrVariation[2],"]): ", 
      length(d$log_diffA3[ d[[factor1]]==arrFactor1[1] & d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]]) )
  
  d$countFactor <- c();
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[2]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[1]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[2]])
  d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]] <-  length(d$log_diffA3[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[3] & d[[variant]]==arrVariation[3]])
  
  # cat("\nd$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]]: ",d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[1]])
  # cat("\nd$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]]: ",d$countFactor[d[[factor1]]==arrFactor1[1]&d[[factor2]]==arrFactor2[2] & d[[variant]]==arrVariation[3]])
  # cat("\nd$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]]: ",d$countFactor[d[[factor1]]==arrFactor1[3]&d[[factor2]]==arrFactor2[1] & d[[variant]]==arrVariation[2]])
  # cat("\nd$countFactor:", toString(d$countFactor))
  return (d)
}

testDistribReal <- function(d_measurement_all_noTikTok_filteredSemiRigorous, strFormula = "~focus+dMask"){
  
  factorDifference <- "dComplex_focus"
  d_measurement_all_noTikTok_filteredSemiRigorous <- addCountColumn(d_measurement_all_noTikTok_filteredSemiRigorous, strFormula = strFormula, factorVariation = factorDifference)
  
  
  groupedPlotCI_3 <- ggplot(d_measurement_all_noTikTok_filteredSemiRigorous, aes(x=log_diffA3,y=as.factor(factorDifference), show.legend = TRUE )) +
    geom_vline(xintercept = -3) +
    geom_violin(data=d_measurement_all_noTikTok_filteredSemiRigorous, aes (x= log_diffA3 , y =  as.factor(factorDifference) ), alpha = 0.5, show.legend = TRUE ) +
    geom_beeswarm(data=d_measurement_all_noTikTok_filteredSemiRigorous, aes(alpha=0.5), cex=3, color="blue" , fill="blue", priority = "density", groupOnX = FALSE, show.legend = TRUE, size=0.001) +
    geom_label(data=d_measurement_all_noTikTok_filteredSemiRigorous, label= factor(d_measurement_all_noTikTok_filteredSemiRigorous$countFactor) , x=-3.25, size=3.5) +
    # geom_text(aes(label = paste0( "attempt..." ,1, sep="" ),x= -3 , y = dComplex_focus, alpha = 0.5 ), show.legend = FALSE ) +
    # geom_text(stat = "cross", mapping = aes(label = after_stat(observed))) +
    # geom_count() +
    # geom_smooth(method = strFormula)+
    # geom_text(label = count, color="red")+
    # geom_point(  aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +
    # geom_jitter(  aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.5, height= 0, col=dComplex_focus), show.legend = FALSE, size=1, shape=1, position=position_jitter(0,0.5)) + # sooooooort of ok... color according to dComplex_focus?
    # geom_beeswarm( aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.5),colour="blue", size=1, show.legend = FALSE ) +
    # geom_sina(aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.5), size=1,col="green",fill="green", show.legend = FALSE) + # not working...
    # geom_quasirandom(aes (x= log_diffA3 , y = dComplex_focus, alpha = 0.5), size=1, col="purple",fill="purple") +
    # make tests about the other potential distributions of points...?!
  xlim(c(-3,6)) +
    ggtitle("Test display responses log_diffA3") +
    facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") + 
    labs(title = 'The test display responses log_diffA3', y = "" ) +
    # theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none") + 
    guides(fill = FALSE) + 
    guides(col = FALSE)
  
  groupedPlotCI_3
}


# Need to think of what would be interesting calculations!
get_WHAT_TO_SAY <- function (responses, input){
  min_mwm_what_ql <- min(d_measurement_all_noTikTok_filteredSemiRigorous$log_diffA1[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
  
}
