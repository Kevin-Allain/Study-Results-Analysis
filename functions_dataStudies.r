library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
library(cowplot)
library(patchwork)
library(stringr)
library(rlist)

setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")

# ---- Functions
samplemean <- function(x, d) {
  return(mean(x[d]))
}
orderData <- function(d){
  d$dMask <- factor(d$dMask, levels=c("easy", "medium", "hard"))
  d$dComplex_focus <- factor(d$dComplex_focus, levels=c("E", "M", "H"))
  return (d)
}

genBoot <- function(d,question,focus="",dMask="",dComplex_focus="",R=10000){
  # cat("\ngenboot: question: ",question,", focus: ", focus," dMask: ", dMask,", dComplex_focus: ", dComplex_focus)
  boot_d <- c()
  if (focus=="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]],samplemean,R)
  } else if (focus=="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$dMask == dMask],samplemean,R)
  } else if (focus=="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus],samplemean,R)
  } else if (focus=="" & dMask != "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus & d$dMask == dMask],samplemean,R)
  } else if (focus!="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus],samplemean,R)
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask],samplemean,R)
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dComplex_focus == dComplex_focus],samplemean,R)
  } else {
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask & d$dComplex_focus == dComplex_focus],samplemean,R)
  }
  return (boot_d)
}

getMean_lowCI_highCI <- function (boot_d){
  ci <- boot.ci(boot.out = boot_d, type = c("norm", "basic", "perc", "bca"));
  mean <- boot_d$t0;
  l_ci <- ci$normal[2];
  h_ci <- ci$normal[3];
  res <- c(mean,l_ci,h_ci)
  # cat("\n in getMean_lowCI_highCI, mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  return (res);
}

make_gensMean_lowCI_highCI <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  cat("\n-- make_gensMean_lowCI_highCI; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
  # boot
  boot_s0 <- genBoot(d,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  return( c(gens_s0) )
}

make_gensMean_lowCI_highCI_sclDependent <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  cat("\n-- make_gensMean_lowCI_highCI_sclDependent; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
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
  cat("\n-- make_gensMean_lowCI_highCI_distractorDependent; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
  # select the data with scale 0
  d_s0 <- d[d$distractor=="h",]
  # boot
  boot_s0 <- genBoot(d_s0,question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  # select the data with scale 1
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

# TODO TEST
# Inspired by the function genAndPlot_differences_factorBased 
genAndPlotCI_factorBased <- function(d, factorScaling=FALSE, factorDistractor=FALSE,factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorVariation="dMask")
{
  arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE");
  arrDMask <- c("easy","medium","hard");
  arrDComplex_focus <- c("E","M","H");
  arrDistractor  <- c("h","n");
  if (factorScaling | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("diffA1","diffA2","diffA3");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  
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
  
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  
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
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    group1_CI<- genBoot(d=selec1,question=curQuestion);
                    group2_CI<- genBoot(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- genBoot(d=selec1,question=curQuestion);
                    group2_CI<- genBoot(d=selec2,question=curQuestion);
                    group3_CI<- genBoot(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
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
                    group1_CI<- genBoot(d=selec1,question=curQuestion);
                    group2_CI<- genBoot(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- genBoot(d=selec1,question=curQuestion);
                    group2_CI<- genBoot(d=selec2,question=curQuestion);
                    group3_CI<- genBoot(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              # numFactor==2
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              if (length(arrFactorVariations)== 2){
                cat("length(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                group1_CI<- genBoot(d=selec1,question=curQuestion);
                group2_CI<- genBoot(d=selec2,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI);
              } 
              else {
                cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                group1_CI<- genBoot(d=selec1,question=curQuestion);
                group2_CI<- genBoot(d=selec2,question=curQuestion);
                group3_CI<- genBoot(d=selec3,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              }
              is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI));
              dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
              dfTest_CI <- rename(dfTest_CI,low_CI=X2);
              dfTest_CI <- rename(dfTest_CI,high_CI=X3);
              dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI$question <- i
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            if (length(arrFactorVariations)== 2){
              cat("length(arrFactorVariations)== 2")
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              group1_CI<- genBoot(d=selec1,question=curQuestion);
              group2_CI<- genBoot(d=selec2,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
              dfTest_CI <- data.frame(group1_CI,group2_CI);
            } 
            else {
              cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              group1_CI<- genBoot(d=selec1,question=curQuestion);
              group2_CI<- genBoot(d=selec2,question=curQuestion);
              group3_CI<- genBoot(d=selec3,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
            }
            is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI));
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
            dfTest_CI <- rename(dfTest_CI,low_CI=X2);
            dfTest_CI <- rename(dfTest_CI,high_CI=X3);
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
            dfTest_CI[factor1] <- curFactor1; 
            dfTest_CI$question <- i
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
      
    }else {
        # no factoring... so which differences do we display?!
        cat("\ncase with numFactor == 0")
        # numFactor==0
        # warning: remember that factorVariation can be distractor
        dfTest_CI <- NULL
        if (length(arrFactorVariations)== 2){
          cat("length(arrFactorVariations)== 2")
          selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
          selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
          group1_CI<- genBoot(d=selec1,question=curQuestion);
          group2_CI<- genBoot(d=selec2,question=curQuestion);
          group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
          group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
          dfTest_CI <- data.frame(group1_CI,group2_CI);
        } 
        else {
          cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
          selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
          selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
          selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
          group1_CI<- genBoot(d=selec1,question=curQuestion);
          group2_CI<- genBoot(d=selec2,question=curQuestion);
          group3_CI<- genBoot(d=selec3,question=curQuestion);
          group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
          group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
          group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
          dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
        }
        is.numeric(dfTest_CI$mean_CI[2])
        dfTest_CI <- data.frame(t(dfTest_CI));
        dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
        dfTest_CI <- rename(dfTest_CI,low_CI=X2);
        dfTest_CI <- rename(dfTest_CI,high_CI=X3);
        dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
        dfTest_CI[factor1] <- curFactor1; 
        dfTest_CI$question <- i
        cols <- c("mean_CI","low_CI","high_CI");
        dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
        leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
        rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
        absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
        strSentence <- paste("Confidence intervals, ",curQuestion)
        dfCI_global <- rbind(dfCI_global, dfTest_CI)
        cat("\ngenerated the data to display, factorVariation-",factorVariation)
      }
    }
  return (dfCI_global);
}

genAndPlot_differences_factorBased <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorDifference="dMask"){
  cat("\ngenAndPlot_differences_factorBased")
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");
  if (factorScaling | factorDifference=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("diffA1","diffA2","diffA3");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\n")
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
  # food for thought... should these include scaling and distractor?! Need to adapt since distractor has only 2 categories
  arrFactorDifferences <- c()
  if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}
  # cat("\narrFactorDifferences: ",arrFactorDifferences)
  # d <- renameGroupedData(d) # test... seems to work fine # Calling this we change the names of the attributes... That's problematic
  # cat("\nfactors and arrFactors set. numFactor",numFactor,", factor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)
  
  # alternative idea: put together the data into one big structure... Another more clever loop can be made for generation of graphs...
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  
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
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[1],]
                  selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[2],]
                  selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[3],]
                  group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
                  group2_CI<- bootQuestionsDifferences_conservative(d=selec1, d2=selec3,question=curQuestion);
                  group3_CI<- bootQuestionsDifferences_conservative(d=selec2, d2=selec3,question=curQuestion);
                  group1_CI <- c(group1_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[1]," and ",arrFactor4[2]))
                  group2_CI <- c(group2_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[1]," and ",arrFactor4[3]))
                  group3_CI <- c(group3_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[2]," and ",arrFactor4[3]))
                  dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI))
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);dfTest_CI <- rename(dfTest_CI,low_CI=X2);dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"Diff_Type"=X4);
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  if (i==1){
                    groupedPlotCI_1 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  } 
                  else if (i==2){
                    groupedPlotCI_2 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  } 
                  else {
                    groupedPlotCI_3 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  }
                } 
                else {
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                  selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                  selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                  group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
                  group2_CI<- bootQuestionsDifferences_conservative(d=selec1, d2=selec3,question=curQuestion);
                  group3_CI<- bootQuestionsDifferences_conservative(d=selec2, d2=selec3,question=curQuestion);
                  group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                  group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                  group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                  dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              # numFactor==2
              # warning: remember that factorDifference can be distractor
              dfTest_CI <- NULL
              if (length(arrFactorDifferences)== 2){
                cat("length(arrFactorDifferences)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                dfTest_CI <- data.frame(group1_CI);
              } 
              else {
                cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
                group2_CI<- bootQuestionsDifferences_conservative(d=selec1, d2=selec3,question=curQuestion);
                group3_CI<- bootQuestionsDifferences_conservative(d=selec2, d2=selec3,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              }
              is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI));
              dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
              dfTest_CI <- rename(dfTest_CI,low_CI=X2);
              dfTest_CI <- rename(dfTest_CI,high_CI=X3);
              dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI$question <- i
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              cat("\ngenerated the data to display, factorDifference-",factorDifference,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorDifference can be distractor
            dfTest_CI <- NULL
            cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
            if (length(arrFactorDifferences)== 2){
              selec1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              dfTest_CI <- data.frame(group1_CI);
            } 
            else {
              selec1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
              group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
              group2_CI<- bootQuestionsDifferences_conservative(d=selec1, d2=selec3,question=curQuestion);
              group3_CI<- bootQuestionsDifferences_conservative(d=selec2, d2=selec3,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
              group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
            }
            is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI));
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
            dfTest_CI <- rename(dfTest_CI,low_CI=X2);
            dfTest_CI <- rename(dfTest_CI,high_CI=X3);
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
            dfTest_CI[factor1] <- curFactor1; 
            dfTest_CI$question <- i
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Differences of confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            cat("\ngenerated the data to display, factorDifference-",factorDifference,", factor1-",factor1,": ",curFactor1)
          }
          }
        }
      }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorDifference can be distractor
      dfTest_CI <- NULL
      cat("\nfactorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
      if (length(arrFactorDifferences)== 2){
        selec1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        dfTest_CI <- data.frame(group1_CI);
      } 
      else {
        selec1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        selec3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
        group1_CI<- bootQuestionsDifferences_conservative(d=selec1, d2= selec2,question=curQuestion);
        group2_CI<- bootQuestionsDifferences_conservative(d=selec1, d2=selec3,question=curQuestion);
        group3_CI<- bootQuestionsDifferences_conservative(d=selec2, d2=selec3,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
        group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
      }
      is.numeric(dfTest_CI$mean_CI[2])
      dfTest_CI <- data.frame(t(dfTest_CI));
      dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
      dfTest_CI <- rename(dfTest_CI,low_CI=X2);
      dfTest_CI <- rename(dfTest_CI,high_CI=X3);
      dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
      dfTest_CI$question <- i
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Differences of confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      cat("\ngenerated the data to display, factorDifference-",factorDifference)
    }
  }
  cat("\n####about to draw, how are the plots: ",(is.null(groupedPlotCI_1)),", ",(is.null(groupedPlotCI_2)),", ",(is.null(groupedPlotCI_3)) )
  cat("\nwhat of the global structure... ",dim(dfCI_global))
  
  class(dfCI_global$category_combination)
  class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
  class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
  class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global)
  cat("\nrenaming done... wtf was happening with dfCI_global not found? we specifically created the bastard a few lines ago")
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}    
  } 
  
  cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else {
    # no wrapping.
  } 
  
  if (numFactor!=0){  
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA1") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)
    # groupedPlotCI_1
    # dfCI_global[dfCI_global$question=="diffA1" & dfCI_global$scaling==0 & dfCI_global$dMask=="Mask Easy" & dfCI_global$category_combination=="dMask , easy _ medium",]
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA2") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA3") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)
    grid.arrange(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3, ncol=3)
  } 
  else {
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA1")
    # groupedPlotCI_1
    # dfCI_global[dfCI_global$question=="diffA1" & dfCI_global$scaling==0 & dfCI_global$dMask=="Mask Easy" & dfCI_global$category_combination=="dMask , easy _ medium",]
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA2") 
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA3")
    grid.arrange(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3, ncol=3)
  }
  
  return (dfCI_global)
}

returnFactorsCombination <- function(factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE){
  res <- c(NULL,NULL,NULL,NULL)
  factor1 <- NULL; factor2 <- NULL; factor3 <- NULL; factor4 <- NULL;
  if (factorScaling & factorDistractor){
    cat("\nError, we don't expect factorScaling & factorDistractor")
  }
  else if(factorScaling){
    # numGraphs <- max(1,factorScaling*length(arrScalings))*max(1,factorFocus*length(arrFocus))*max(1,factorDMask*length(arrMask))*max(1,factorDComplex_focus*length(arrDComplex_focus))
    factor1 <- "scaling";
    if (factorFocus){
      factor2 = "focus";
      if (factorDMask){
        factor3 = "dMask";
        if (factorDComplex_focus){
          factor4 <- "dComplex_focus";
        }
      }
    } else if(factorDMask){
      factor2 <- "dMask";
      if (factorDComplex_focus){
        factor3 <- "dComplex_focus";
      }
    } else if (factorDComplex_focus){
      factor2 <- "dComplex_focus";
    }
  } 
  else if (factorDistractor){
    factor1 <- "distractor";
    if (factorFocus){
      factor2 = "focus";
      if (factorDMask){
        factor3 = "dMask";
        if (factorDComplex_focus){
          factor4 <- "dComplex_focus";
        }
      }
    } else if(factorDMask){
      factor2 <- "dMask";
      if (factorDComplex_focus){
        factor3 <- "dComplex_focus";
      }
    } else if (factorDComplex_focus){
      factor2 <- "dComplex_focus";
    }
  } 
  else {
    # numGraphs <- max(1,factorFocus*length(arrFocus))*max(1,factorDMask*length(arrMask))*max(1,factorDComplex_focus*length(arrDComplex_focus))    
    if (factorFocus){
      factor1 = "focus";
      if (factorDMask){
        factor2 = "dMask";
        if (factorDComplex_focus){
          factor3 <- "dComplex_focus";
        }
      } 
      else{
        if (factorDComplex_focus){
          factor2 <- "dComplex_focus";
        }
      }
    } 
    else if(factorDMask){
      factor1 <- "dMask";
      if (factorDComplex_focus){
        factor2 <- "dComplex_focus";
      }
    } else if (factorDComplex_focus){
      factor1 <- "dComplex_focus";
    }
  }
  
  res <- c(factor1,factor2,factor3,factor4)
  return (res)
}

# Rename the categories for readability
renameGroupedData <- function(groupedData_all) {  
  cat("\n____renameGroupedData, colnames: ",toString(colnames(groupedData_all)))
  if("dMask" %in% colnames(groupedData_all))
  {
    cat("\nrename for dMask")
    groupedData_all$dMask = as.character(groupedData_all$dMask)
    groupedData_all$dMask[groupedData_all$dMask == "easy"] = "Mask Easy"
    groupedData_all$dMask[groupedData_all$dMask == "medium"] = "Mask Medium"
    groupedData_all$dMask[groupedData_all$dMask == "hard"] = "Mask Hard"
    groupedData_all$orderMaskComplex <- factor(groupedData_all$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
    groupedData_all$dMask[groupedData_all$dMask == "Mask Easy"] = "easy"
    groupedData_all$dMask[groupedData_all$dMask == "Mask Medium"] = "medium"
    groupedData_all$dMask[groupedData_all$dMask == "Mask Hard"] = "hard"
    
  }
  if ("dComplex_focus" %in% colnames(groupedData_all))
  {
    cat("\nrename for dComplex_focus")
    groupedData_all$dComplex_focus = as.character(groupedData_all$dComplex_focus)
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "E"] = "Focus Easy"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "M"] = "Focus Medium"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "H"] = "Focus Hard"
    groupedData_all$orderFocusComplex <- factor(groupedData_all$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Easy"] = "E"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Medium"] = "M"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Hard"] = "H"
  }
  if ("category_combination" %in% colnames(groupedData_all))
  {
    cat("\nRename for category_combination") # TODO add verifications and changes for other factoring approaches
    if (grepl("Mask",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\ncase of dMask to change")
      groupedData_all$category_combination = as.character(groupedData_all$category_combination)
      groupedData_all$category_combination[groupedData_all$category_combination == "dMask , medium _ hard"] = "Mask: Medium-Hard"
      groupedData_all$category_combination[groupedData_all$category_combination == "dMask , easy _ medium"] = "Mask: Easy-Medium"
      groupedData_all$category_combination[groupedData_all$category_combination == "dMask , easy _ hard"] = "Mask: Easy-Hard"
      groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("Mask: Easy-Medium","Mask: Easy-Hard","Mask: Medium-Hard"))
      groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Medium-Hard"] = "dMask , medium _ hard"
      groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Easy-Medium"] = "dMask , easy _ medium"
      groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Easy-Hard"] = "dMask , easy _ hard"
    } else if(grepl("scaling",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\ncase of scaling to change")
      groupedData_all$category_combination = as.character(groupedData_all$category_combination)
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling , 0 _ 1"] = "scaling: 0-1"
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling , 0 _ 2"] = "scaling: 0-2"
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling , 1 _ 2"] = "scaling: 1-2"
      groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("scaling: 0-1","scaling: 0-2","scaling: 1-2"))
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 0-1"] = "scaling , 0 _ 1"
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 0-2"] = "scaling , 0 _ 2"
      groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 1-2"] = "scaling , 1 _ 2"
    } else if(grepl("distractor",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\ncase of distractor to change")
      groupedData_all$category_combination = as.character(groupedData_all$category_combination)
      groupedData_all$category_combination[groupedData_all$category_combination == "distractor , h _ n"] = "distractor: hidden-normal"
      groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("distractor: hidden-normal"))
      groupedData_all$category_combination[groupedData_all$category_combination == "distractor: hidden-normal"] = "distractor , h _ n"
    } else if(grepl("dComplex_focus",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\ncase of dComplex_focus to change")
      groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
      groupedData_all$category_combination[groupedData_all$category_combination == "dComplex_focus , E _ M"] = "focus complexity: Easy-Medium"
      groupedData_all$category_combination[groupedData_all$category_combination == "dComplex_focus , E _ H"] = "focus complexity: Easy-Hard"
      groupedData_all$category_combination[groupedData_all$category_combination == "dComplex_focus , M _ H"] = "focus complexity: Medium-Hard"
      groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus complexity: Easy-Medium","focus complexity: Easy-Hard","focus complexity: Medium-Hard"))
      groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy-Medium"] = "dComplex_focus , E _ M"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy-Hard"] = "dComplex_focus , E _ H"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Medium-Hard"] = "dComplex_focus , M _ H"
      # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHAT_Qn"))
    } else if(grepl("focus",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\ncase of focus to change")
      groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
      groupedData_all$category_combination[groupedData_all$category_combination == "focus , WHAT_Qn _ WHAT_Ql"] = "focus: WHAT_Qn-WHAT_Ql"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus , WHAT_Qn _ WHERE"] = "focus: WHAT_Qn-WHERE"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus , WHAT_Ql _ WHERE"] = "focus: WHAT_Ql-WHERE"
      groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Qn-WHAT_Ql","focus: WHAT_Qn-WHERE","focus: WHAT_Ql-WHERE"))
      groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn-WHAT_Ql"] = "focus , WHAT_Qn _ WHAT_Ql"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn-WHERE"] = "focus , WHAT_Qn _ WHERE"
      groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql-WHERE"] = "focus , WHAT_Ql _ WHERE"
      # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHAT_Qn"))
    } 
  }
  if ("scaling" %in% colnames(groupedData_all)){
    groupedData_all$orderedScaling <- NA
    groupedData_all$scaling = as.character(groupedData_all$scaling)
    groupedData_all$orderedScaling[groupedData_all$scaling == 0] <- "Scaling 0"
    groupedData_all$orderedScaling[groupedData_all$scaling == 1] <- "Scaling 1"
    groupedData_all$orderedScaling[groupedData_all$scaling == 2] <- "Scaling 2"
  }
  
  return (groupedData_all);
}

generateGroupedData <- function (d){
  if (!is.null(d$scaling))
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask,scaling) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  } 
  else if (!is.null(d$distractor))
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask,distractor) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  } 
  else 
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  }
  groupedData_all["low_ci_DiffA1"] <- NA; groupedData_all["high_ci_DiffA1"] <-NA; 
  groupedData_all["low_ci_DiffA2"] <-NA; groupedData_all["high_ci_DiffA2"] <-NA; 
  groupedData_all["low_ci_DiffA3"] <-NA; groupedData_all["high_ci_DiffA3"] <-NA;
  groupedData_all["mean_t0_DiffA1"]<-NA;groupedData_all["mean_t0_DiffA2"]<-NA;groupedData_all["mean_t0_DiffA3"]<-NA;
  return (groupedData_all)
}

# TODO work in progress
# overall question: do we disregard the entire set of answers from a participant when they provide one problematic answer, or do we keep the rest? Let's start by being throrough in the data removal.
# about the reported trust: do we disregard the whole set of answers of a participant if they once answered 0 to all trust records for one stimuli, or only for that one stimuli...?
# impossibilities: we know of the cases for WHAT_Ql, but are there other impossiblilities?
filter_getRightParticipants <- function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[(d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d <- d[which(!(d$ResponseId %in% toFilter_ResponsesId)),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

filter_getWrongParticipants <- function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[(d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d <- d[which(d$ResponseId %in% toFilter_ResponsesId),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

modify_d_OkOrNot <-function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[(d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d$passedFilter <- NA
  d$passedFilter[which(d$ResponseId %in% toFilter_ResponsesId)] <- FALSE
  d$passedFilter[which(!(d$ResponseId %in% toFilter_ResponsesId))] <- TRUE
  return (d)
}

d_alt_enrichedFilter <- modify_d_OkOrNot(d_alt)
d_alt_Right <- filter_getRightParticipants(d_alt)
d_alt_Right
d_alt_Wrong <- filter_getWrongParticipants(d_alt)
d_alt_Wrong


# TODO FIX!!! potentially buggy!!! 
# TODO take pieces of the code from genAndPlot_differences_factorBased to adapt the factoring dynamically but with the generation (and display?) of each category
setGroupDataCI <- function(groupedData_all,d,scaling=FALSE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE){
  arrFocus_scaling <- c("WHAT_Qn","WHAT_Ql");
  arrFocus_measurement <- c("WHAT_Qn","WHAT_Ql","WHERE");
  arrDMask <- c("easy","medium","hard");
  arrDComplex_focus <- c("E","M","H");
  arrDistractor  <- c("h","n");
  for (k in 1:3){
    strDiff <- paste("diffA",k,sep="");
    strMean_t0 <- paste("mean_t0_DiffA",k,sep="");strlow_ci <- paste("low_ci_DiffA",k,sep=""); strhigh_ci <- paste("high_ci_DiffA",k,sep="");
    # ######## Scaling part
    if (scaling & !focus & !dMask & !dComplex_focus) {
      cat("\nscaling")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff)
      for (j in 1:3){
        groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
      }
    } 
    else if (scaling & !focus & dMask & !dComplex_focus) {
      cat("\nscaling X dMask");
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,"",arrDMask[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        }
      }
    } 
    else if (scaling & focus & !dMask & dComplex_focus) {
      cat("\nscaling X focus X dComplex_focus");
      for (f in 1:length(arrFocus_scaling)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,arrFocus_scaling[f],"",arrDComplex_focus[cf])
          for (j in 1:3){
            rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci)
            groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
            groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
            groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
          }
        }
      }
    }
    else if(scaling & focus & !dMask & !dComplex_focus) {
      cat("\nscaling X focus");
      for (i in 1:length(arrFocus_scaling)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,arrFocus_scaling[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_high_ci
        }
      }
    } 
    else if(scaling & !focus & !dMask & dComplex_focus){
      cat("\nscaling X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,"","",arrDComplex_focus[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
        }
      }
    } # ########## Distractor part
    else if (distractor & !focus & !dMask & !dComplex_focus) {
      cat("\ndistractor")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      for (j in 1:length(arrDistractor)){
        groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
      }
    } 
    else if (distractor & !focus & dMask & !dComplex_focus) {
      cat("\ndistractor X dMask");
      # & groupedData_all$scaling == arrDMask[i]
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,"",arrDMask[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        }
      }
    } 
    else if (distractor & focus & !dMask & dComplex_focus) {
      cat("\ndistractor X focus X dComplex_focus");
      for (f in 1:length(arrFocus_measurement)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,focus=arrFocus_measurement[f],dMask="",dComplex_focus=arrDComplex_focus[cf])
          for (j in 1:length(arrDistractor)){
            rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci,"\n -- arrDistractor[j]: ",arrDistractor[j],", arrFocus_measurement[f]: ",arrFocus_measurement[f],", arrDComplex_focus[cf]: ",arrDComplex_focus[cf])
            groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
            groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
            groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
          }
        }
      }
    }
    else if(distractor & focus & !dMask & !dComplex_focus) {
      cat("\ndistractor X focus");
      for (i in 1:length(arrFocus_measurement)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,arrFocus_measurement[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_high_ci
        }
      }
    } 
    else if(distractor & !focus & !dMask & dComplex_focus){
      cat("\ndistractor X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,"","",arrDComplex_focus[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
        }
      }
    } # ######## Measurement part
    else if (!scaling & !distractor & !focus & !dMask & !dComplex_focus) {
      cat("\nmeasurment")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      groupedData_all[[strMean_t0]] <- rep(summ_sclAll_diffAx[1], dim(groupedData_all)[1])
      groupedData_all[[strlow_ci]] <- rep(summ_sclAll_diffAx[2], dim(groupedData_all)[1])
      groupedData_all[[strhigh_ci]] <- rep(summ_sclAll_diffAx[3], dim(groupedData_all)[1])
    } 
    else if (!scaling & !distractor & !focus & dMask & !dComplex_focus) {
      cat("\nmeasurement X dMask");
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,"",arrDMask[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$dMask == arrDMask[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        
      }
    } 
    else if (!scaling & !distractor & focus & !dMask & dComplex_focus) {
      cat("\nmeasurement X focus X dComplex_focus");
      for (f in 1:length(arrFocus_measurement)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,arrFocus_measurement[f],"",arrDComplex_focus[cf])
          rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci)
          groupedData_all[[strMean_t0]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
          groupedData_all[[strlow_ci]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
          groupedData_all[[strhigh_ci]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
        }
      }
    }
    else if(!scaling & !distractor & focus & !dMask & !dComplex_focus) {
      cat("\nmeasurement X focus");
      for (i in 1:length(arrFocus_measurement)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,arrFocus_measurement[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_high_ci
      }
    } 
    else if(!scaling & !distractor & !focus & !dMask & dComplex_focus){
      cat("\nmeasurement X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,"","",arrDComplex_focus[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
      }
    }
    else {
      cat("\ncategory missing?")
    }
  }
  
  return (groupedData_all);
}

genDF_scaling_correctB_dMask <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_dMask <- d_scl0 %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_dMask["scaling"] <-0
  sumCorrectB_scl1_dMask <- d_scl1 %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_dMask["scaling"] <-1
  sumCorrectB_scl2_dMask <- d_scl2 %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_dMask["scaling"] <-2
  dfCorrectB_scaling_dMask <- rbind(sumCorrectB_scl0_dMask, sumCorrectB_scl1_dMask,sumCorrectB_scl2_dMask)
  return (dfCorrectB_scaling_dMask)
}

genDF_scaling_correctB_focus <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_focus <- d_scl0 %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_focus["scaling"] <-0
  sumCorrectB_scl1_focus <- d_scl1 %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_focus["scaling"] <-1
  sumCorrectB_scl2_focus <- d_scl2 %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_focus["scaling"] <-2
  dfCorrectB_scaling_focus <- rbind(sumCorrectB_scl0_focus, sumCorrectB_scl1_focus,sumCorrectB_scl2_focus)
  return (dfCorrectB_scaling_focus)
}

genDF_scaling_correctB_dComplex_focus <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_dComplex_focus <- d_scl0 %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_dComplex_focus["scaling"] <- 0
  sumCorrectB_scl1_dComplex_focus <- d_scl1 %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_dComplex_focus["scaling"] <- 1
  sumCorrectB_scl2_dComplex_focus <- d_scl2 %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_dComplex_focus["scaling"] <- 2
  dfCorrectB_scaling_dComplex_focus <- rbind(sumCorrectB_scl0_dComplex_focus, sumCorrectB_scl1_dComplex_focus,sumCorrectB_scl2_dComplex_focus)
  return (dfCorrectB_scaling_dComplex_focus)
}

genDF_scaling_correctB_overall <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_overAll <- d_scl0 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_overAll["scaling"] <- 0
  sumCorrectB_scl1_overAll <- d_scl1 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_overAll["scaling"] <- 1
  sumCorrectB_scl2_overAll <- d_scl2 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_overAll["scaling"] <- 2
  dfCorrectB_scaling_overAll <- rbind(sumCorrectB_scl0_overAll, sumCorrectB_scl1_overAll,sumCorrectB_scl2_overAll)
  return (dfCorrectB_scaling_overAll)
}

genDF_scaling_trust <- function (d_scl0,d_scl1,d_scl2){
  sumTrustA1_scl0_overAll <- d_scl0 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl0_overAll["scaling"] <- 0
  sumTrustA1_scl0_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl0_overAll["trustA2"] <- NA; sumTrustA1_scl0_overAll["trustA3"] <- NA; sumTrustA1_scl0_overAll["trustB"] <- NA
  sumTrustA2_scl0_overAll <- d_scl0 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl0_overAll["scaling"] <- 0
  sumTrustA2_scl0_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl0_overAll["trustA1"] <- NA; sumTrustA2_scl0_overAll["trustA3"] <- NA; sumTrustA2_scl0_overAll["trustB"] <- NA
  sumTrustA3_scl0_overAll <- d_scl0 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl0_overAll["scaling"] <- 0
  sumTrustA3_scl0_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl0_overAll["trustA1"] <- NA; sumTrustA3_scl0_overAll["trustA2"] <- NA; sumTrustA3_scl0_overAll["trustB"] <- NA
  sumTrustB_scl0_overAll <- d_scl0 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl0_overAll["scaling"] <- 0
  sumTrustB_scl0_overAll["trustType"] <- "trustB"
  sumTrustB_scl0_overAll["trustA1"] <- NA; sumTrustB_scl0_overAll["trustA2"] <- NA; sumTrustB_scl0_overAll["trustA3"] <- NA
  
  sumTrustA1_scl1_overAll <- d_scl1 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl1_overAll["scaling"] <- 1
  sumTrustA1_scl1_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl1_overAll["trustA2"] <- NA; sumTrustA1_scl1_overAll["trustA3"] <- NA; sumTrustA1_scl1_overAll["trustB"] <- NA
  sumTrustA2_scl1_overAll <- d_scl1 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl1_overAll["scaling"] <- 1
  sumTrustA2_scl1_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl1_overAll["trustA1"] <- NA; sumTrustA2_scl1_overAll["trustA3"] <- NA; sumTrustA2_scl1_overAll["trustB"] <- NA
  sumTrustA3_scl1_overAll <- d_scl1 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl1_overAll["scaling"] <- 1
  sumTrustA3_scl1_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl1_overAll["trustA1"] <- NA; sumTrustA3_scl1_overAll["trustA2"] <- NA; sumTrustA3_scl1_overAll["trustB"] <- NA
  sumTrustB_scl1_overAll <- d_scl1 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl1_overAll["scaling"] <- 1
  sumTrustB_scl1_overAll["trustType"] <- "trustB"
  sumTrustB_scl1_overAll["trustA1"] <- NA; sumTrustB_scl1_overAll["trustA2"] <- NA; sumTrustB_scl1_overAll["trustA3"] <- NA
  
  sumTrustA1_scl2_overAll <- d_scl2 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl2_overAll["scaling"] <- 2
  sumTrustA1_scl2_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl2_overAll["trustA2"] <- NA; sumTrustA1_scl2_overAll["trustA3"] <- NA; sumTrustA1_scl2_overAll["trustB"] <- NA
  sumTrustA2_scl2_overAll <- d_scl2 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl2_overAll["scaling"] <- 2
  sumTrustA2_scl2_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl2_overAll["trustA1"] <- NA; sumTrustA2_scl2_overAll["trustA3"] <- NA; sumTrustA2_scl2_overAll["trustB"] <- NA
  sumTrustA3_scl2_overAll <- d_scl2 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl2_overAll["scaling"] <- 2
  sumTrustA3_scl2_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl2_overAll["trustA1"] <- NA; sumTrustA3_scl2_overAll["trustA2"] <- NA; sumTrustA3_scl2_overAll["trustB"] <- NA
  sumTrustB_scl2_overAll <- d_scl2 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl2_overAll["scaling"] <- 2
  sumTrustB_scl2_overAll["trustType"] <- "trustB"
  sumTrustB_scl2_overAll["trustA1"] <- NA; sumTrustB_scl2_overAll["trustA2"] <- NA; sumTrustB_scl2_overAll["trustA3"] <- NA
  
  dfTrust_scaling_overAll <- rbind(sumTrustA1_scl0_overAll,sumTrustA2_scl0_overAll,sumTrustA3_scl0_overAll,sumTrustB_scl0_overAll, sumTrustA1_scl1_overAll,sumTrustA2_scl1_overAll,sumTrustA3_scl1_overAll,sumTrustB_scl1_overAll, sumTrustA1_scl2_overAll,sumTrustA2_scl2_overAll,sumTrustA3_scl2_overAll,sumTrustB_scl2_overAll)
  return (dfTrust_scaling_overAll)
}

genDF_distractor_correctB_focus <- function (d_h,d_n){
  sumCorrectB_h_focus <- d_h %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_focus["distractor"] <-"h"
  sumCorrectB_n_focus <- d_n %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_focus["distractor"] <- "n"
  dfCorrectB_distractor_focus <- rbind(sumCorrectB_h_focus, sumCorrectB_n_focus)
  return (dfCorrectB_distractor_focus)
}

genDF_distractor_correctB_dMask <- function (d_h,d_n){
  sumCorrectB_h_dMask <- d_h %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_dMask["distractor"] <-"h"
  sumCorrectB_n_dMask <- d_n %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_dMask["distractor"] <- "n"
  dfCorrectB_distractor_dMask <- rbind(sumCorrectB_h_dMask, sumCorrectB_n_dMask)
  return (dfCorrectB_distractor_dMask)
}

genDF_distractor_correctB_dComplex_focus <- function (d_h,d_n){
  sumCorrectB_h_dComplex_focus <- d_h %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_dComplex_focus["distractor"] <- "h"
  sumCorrectB_n_dComplex_focus <- d_n %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_dComplex_focus["distractor"] <- "n"
  dfCorrectB_distractor_dComplex_focus <- rbind(sumCorrectB_h_dComplex_focus, sumCorrectB_n_dComplex_focus)
  return (dfCorrectB_distractor_dComplex_focus)
}

genDF_distractor_correctB_overall <- function (d_h,d_n){
  sumCorrectB_h_overAll <- d_h %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_h_overAll["distractor"] <- "h"
  sumCorrectB_n_overAll <- d_n %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_overAll["distractor"] <- "n"
  dfCorrectB_distractor_overAll <- rbind(sumCorrectB_h_overAll, sumCorrectB_n_overAll)
  return (dfCorrectB_distractor_overAll)
}

genDF_distractor_trust <- function (d_h,d_n){
  sumTrustA1_h_overAll <- d_h %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_h_overAll["distractor"] <- "h"
  sumTrustA1_h_overAll["trustType"] <- "trustA1"
  sumTrustA1_h_overAll["trustA2"] <- NA; sumTrustA1_h_overAll["trustA3"] <- NA; sumTrustA1_h_overAll["trustB"] <- NA
  sumTrustA2_h_overAll <- d_h %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_h_overAll["distractor"] <- "h"
  sumTrustA2_h_overAll["trustType"] <- "trustA2"
  sumTrustA2_h_overAll["trustA1"] <- NA; sumTrustA2_h_overAll["trustA3"] <- NA; sumTrustA2_h_overAll["trustB"] <- NA
  sumTrustA3_h_overAll <- d_h %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_h_overAll["distractor"] <- "h"
  sumTrustA3_h_overAll["trustType"] <- "trustA3"
  sumTrustA3_h_overAll["trustA1"] <- NA; sumTrustA3_h_overAll["trustA2"] <- NA; sumTrustA3_h_overAll["trustB"] <- NA
  sumTrustB_h_overAll <- d_h %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_h_overAll["distractor"] <- "h"
  sumTrustB_h_overAll["trustType"] <- "trustB"
  sumTrustB_h_overAll["trustA1"] <- NA; sumTrustB_h_overAll["trustA2"] <- NA; sumTrustB_h_overAll["trustA3"] <- NA
  
  sumTrustA1_n_overAll <- d_n %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_n_overAll["distractor"] <- "n"
  sumTrustA1_n_overAll["trustType"] <- "trustA1"
  sumTrustA1_n_overAll["trustA2"] <- NA; sumTrustA1_n_overAll["trustA3"] <- NA; sumTrustA1_n_overAll["trustB"] <- NA
  sumTrustA2_n_overAll <- d_n %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_n_overAll["distractor"] <- "n"
  sumTrustA2_n_overAll["trustType"] <- "trustA2"
  sumTrustA2_n_overAll["trustA1"] <- NA; sumTrustA2_n_overAll["trustA3"] <- NA; sumTrustA2_n_overAll["trustB"] <- NA
  sumTrustA3_n_overAll <- d_n %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_n_overAll["distractor"] <- "n"
  sumTrustA3_n_overAll["trustType"] <- "trustA3"
  sumTrustA3_n_overAll["trustA1"] <- NA; sumTrustA3_n_overAll["trustA2"] <- NA; sumTrustA3_n_overAll["trustB"] <- NA
  sumTrustB_n_overAll <- d_n %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_n_overAll["distractor"] <- "n"
  sumTrustB_n_overAll["trustType"] <- "trustB"
  sumTrustB_n_overAll["trustA1"] <- NA; sumTrustB_n_overAll["trustA2"] <- NA; sumTrustB_n_overAll["trustA3"] <- NA
  
  dfTrust_distractor_overAll <- rbind(sumTrustA1_h_overAll,sumTrustA2_h_overAll,sumTrustA3_h_overAll,sumTrustB_h_overAll, sumTrustA1_n_overAll,sumTrustA2_n_overAll,sumTrustA3_n_overAll,sumTrustB_n_overAll)
  return (dfTrust_distractor_overAll)
}

genDF_measurement_correctB_dComplex_focus <- function (d){
  sumCorrectB_dComplex_focus <- d %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_dComplex_focus <- rbind(sumCorrectB_dComplex_focus)
  return (dfCorrectB_dComplex_focus)
}

genDF_measurement_correctB_dMask <- function (d){
  sumCorrectB_dMask <- d %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_dMask <- rbind(sumCorrectB_dMask)
  return (dfCorrectB_measurement_dMask)
}

genDF_measurement_correctB_focus <- function (d){
  sumCorrectB_focus <- d %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_focus <- rbind(sumCorrectB_focus)
  return (dfCorrectB_measurement_focus)
}

genDF_measurement_correctB_overall <- function (d){
  sumCorrectB_overAll <- d %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_overAll <- rbind(sumCorrectB_overAll)
  return (dfCorrectB_measurement_overAll)
}

genDF_measurement_trust <- function (d){
  sumTrustA1_overAll <- d %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_overAll["trustType"] <- "trustA1"
  sumTrustA1_overAll["trustA2"] <- NA; sumTrustA1_overAll["trustA3"] <- NA; sumTrustA1_overAll["trustB"] <- NA
  sumTrustA2_overAll <- d %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_overAll["trustType"] <- "trustA2"
  sumTrustA2_overAll["trustA1"] <- NA; sumTrustA2_overAll["trustA3"] <- NA; sumTrustA2_overAll["trustB"] <- NA
  sumTrustA3_overAll <- d %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_overAll["trustType"] <- "trustA3"
  sumTrustA3_overAll["trustA1"] <- NA; sumTrustA3_overAll["trustA2"] <- NA; sumTrustA3_overAll["trustB"] <- NA
  sumTrustB_overAll <- d %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_overAll["trustType"] <- "trustB"
  sumTrustB_overAll["trustA1"] <- NA; sumTrustB_overAll["trustA2"] <- NA; sumTrustB_overAll["trustA3"] <- NA
  
  dfTrust_measurement_overAll <- rbind(sumTrustA1_overAll,sumTrustA2_overAll,sumTrustA3_overAll,sumTrustB_overAll)
  return (dfTrust_measurement_overAll)
}


# -------- gen and plot trust
genAndPlotTrust_scaling_overall <- function(d_scl0,d_scl1,d_scl2){
  dfTrust_scaling_overAll <- genDF_scaling_trust(d_scl0,d_scl1,d_scl2)
  plotTrustA1_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  plotTrustA1_scl0_overAll
  plotTrustA1_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA1_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())  
  plotTrustA3_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + 
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust B", y = "", fill = "trust") +
    scale_fill_manual("trust", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA1_scl0_overAll + plotTrustA1_scl1_overAll + plotTrustA1_scl2_overAll + plotTrustA2_scl0_overAll + plotTrustA2_scl1_overAll + plotTrustA2_scl2_overAll +
    plotTrustA3_scl0_overAll + plotTrustA3_scl1_overAll + plotTrustA3_scl2_overAll + plotTrustB_scl0_overAll + plotTrustB_scl1_overAll + plotTrustB_scl2_overAll +  
    plot_layout(ncol = 12, widths = c(1, 1))
}

genAndPlot_differencesBoot_scaling <- function (d,d2,d3,question,focus="",dMask="",dComplex_focus="",R=10000){
  diffsOfBoots_1_2 <- bootQuestionsDifferences_conservative(d,d2,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R)
  diffsOfBoots_1_3 <- bootQuestionsDifferences_conservative(d,d3,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R)
  diffsOfBoots_2_3 <- bootQuestionsDifferences_conservative(d2,d3,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R)
  diffsOfBoots_1_2 <- c(diffsOfBoots_1_2, "diff scale 0 and 1")
  diffsOfBoots_1_3 <- c(diffsOfBoots_1_3, "diff scale 0 and 2")
  diffsOfBoots_2_3 <- c(diffsOfBoots_2_3, "diff scale 1 and 2")
  df <- data.frame(diffsOfBoots_1_2,diffsOfBoots_1_3,diffsOfBoots_2_3);
  df <- data.frame(t(df))
  df <- rename(df,mean_CI=X1);df <- rename(df,low_CI=X2);df <- rename(df,high_CI=X3);df <- rename(df,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  df[,cols] <- lapply( df[,cols],as.numeric)
  cat("\ndf$low_CI: ",df$low_CI,", df$high_CI: ",df$high_CI)
  strSentence <- paste("The differences accordin to scaling for the questions: ",question)
  leftEdgeGraph <- min(-0.15, min(df$low_CI) -0.1 ); rightEdgeGraph <- max(0.15,max(df$high_CI)+0.1)
  cat("\nleftEdgeGraph: ",leftEdgeGraph,", rightEdgeGraph: ",rightEdgeGraph)
  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
  cat("\nabsGraphEdge: ",absGraphEdge)
  groupedPlotDiffB <- ggplot(df, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-absGraphEdge,absGraphEdge)) + 
    ggtitle(strSentence)
  
  grid.arrange(groupedPlotDiffB, ncol=1)
}

genAndPlot_errorRate_correctB_scaling <- function (d_scl0, d_scl1, d_scl2){
  boot_scl0 <- boot(d_scl0$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,0)
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_scl1$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,1)
  # 
  boot_scl2 <- boot(d_scl2$correctB,samplemean,10000)
  ci_scl2 <- boot.ci(boot.out = boot_scl2, type = c("norm", "basic", "perc", "bca"));
  mean_scl2 <- boot_scl2$t0;
  l_ci_scl2 <- ci_scl2$normal[2];
  h_ci_scl2 <- ci_scl2$normal[3];
  res_scl2 <- c(mean_scl2,l_ci_scl2,h_ci_scl2)
  res_scl2 <- c(res_scl2,2)
  
  df_errorRate <- data.frame(res_scl0,res_scl1,res_scl2)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,scaling=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}

genAndPlotCorrectB_scaling_mask <- function(d_scl0,d_scl1,d_scl2) {
  dfCorrectB_scaling_dMask <-genDF_scaling_correctB_dMask(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_dMask <- renameGroupedData(dfCorrectB_scaling_dMask)
  plotCorrectB_scl0_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==0,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==1,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==2,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_maskDiff_correctB <- grid.arrange(plotCorrectB_scl0_dMask, plotCorrectB_scl1_dMask, plotCorrectB_scl2_dMask,ncol=3,
                                         top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_scaling_dcomplex_focus <- function (d_scl0,d_scl1,d_scl2){
  dfCorrectB_scaling_dComplex_focus <- genDF_scaling_correctB_dComplex_focus(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_dComplex_focus <- renameGroupedData(dfCorrectB_scaling_dComplex_focus)
  plotCorrectB_scl0_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==0,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==1,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==2,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_scl0_dFocus, plotCorrectB_scl1_dFocus, plotCorrectB_scl2_dFocus,ncol=3,
                                          top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_scaling_focus <- function (d_scl0,d_scl1,d_scl2){
  dfCorrectB_scaling_focus <- genDF_scaling_correctB_focus(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_focus <- renameGroupedData(dfCorrectB_scaling_focus)
  plotCorrectB_scl0_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==0,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==1,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==2,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_scl0_dFocus, plotCorrectB_scl1_dFocus, plotCorrectB_scl2_dFocus,ncol=3,
                                          top = textGrob("focus",gp=gpar(fontsize=20,font=3)))
}

genAndPlotTrust_distractor_overall <- function(d_h,d_n){
  d_h <- subset(d_h, numbers_only(d_h$trustA1) & numbers_only(d_h$trustA2) & numbers_only(d_h$trustA3) & numbers_only(d_h$trustB))
  d_n <- subset(d_n, numbers_only(d_n$trustA1) & numbers_only(d_n$trustA2) & numbers_only(d_n$trustA3) & numbers_only(d_n$trustB))
  
  dfTrust_distractor_overAll <- genDF_distractor_trust(d_h,d_n)
  plotTrustA1_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  plotTrustA1_h_overAll
  plotTrustA1_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA2_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA3_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustB_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust B", y = "", fill = "Trust") +
    scale_fill_manual("Trust", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  
  plotTrustA1_h_overAll + plotTrustA1_n_overAll +  plotTrustA2_h_overAll + plotTrustA2_n_overAll + 
    plotTrustA3_h_overAll + plotTrustA3_n_overAll +  plotTrustB_h_overAll + plotTrustB_n_overAll + 
    plot_layout(ncol = 8, widths = c(1, 1))
}
genAndPlotTrust_measurement_focus <- function(d) {
  dfCorrectB_measurement_focus <-genDF_measurement_correctB_focus(d)
  dfCorrectB_measurement_focus <- renameGroupedData(dfCorrectB_measurement_focus)
  plotCorrectB_focus <- ggplot(dfCorrectB_measurement_focus, aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_focus,  
                                          top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))
}

genAndPlotTrust_measurement_dcomplex_focus <- function (d){
  dfCorrectB_measurement_dComplex_focus <- genDF_measurement_correctB_dComplex_focus(d)
  dfCorrectB_measurement_dComplex_focus <- renameGroupedData(dfCorrectB_measurement_dComplex_focus)
  plotCorrectBa_dFocus <- ggplot(dfCorrectB_measurement_dComplex_focus, aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectBa_dFocus, ncol=1,
                                          top = textGrob("dComplexFocusDiff",gp=gpar(fontsize=20,font=3)))
}


# -------- gen and plot error rate



# dim(d_scl0[d_scl0$correctB==1,])[1] / dim(d_scl0)[1]
# Error rate: computed as the number of incorrect answers per task multiplied by the total number of repetitions.

genAndPlot_errorRate_correctB_scaling <- function (d_scl0, d_scl1, d_scl2) # This is NOT it!
{
  boot_scl0 <- boot(d_scl0$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,0)
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_scl1$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,1)
  # 
  boot_scl2 <- boot(d_scl2$correctB,samplemean,10000)
  ci_scl2 <- boot.ci(boot.out = boot_scl2, type = c("norm", "basic", "perc", "bca"));
  mean_scl2 <- boot_scl2$t0;
  l_ci_scl2 <- ci_scl2$normal[2];
  h_ci_scl2 <- ci_scl2$normal[3];
  res_scl2 <- c(mean_scl2,l_ci_scl2,h_ci_scl2)
  res_scl2 <- c(res_scl2,2)
  
  df_errorRate <- data.frame(res_scl0,res_scl1,res_scl2)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,scaling=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}
genAndPlot_errorRate_correctB_distractor <- function (d_h, d_n){
  boot_scl0 <- boot(d_h$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,"h")
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_n$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,"n")
  
  df_errorRate <- data.frame(res_scl0,res_scl1)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,distractor=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=distractor)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}

genAndPlot_differences_errorRateB_scaling <- function (d_scl0, d_scl1, d_scl2){
  questions <- c("correctB")
  diffsOfBoots_0_1_B <- bootQuestionsDifferences_conservative(d_scl0,d_scl1,"correctB")
  diffsOfBoots_0_2_B <- bootQuestionsDifferences_conservative(d_scl0,d_scl2,"correctB")
  diffsOfBoots_1_2_B <- bootQuestionsDifferences_conservative(d_scl1,d_scl2,"correctB")
  diffsOfBoots_0_1_B <- c(diffsOfBoots_0_1_B, "Differences Scaling 0 and 1")
  diffsOfBoots_0_2_B <- c(diffsOfBoots_0_2_B, "Differences Scaling 0 and 2")
  diffsOfBoots_1_2_B <- c(diffsOfBoots_1_2_B, "Differences Scaling 1 and 2")
  
  cat("\ndiffsOfBoots_0_1_B: ",toString(diffsOfBoots_0_1_B))
  
  diffsOfBoots_0_1_B <- data.frame(diffsOfBoots_0_1_B);
  diffsOfBoots_0_1_B <- data.frame(t(diffsOfBoots_0_1_B));
  diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,mean_CI=X1);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,low_CI=X2);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,high_CI=X3);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_0_1_B[,cols] <- lapply( diffsOfBoots_0_1_B[,cols],as.numeric)
  
  diffsOfBoots_0_2_B <- data.frame(diffsOfBoots_0_2_B);
  diffsOfBoots_0_2_B <- data.frame(t(diffsOfBoots_0_2_B))
  diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,mean_CI=X1);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,low_CI=X2);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,high_CI=X3);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_0_2_B[,cols] <- lapply( diffsOfBoots_0_2_B[,cols],as.numeric)
  
  diffsOfBoots_1_2_B <- data.frame(diffsOfBoots_1_2_B);
  diffsOfBoots_1_2_B <- data.frame(t(diffsOfBoots_1_2_B))
  diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,mean_CI=X1);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,low_CI=X2);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,high_CI=X3);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_1_2_B[,cols] <- lapply( diffsOfBoots_1_2_B[,cols],as.numeric)
  
  cat("\ngenerated the data frames")
  
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;

  dfCI_global <- rbind(dfCI_global, diffsOfBoots_0_1_B); dfCI_global <- rbind(dfCI_global, diffsOfBoots_0_2_B); dfCI_global <- rbind(dfCI_global, diffsOfBoots_1_2_B);
  
  groupedPlotDiffB <- ggplot(dfCI_global, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-1,1)) + 
    ggtitle("Differences of Error Rate: Scaling")
  
  grid.arrange(groupedPlotDiffB, ncol=1)
  
}
# genAndPlot_differences_errorRateB_scaling(d_scl0, d_scl1, d_scl2)

genAndPlot_differencesBoot_distractor <- function (d,d2,focus="",dMask="",dComplex_focus="",R=10000){
  questions <- c("correctB")
  diffsOfBoots_1_2_B <- bootQuestionsDifferences_conservative(d,d2,question=questions[1],focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R)
  diffsOfBoots_1_2_B <- c(diffsOfBoots_1_2_B, "diff distractor and normal")
  df_B <- data.frame(diffsOfBoots_1_2_B);
  df_B <- data.frame(t(df_B))
  df_B <- rename(df_B,mean_CI=X1);df_B <- rename(df_B,low_CI=X2);df_B <- rename(df_B,high_CI=X3);df_B <- rename(df_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  df_B[,cols] <- lapply( df_B[,cols],as.numeric)
  
  groupedPlotDiffB <- ggplot(df_B, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-1,1)) + 
    ggtitle("diffB")
  
  grid.arrange(groupedPlotDiffB, ncol=1)
}

genAndPlotTrust_distractor_mask <- function(d_h,d_n) {
  dfCorrectB_distractor_dMask <-genDF_distractor_correctB_dMask(d_h,d_n)
  dfCorrectB_distractor_dMask <- renameGroupedData(dfCorrectB_distractor_dMask)
  plotCorrectB_h_dMask <- ggplot(dfCorrectB_distractor_dMask[dfCorrectB_distractor_dMask$distractor=="h",], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Distractor", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_n_dMask <- ggplot(dfCorrectB_distractor_dMask[dfCorrectB_distractor_dMask$distractor=="n",], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Normal", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_maskDiff_correctB <- grid.arrange(plotCorrectB_h_dMask, plotCorrectB_n_dMask, 
                                         top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_distractor_dcomplex_focus <- function (d_h,d_n){
  dfCorrectB_distractor_dComplex_focus <- genDF_distractor_correctB_dComplex_focus(d_h,d_n)
  dfCorrectB_distractor_dComplex_focus <- renameGroupedData(dfCorrectB_distractor_dComplex_focus)
  plotCorrectB_h_dFocus <- ggplot(dfCorrectB_distractor_dComplex_focus[dfCorrectB_distractor_dComplex_focus$distractor=="h",], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Distractor", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_n_dFocus <- ggplot(dfCorrectB_distractor_dComplex_focus[dfCorrectB_distractor_dComplex_focus$distractor=="n",], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Normal", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_h_dFocus, plotCorrectB_n_dFocus, ncol=2,
                                          top = textGrob("dComplexFocusDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_distractor_focus <- function(d_h,d_n) {
  dfCorrectB_distractor_focus <-genDF_distractor_correctB_focus(d_h,d_n)
  dfCorrectB_distractor_focus <- renameGroupedData(dfCorrectB_distractor_focus)
  plotCorrectB_h_focus <- ggplot(dfCorrectB_distractor_focus[dfCorrectB_distractor_focus$distractor=="h",], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Distractor", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_n_focus <- ggplot(dfCorrectB_distractor_focus[dfCorrectB_distractor_focus$distractor=="n",], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Normal", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_h_focus, plotCorrectB_n_focus, 
                                          top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))
}

genAndPlotTrust_measurement_overall <- function(d){
  d <- subset(d, numbers_only(d$trustA1) & numbers_only(d$trustA2) & numbers_only(d$trustA3) & numbers_only(d$trustB))
  
  dfTrust_measurement_overAll <- genDF_measurement_trust(d)
  plotTrustA1_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  
  plotTrustA2_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA3_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustB_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB)))  +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust B", y = "", fill = "trustB") +
    scale_fill_manual("Trust", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA1_overAll +  plotTrustA2_overAll +  plotTrustA3_overAll +  plotTrustB_overAll + plot_layout(ncol = 4, widths = c(1, 1))
}
genAndPlotTrust_measurement_mask <- function(d) {
  dfCorrectB_measurement_dMask <-genDF_measurement_correctB_dMask(d)
  dfCorrectB_measurement_dMask <- renameGroupedData(dfCorrectB_measurement_dMask)
  plotCorrectB_dMask <- ggplot(dfCorrectB_measurement_dMask, aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_maskDiff_correctB <- grid.arrange(plotCorrectB_dMask,  
                                         top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
}
