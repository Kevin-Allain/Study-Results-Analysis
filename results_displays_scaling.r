# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
library(cowplot)
library(patchwork)

setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")

# ---- Data loading
d_scl0 <-  read.table(file="data/transformed/survey_complete_scaling_0_2021_09_19_headerAdapted.csv",TRUE, ",")
d_scl1 <-  read.table(file="data/transformed/survey_complete_scaling_1_2021_09_19_headerAdapted.csv",TRUE, ",")
d_scl2 <-  read.table(file="data/transformed/survey_complete_scaling_2_2021_09_19_headerAdapted.csv",TRUE, ",")

# ---- Functions
samplemean <- function(x, d) {
  return(mean(x[d]))
}
orderData <- function(d){
  d$dMask <- factor(d$dMask, levels=c("easy", "medium", "hard"))
  d$dComplex_focus <- factor(d$dComplex_focus, levels=c("E", "M", "H"))
  return (d)
}

genBoot <- function(d,question,focus,dMask="",dComplex_Focus="",R=10000){
  boot_d <- c()
  if (dMask == "" & dComplex_Focus ==""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus],samplemean,R)
    }
  } else if (dMask != "" & dComplex_Focus ==""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask],samplemean,R)
    }
  } else if (dMask == "" & dComplex_Focus !=""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
    }
  } else {
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
    }
  }
  return (boot_d)
}

getMean_lowCI_highCI <- function (boot_d){
  ci <- boot.ci(boot.out = boot_d, type = c("norm", "basic", "perc", "bca"));
  mean <- boot_d$t0;
  l_ci <- ci$normal[2];
  h_ci <- ci$normal[3];
  res <- c(mean,l_ci,h_ci)
  return (res);
}

genDifferencesBoot <- function(d,d2,question,focus,dMask="",dComplex_Focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  if (dMask == "" & dComplex_Focus ==""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus],samplemean,R)
    }
  } else if (dMask != "" & dComplex_Focus ==""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask],samplemean,R)
    }
  } else if (dMask == "" & dComplex_Focus !=""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    }
  } else {
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
    }
  }
  
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  sumAbsDiffs <- c(abs(sumD[1]-sumD2[1]), abs(sumD[2]-sumD2[2]), abs(sumD[3]-sumD2[3]) )
  return (sumAbsDiffs)
}

getDifferencesBoot <- function (boot_d,boot_d2){
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  sumAbsDiffs <- c(abs(sumD[1]-sumD2[1]), abs(sumD[2]-sumD2[2]), abs(sumD[3]-sumD2[3]) )
  return (sumAbsDiffs)
}

# we need a function to display differences between scaling groups...! With confidence intervals...
# potential approach 1: get the values according to groups, then make substractions according to their shared cntrQ NO
# potential approach 2: get the values according to groups, make boot for each group, make the sample proportion, and then put it as the middle
# https://online.stat.psu.edu/stat100/lesson/9/9.3
bootQuestionsDifferences <- function(d,d2,question,focus,dMask="",dComplex_Focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  sampleSize <- -1; sampleSize2 <- -1;
  if (dMask == "" & dComplex_Focus ==""){
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
  else if (dMask != "" & dComplex_Focus ==""){
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
  else if (dMask == "" & dComplex_Focus !=""){
    if (question == "diffA1"){
      if (length(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]) == 0){
        boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus]) == 0){
        boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]);
      }
    }else if (question == "diffA2"){
      if (length(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]) == 0){
        boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus]) == 0){
        boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]);
      }
    } else {
      if (length(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]) == 0){
        boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus]) == 0){
        boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_Focus]);
      }
    }
  } 
  else {
    if (question == "diffA1"){
      if(length(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus])==0){
        boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus])==0) {
        boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus]);
      }
    }else if (question == "diffA2"){
      if(length(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus])==0){
        boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus])==0) {
        boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus]);
      }
    } else {
      if(length(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus])==0){
        boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD <- getMean_lowCI_highCI(boot_d)
        return(sumD)
      } else if (length(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus])==0) {
        boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sumD2 <- getMean_lowCI_highCI(boot_d2)
        return(sumD2)
      } else {
        boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus],samplemean,R)
        boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus],samplemean,R)
        sampleSize <- length(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_Focus]); sampleSize2 <- length(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_Focus]);
      }
    }
  }
  
  # calculate the differences and sample proportions...
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  
  meanDiff <- sumD[1]-sumD2[1]
  print("meanDiff: ")
  print(meanDiff)
  stdErr_d <- -1;stdErr_d2 <- -1
  # if (question == "diffA1"){
  #   stdErr_d <- sd(boot_d$diffA1);stdErr_d2 <- sd(boot_d2$diffA1)
  # } else if (question == "diffA1"){
  #   stdErr_d <- sd(boot_d$diffA2);stdErr_d2 <- sd(boot_d2$diffA2)
  # } else {
  #   stdErr_d <- sd(boot_d$diffA3);stdErr_d2 <- sd(boot_d2$diffA3)
  # }
  stdErr_d <- sd(boot_d$t);stdErr_d2 <- sd(boot_d2$t);
  print("managed to run sd")
  print(stdErr_d)
  SEM_d <- stdErr_d/sqrt(sampleSize); SEM_d2 <- stdErr_d2/sqrt(sampleSize2);
  print("SEMs")
  print(SEM_d);print(SEM_d2);
  std_Error_Difference <- sqrt(SEM_d*SEM_d + SEM_d2*SEM_d2)
  res <- c(meanDiff, meanDiff - std_Error_Difference, meanDiff + std_Error_Difference)
  return (res)
}

# ---- Test calls
class(d_scl0); d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecA <- d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecB <- d_scl1[d_scl1$focus=="WHAT_Qn",]; d_selecC <- d_scl2[d_scl2$focus=="WHAT_Qn",]; d_selecA[10,]$cntrQ; dim(d_selecA %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecB %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecC %>% distinct(cntrQ,.keep_all=TRUE)) ; unique(d_selecB$cntrQ); dim(d_selecA[d_selecA$cntrQ==381,]); dim(d_selecB[d_selecB$cntrQ==381,]); dim(d_selecC[d_selecC$cntrQ==381,]); length(d_selecA$diff42) == 0 ; boot_dTest <- boot(d_selecA$diffA1,samplemean,10000); boot_dTest.data; boot_dEmpty <- boot(d_selecA$diff42,samplemean,10000); stdErrorTest <- sd(boot_dTest$t); stdErrorTest 
# stdErrorTest <- boot_dTest$statistic(stderr())
ciTest <- boot.ci(boot.out = boot_dTest, type = c("norm", "basic", "perc", "bca"));

ci_Diff_scl0_scl2 <-bootQuestionsDifferences(d_scl0,d_scl2,"diffA1","WHAT_Qn")
ci_Diff_scl0_scl2

boot_diffA1_scl0 <-genBoot(d_scl0,"diffA1","WHAT_Qn")
sum_boot_diffA1_scl0 <- getMean_lowCI_highCI((boot_diffA1_scl0))
sum_boot_diffA1_scl0

boot_diffA1_scl1 <-genBoot(d_scl1,"diffA1","WHAT_Qn")
sum_boot_diffA1_scl1 <- getMean_lowCI_highCI((boot_diffA1_scl1))
diffsTest <- getDifferencesBoot(boot_diffA1_scl0,boot_diffA1_scl1)
sum_boot_diffA1_scl0
sum_boot_diffA1_scl1
diffsTest

# ---- Data transform
d_scl0<- orderData(d_scl0); d_scl1 <- orderData(d_scl1); d_scl2 <- orderData(d_scl2);
# trust
sumTrustA1_scl0_overAll <- d_scl0 %>% 
  group_by (trustA1) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA2_scl0_overAll <- d_scl0 %>% 
  group_by (trustA2) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA3_scl0_overAll <- d_scl0 %>% 
  group_by (trustA3) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustB_scl0_overAll <- d_scl0 %>% 
  group_by (trustB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

sumTrustA1_scl1_overAll <- d_scl1 %>% 
  group_by (trustA1) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA2_scl1_overAll <- d_scl1 %>% 
  group_by (trustA2) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA3_scl1_overAll <- d_scl1 %>% 
  group_by (trustA3) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustB_scl1_overAll <- d_scl1 %>% 
  group_by (trustB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

sumTrustA1_scl2_overAll <- d_scl2 %>% 
  group_by (trustA1) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA2_scl2_overAll <- d_scl2 %>% 
  group_by (trustA2) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustA3_scl2_overAll <- d_scl2 %>% 
  group_by (trustA3) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumTrustB_scl2_overAll <- d_scl2 %>% 
  group_by (trustB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

# correctB
sumCorrectB_scl0_dMask <- d_scl0 %>%
  group_by(dMask, correctB) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))
sumCorrectB_scl1_dMask <- d_scl1 %>% 
  group_by(dMask, correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl2_dMask <- d_scl2 %>% 
  group_by(dMask, correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl0_dComplex_focus <- d_scl0 %>%
  group_by(dComplex_focus, correctB) %>%
  summarise(count = n()) %>%
  mutate(perc = count/sum(count))
sumCorrectB_scl1_dComplex_focus <- d_scl1 %>% 
  group_by(dComplex_focus, correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl2_dComplex_focus <- d_scl2 %>% 
  group_by(dComplex_focus, correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl0_overAll <- d_scl0 %>% 
  group_by (correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl1_overAll <- d_scl1 %>% 
  group_by (correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))
sumCorrectB_scl2_overAll <- d_scl2 %>% 
  group_by (correctB) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

# ---- Plots
sumTrustA1_scl0_overAll
plotTrustA1_scl0_overAll
dar_trusts <- data.frame(a=sumTrustA1_scl0_overAll,b=sumTrustA1_scl1_overAll,c=sumTrustA1_scl2_overAll)
# trusts -overAll
plotTrustA1_scl0_overAll <- ggplot(sumTrustA1_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A1", y = "percent", fill = "trustA1") +
  theme_minimal(base_size = 5)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank()) 
plotTrustA1_scl1_overAll <- ggplot(sumTrustA1_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A1", y = "", fill = "trustA1") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA1_scl2_overAll <- ggplot(sumTrustA1_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A1", y = "", fill = "trustA1") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl0_overAll <- ggplot(sumTrustA2_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A2", y = "", fill = "trustA2") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl1_overAll <- ggplot(sumTrustA2_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A2", y = "", fill = "trustA2") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl2_overAll <- ggplot(sumTrustA2_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A2", y = "", fill = "trustA2") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())  
plotTrustA3_scl0_overAll <- ggplot(sumTrustA3_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A3", y = "", fill = "trustA3") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA3_scl1_overAll <- ggplot(sumTrustA3_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A3", y = "", fill = "trustA3") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA3_scl2_overAll <- ggplot(sumTrustA3_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A3", y = "", fill = "trustA3") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl0_overAll <- ggplot(sumTrustB_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust B", y = "", fill = "trustB") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl1_overAll <- ggplot(sumTrustB_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust B", y = "", fill = "trustB") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl2_overAll <- ggplot(sumTrustB_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + 
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust B", y = "", fill = "trust") +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())  

# correctB - overAll
plotCorrectB_scl0_overAll <- ggplot(sumCorrectB_scl0_overAll, aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 0", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
plotCorrectB_scl1_overAll <- ggplot(sumCorrectB_scl1_overAll, aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 1", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
plotCorrectB_scl2_overAll <- ggplot(sumCorrectB_scl2_overAll, aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 2", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  


# correctB - According to Mask
plotCorrectB_scl0_dMask <- ggplot(sumCorrectB_scl0_dMask, aes(x = factor(dMask), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 0", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

plotCorrectB_scl1_dMask <- ggplot(sumCorrectB_scl1_dMask, aes(x = factor(dMask), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 1", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

plotCorrectB_scl2_dMask <- ggplot(sumCorrectB_scl2_dMask, aes(x = factor(dMask), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 2", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

# correctB - According to Focus
plotCorrectB_scl0_dFocus <- ggplot(sumCorrectB_scl0_dComplex_focus, aes(x = factor(dComplex_focus), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 0", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

plotCorrectB_scl1_dFocus <- ggplot(sumCorrectB_scl1_dComplex_focus, aes(x = factor(dComplex_focus), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 1", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

plotCorrectB_scl2_dFocus <- ggplot(sumCorrectB_scl2_dComplex_focus, aes(x = factor(dComplex_focus), y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 2", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  

# Plots aggregations
# trust
# grid_overAll_trusts <-grid.arrange(plotTrustA1_scl0_overAll, plotTrustA1_scl1_overAll, plotTrustA1_scl2_overAll, plotTrustA2_scl0_overAll, plotTrustA2_scl1_overAll, plotTrustA2_scl2_overAll, plotTrustA3_scl0_overAll, plotTrustA3_scl1_overAll, plotTrustA3_scl2_overAll, plotTrustB_scl0_overAll, plotTrustB_scl1_overAll, plotTrustB_scl2_overAll, ncol=12, top = textGrob("overall",gp=gpar(fontsize=20,font=3)))
# plot_grid(plotTrustA1_scl0_overAll, plotTrustA1_scl1_overAll, plotTrustA1_scl2_overAll, align = "v")

plotTrustA1_scl0_overAll + plotTrustA1_scl1_overAll + plotTrustA1_scl2_overAll + plotTrustA2_scl0_overAll + plotTrustA2_scl1_overAll + plotTrustA2_scl2_overAll +
  plotTrustA3_scl0_overAll + plotTrustA3_scl1_overAll + plotTrustA3_scl2_overAll + plotTrustB_scl0_overAll + plotTrustB_scl1_overAll + plotTrustB_scl2_overAll +  
  plot_layout(ncol = 12, widths = c(1, 1))


# correctB
grid_overAll_correctB <- grid.arrange(plotCorrectB_scl0_overAll, plotCorrectB_scl1_overAll, plotCorrectB_scl2_overAll,ncol=3,
                                       top = textGrob("overall",gp=gpar(fontsize=20,font=3)))
grid_maskDiff_correctB <- grid.arrange(plotCorrectB_scl0_dMask, plotCorrectB_scl1_dMask, plotCorrectB_scl2_dMask,ncol=3,
                              top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
grid_focusDiff_correctB <- grid.arrange(plotCorrectB_scl0_dFocus, plotCorrectB_scl1_dFocus, plotCorrectB_scl2_dFocus,ncol=3,
                               top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))



