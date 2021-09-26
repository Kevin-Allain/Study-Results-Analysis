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
# d_scl0 <-  read.table(file="data/transformed/survey_complete_scaling_0_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_scl1 <-  read.table(file="data/transformed/survey_complete_scaling_1_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_scl2 <-  read.table(file="data/transformed/survey_complete_scaling_2_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_sclAll <- read.table(file="data/transformed/survey_complete_scaling_all_2021_09_19_headerAdapted.csv",TRUE, ",")
d_scl0 <-  read.table(file="data/transformed/survey_complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_scl1 <-  read.table(file="data/transformed/survey_complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_scl2 <-  read.table(file="data/transformed/survey_complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_sclAll <- read.table(file="data/transformed/survey_complete_scaling_all_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
d_sclAll <- na.omit(d_sclAll)
d_scl0 <- d_sclAll[d_sclAll$scaling==0,] 
d_scl1 <- d_sclAll[d_sclAll$scaling==1,]
d_scl2 <- d_sclAll[d_sclAll$scaling==2,] 

# ---- Global variables
arrCategories_measurement <- c("EEE", "EME", "EHE", "MEE", "MME", "MHE", "HEE", "HME", "HHE", "EEM", "EMM", "EHM", "MEM", "MMM", "MHM", "HEM", "HMM", "HHM", "EEH", "EMH", "EHH", "MEH", "MMH", "MHH", "HEH", "HMH", "HHH")
arrCategories_distractor <- c("EEE", "EHE", "HEE", "MMM", "HHH")
arrCategories_scaling <- c("EE", "EH", "HE", "MM", "HH")

boot(d_sclAll$diffA1,samplemean,10000)
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
  cat("genboot: question: ",question,", focus: ", focus," dMask: ", dMask,", dComplex_focus: ", dComplex_focus)
  boot_d <- c()
  if (focus=="" & dMask == "" & dComplex_focus ==""){
      boot_d <- boot(d[[question]],samplemean,R)
  } else if (focus=="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$dMask == dMask],samplemean,R)
  } else if (focus=="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus],samplemean,R)
  } else if (focus=="" & dMask != "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$dComplex_focus == dComplex_focus & d$dMask == dMask],samplemean,R)
  } 
  else if (focus!="" & dMask == "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus],samplemean,R)
  } else if (focus!="" & dMask != "" & dComplex_focus ==""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask],samplemean,R)
  } else if (focus!="" & dMask == "" & dComplex_focus !=""){
    boot_d <- boot(d[[question]][d$focus == focus & d$dComplex_focus == dComplex_focus],samplemean,R)
  } else {
    boot_d <- boot(d[[question]][d$focus == focus & d$dMask == dMask & d$dComplex_focus == dComplex_focus],samplemean,R)
  }

  #   else if (dMask == "" & dComplex_focus ==""){
  #   if (question == "diffA1"){
  #     boot_d <- boot(d$diffA1[d$focus==focus],samplemean,R)
  #   }else if (question == "diffA2"){
  #     boot_d <- boot(d$diffA2[d$focus==focus],samplemean,R)
  #   } else {
  #     boot_d <- boot(d$diffA3[d$focus==focus],samplemean,R)
  #   }
  # } 
  # else if (dMask != "" & dComplex_focus ==""){
  #   if (question == "diffA1"){
  #     boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask],samplemean,R)
  #   }else if (question == "diffA2"){
  #     boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask],samplemean,R)
  #   } else {
  #     boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask],samplemean,R)
  #   }
  # } 
  # else if (dMask == "" & dComplex_focus !=""){
  #   if (question == "diffA1"){
  #     boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   }else if (question == "diffA2"){
  #     boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   } else {
  #     boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   }
  # } 
  # else {
  #   if (question == "diffA1"){
  #     boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   }else if (question == "diffA2"){
  #     boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   } else {
  #     boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
  #   }
  # }
  
  return (boot_d)
}

genBoot(d_sclAll,"diffA2","WHAT_Ql","easy","H")

boot(d_sclAll[["diffA3"]][d_sclAll$dMask == "medium"],samplemean,10000)

getMean_lowCI_highCI <- function (boot_d){
  ci <- boot.ci(boot.out = boot_d, type = c("norm", "basic", "perc", "bca"));
  mean <- boot_d$t0;
  l_ci <- ci$normal[2];
  h_ci <- ci$normal[3];
  res <- c(mean,l_ci,h_ci)
  cat("in getMean_lowCI_highCI, mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  return (res);
}

d_s1 <- d_sclAll[d_sclAll$scaling==1,]
dim(d_s1)

make_gensMean_lowCI_highCI_sclDependent <- function (d,question, focus="", dMask="",dComplex_focus="",R=10000){
  cat("-- make_gensMean_lowCI_highCI_sclDependent; question : ",question,", focus: ",focus,", dMask: ",dMask,", dComplex_focus: ",dComplex_focus);
    # select the data with scale 0
  d_s0 <- d_sclAll[d_sclAll$scaling==0,]
  print("d_s0 selected")
  # boot
  boot_s0 <- genBoot(d_s0,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s0 <- getMean_lowCI_highCI(boot_s0)
  # select the data with scale 1
  d_s1 <- d_sclAll[d_sclAll$scaling==1,]
  print("d_s1 selected");
  # boot
  boot_s1 <- genBoot(d_s1,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s1 <- getMean_lowCI_highCI(boot_s1)
  # select the data with scale 2
  d_s2 <- d_sclAll[d_sclAll$scaling==2,]
  print("d_s2 selected");
  # boot
  boot_s2 <- genBoot(d_s2,question,focus,dMask,dComplex_focus,R)
  # call the summary
  gens_s2 <- getMean_lowCI_highCI(boot_s2)
  return( c(gens_s0,gens_s1,gens_s2) )
}



d_sclAll[d_sclAll$info_focus_dComplex_dMask == "WHAT_Ql_M_E" & d_sclAll$focus=="WHAT_Ql" & d_sclAll$scaling==1,]
d_sclAll[d_sclAll$focus=="WHAT_Ql" & d_sclAll$dMask=="easy" & d_sclAll$dComplex_focus == "M" & d_sclAll$scaling==1,]

boot(d_sclAll$diffA1[d_sclAll$dMask=="easy" & d_sclAll$dComplex_focus=="M" & d_sclAll$scaling==1 & d_sclAll$focus=="WHAT_Ql"],samplemean,10000)
make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA1","WHAT_Ql","","M") # works! # arrData <- list(d_scl0,d_scl1,d_scl2); groupedData_scl0; boots_diffA1_focus_WHAT_Qn <- list(scl_0=genBoot(d_scl0,"diffA1","WHAT_Qn"),scl_1=genBoot(d_scl1,"diffA1","WHAT_Qn"),scl_2=genBoot(d_scl2,"diffA1","WHAT_Qn")); boots_diffA1_focus_WHAT_Qn; as.data.frame(boots_diffA1_focus_WHAT_Qn); ggplot(as.data.frame(vals_scl0_fcs_WHAT_Qn),(aes(x=vals_scl0_fcs_WHAT_Qn[1],y=10))) + geom_point() +  geom_point(data=as.data.frame(vals_scl1_fcs_WHAT_Qn),colour='red') + xlim(-30, 30)

genDifferencesBoot <- function(d,d2,question,focus,dMask="",dComplex_focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  if (dMask == "" & dComplex_focus ==""){
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
  } else if (dMask != "" & dComplex_focus ==""){
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
  } else if (dMask == "" & dComplex_focus !=""){
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dComplex_focus==dComplex_focus],samplemean,R)
    }
  } else {
    if (question == "diffA1"){
      boot_d <- boot(d$diffA1[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA1[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
    }else if (question == "diffA2"){
      boot_d <- boot(d$diffA2[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA2[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
    } else {
      boot_d <- boot(d$diffA3[d$focus==focus & d$dMask==dMask & d$dComplex_focus==dComplex_focus],samplemean,R)
      boot_d2 <- boot(d2$diffA3[d2$focus==focus & d2$dMask==dMask & d2$dComplex_focus==dComplex_focus],samplemean,R)
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
bootQuestionsDifferences <- function(d,d2,question,focus,dMask="",dComplex_focus="",R=10000){
  boot_d <- c();boot_d2 <- c();
  sampleSize <- -1; sampleSize2 <- -1;
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
  
  # calculate the differences and sample proportions...
  sumD <- getMean_lowCI_highCI(boot_d)
  sumD2 <- getMean_lowCI_highCI(boot_d2)
  meanDiff <- sumD[1]-sumD2[1]
  print(meanDiff)
  stdErr_d <- -1;stdErr_d2 <- -1
  stdErr_d <- sd(boot_d$t);stdErr_d2 <- sd(boot_d2$t);
  print(stdErr_d)
  SEM_d <- stdErr_d/sqrt(sampleSize); SEM_d2 <- stdErr_d2/sqrt(sampleSize2);
  print(SEM_d);print(SEM_d2);
  std_Error_Difference <- sqrt(SEM_d*SEM_d + SEM_d2*SEM_d2)
  res <- c(meanDiff, meanDiff - std_Error_Difference, meanDiff + std_Error_Difference)
  return (res)
}


# ---- Test calls
class(d_scl0); d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecA <- d_scl0[d_scl0$focus=="WHAT_Qn",]; d_selecB <- d_scl1[d_scl1$focus=="WHAT_Qn",]; d_selecC <- d_scl2[d_scl2$focus=="WHAT_Qn",]; d_selecA[10,]$cntrQ; dim(d_selecA %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecB %>% distinct(cntrQ,.keep_all=TRUE)); dim(d_selecC %>% distinct(cntrQ,.keep_all=TRUE)) ; unique(d_selecB$cntrQ); dim(d_selecA[d_selecA$cntrQ==381,]); dim(d_selecB[d_selecB$cntrQ==381,]); dim(d_selecC[d_selecC$cntrQ==381,]); 
boot_dTest <- boot(d_selecA$diffA1,samplemean,10000); # boot_dEmpty <- boot(d_selecA$diff42,samplemean,10000); stdErrorTest <- sd(boot_dTest$t); stdErrorTest  # stdErrorTest <- boot_dTest$statistic(stderr())
ciTest <- boot.ci(boot.out = boot_dTest, type = c("norm", "basic", "perc", "bca"));
ci_Diff_scl0_scl2 <-bootQuestionsDifferences(d_scl0,d_scl2,"diffA1","WHAT_Qn")
ci_Diff_scl0_scl2
boot_diffA1_scl0 <-genBoot(d_scl0,"diffA1","WHAT_Qn")
sum_boot_diffA1_scl0 <- getMean_lowCI_highCI((boot_diffA1_scl0))
sum_boot_diffA1_scl0
make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA3","WHAT_Ql")
boot_diffA1_scl1 <-genBoot(d_scl,"diffA1","WHAT_Qn")
sum_boot_diffA1_scl1 <- getMean_lowCI_highCI((boot_diffA1_scl1))
diffsTest <- getDifferencesBoot(boot_diffA1_scl0,boot_diffA1_scl1)
sum_boot_diffA1_scl0
sum_boot_diffA1_scl1
diffsTest

# ---- Data transform
d_scl0<- orderData(d_scl0); d_scl1 <- orderData(d_scl1); d_scl2 <- orderData(d_scl2);

summaryCI_diffA1 <- make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA1","WHAT_Ql")

# group
summ_sclAll_diffA1_WHAT_Qn <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA1","WHAT_Qn")
summ_sclAll_diffA1_WHAT_Ql <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA1","WHAT_Ql")
summ_sclAll_diffA2_WHAT_Qn <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA2","WHAT_Qn")
summ_sclAll_diffA2_WHAT_Ql <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA2","WHAT_Ql")
summ_sclAll_diffA3_WHAT_Qn <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA3","WHAT_Qn")
summ_sclAll_diffA3_WHAT_Ql <-make_gensMean_lowCI_highCI_sclDependent(d_sclAll,"diffA3","WHAT_Ql")

# summ_diffA1_sclAll_WHAT_Qn <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA1","WHAT_Qn"));summ_diffA1_sclAll_WHAT_Ql <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA1","WHAT_Ql"));#getMean_lowCI_highCI(genBoot(d_sclAll,"diffA1","WHERE")); summ_diffA2_sclAll_WHAT_Qn <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA2","WHAT_Qn"));summ_diffA2_sclAll_WHAT_Ql <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA2","WHAT_Ql"));#getMean_lowCI_highCI(genBoot(d_sclAll,"diffA2","WHERE")); summ_diffA3_sclAll_WHAT_Qn <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA3","WHAT_Qn"));summ_diffA3_sclAll_WHAT_Ql <-getMean_lowCI_highCI(genBoot(d_sclAll,"diffA3","WHAT_Ql"));#getMean_lowCI_highCI(genBoot(d_sclAll,"diffA3","WHERE"));

groupedData_all <- d_sclAll %>%
  group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask,scaling) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
  )
groupedData_all["low_ci_DiffA1"] <- NA; groupedData_all["high_ci_DiffA1"] <-NA; 
groupedData_all["low_ci_DiffA2"] <-NA; groupedData_all["high_ci_DiffA2"] <-NA; 
groupedData_all["low_ci_DiffA3"] <-NA; groupedData_all["high_ci_DiffA3"] <-NA;
groupedData_all["mean_t0_DiffA1"]<-NA;groupedData_all["mean_t0_DiffA2"]<-NA;groupedData_all["mean_t0_DiffA3"]<-NA;

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


# diffA1

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

grpdTest <-d_scl0 %>% 
  group_by (trustB)
grpdTest[grpdTest$trustB > 5,]
d_sclAll[d_sclAll$trustB > 5,]

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

# # Rename the categories for readability
# groupedData_all$dMask = as.character(groupedData_all$dMask)
# groupedData_all$dMask[groupedData_all$dMask == "easy"] = "Mask Easy"
# groupedData_all$dMask[groupedData_all$dMask == "medium"] = "Mask Medium"
# groupedData_all$dMask[groupedData_all$dMask == "hard"] = "Mask Hard"
# groupedData_all$dComplex_focus = as.character(groupedData_all$dComplex_focus)
# groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "E"] = "Focus Easy"
# groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "M"] = "Focus Medium"
# groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "H"] = "Focus Hard"
# groupedData_all$orderFocusComplex <- factor(groupedData_all$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
# groupedData_all$orderMaskComplex <- factor(groupedData_all$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

# groupedData_all[groupedData_all$focus=="WHAT_Qn",]$low_ci_DiffA1
# groupedData_all[groupedData_all$focus=="WHAT_Qn",]$low_ci_DiffA2

# ---- Plots
# diffA1 according to scale
groupedPlotDiffA1 <- ggplot(groupedData_all[groupedData_all$focus=="WHAT_Qn",], aes(x=mean_t0_DiffA1,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=low_ci_DiffA1, xmax=high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-20,20))  +
  facet_wrap( ~ dMask , dir="v", ncol=1)
groupedPlotDiffA1
# potential approach: use plot_layout?

# trusts -overAll
plotTrustA1_scl0_overAll <- ggplot(sumTrustA1_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A1", y = "percent", fill = "trustA1") +
  scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 5)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank()) 
plotTrustA1_scl1_overAll <- ggplot(sumTrustA1_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A1", y = "", fill = "trustA1") +
  scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA1_scl2_overAll <- ggplot(sumTrustA1_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A1", y = "", fill = "trustA1") +
  scale_fill_manual("trustA1", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl0_overAll <- ggplot(sumTrustA2_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A2", y = "", fill = "trustA2") +
  scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl1_overAll <- ggplot(sumTrustA2_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A2", y = "", fill = "trustA2") +
  scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA2_scl2_overAll <- ggplot(sumTrustA2_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A2", y = "", fill = "trustA2") +
  scale_fill_manual("trustA2", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())  
plotTrustA3_scl0_overAll <- ggplot(sumTrustA3_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust A3", y = "", fill = "trustA3") +
  scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA3_scl1_overAll <- ggplot(sumTrustA3_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust A3", y = "", fill = "trustA3") +
  scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustA3_scl2_overAll <- ggplot(sumTrustA3_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust A3", y = "", fill = "trustA3") +
  scale_fill_manual("trustA3", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl0_overAll <- ggplot(sumTrustB_scl0_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 0 Trust B", y = "", fill = "trustB") +
  scale_fill_manual("trustB", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl1_overAll <- ggplot(sumTrustB_scl1_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 1 Trust B", y = "", fill = "trustB") +
  scale_fill_manual("trustB", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
  theme_minimal(base_size = 10)+ 
  theme(text = element_text(size = 10), axis.text.x = element_blank())
plotTrustB_scl2_overAll <- ggplot(sumTrustB_scl2_overAll, aes(x=0,y = perc*100, fill = factor(trustB))) + 
  geom_bar(stat="identity", width = 0.4) +
  labs(x = "Scale 2 Trust B", y = "", fill = "trust") +
  scale_fill_manual("trust", values = c("0" = "#771C19", "1" = "orange", "2" = "cyan","3"="#AAAA42","4"="#E25033","5"="purple")) +
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
# diffA1



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



