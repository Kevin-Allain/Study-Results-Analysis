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
# survey_complete_distractor_n_2021_09_18_headerAdapted.csv
# survey_complete_distractor_h_2021_09_18_headerAdapted.csv
d_distr_n <-  read.table(file="data/transformed/survey_complete_distractor_n_2021_09_18_headerAdapted.csv",TRUE, ",")
d_distr_h <-  read.table(file="data/transformed/survey_complete_distractor_h_2021_09_18_headerAdapted.csv",TRUE, ",")
d_distr_all <-  read.table(file="data/transformed/survey_complete_distractor_all_2021_09_18_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_distr_all <- na.omit(d_distr_all)
d_distr_n <- d_distr_all[d_distr_all$distractor == "n",];
d_distr_h <- d_distr_all[d_distr_all$distractor == "h",];

# ---- Global variables
arrCategories_measurement <- c("EEE", "EME", "EHE", "MEE", "MME", "MHE", "HEE", "HME", "HHE", "EEM", "EMM", "EHM", "MEM", "MMM", "MHM", "HEM", "HMM", "HHM", "EEH", "EMH", "EHH", "MEH", "MMH", "MHH", "HEH", "HMH", "HHH")
arrCategories_distractor <- c("EEE", "EHE", "HEE", "MMM", "HHH")
arrCategories_scaling <- c("EE", "EH", "HE", "MM", "HH")


# ---- Test calls
dim(d_distr_n)
dim(d_distr_h)
dim(d_distr_n[d_distr_n$cntrQ==381,])
dim(d_distr_h[d_distr_h$cntrQ==381,])
d_selecA <- d_distr_n[d_distr_n$focus=="WHERE",]
d_selecB <- d_distr_h[d_distr_h$focus=="WHERE",]
dim(d_selecA %>% distinct(cntrQ,.keep_all=TRUE))
dim(d_selecB %>% distinct(cntrQ,.keep_all=TRUE))
unique(d_selecB$cntrQ)
dim(d_selecA[d_selecA$cntrQ==381,])
dim(d_selecB[d_selecB$cntrQ==381,])
d_distr_all[d_distr_all$distractor=="h" & d_distr_all$focus=="WHAT_Ql" & d_distr_all$dComplex_focus == "M",] # empty... not anymore
testGenNumBoot <- make_gensMean_lowCI_highCI_distractorDependent(d_distr_all,"diffA1","","","")
testGenNumBoot
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
  cat("\n in getMean_lowCI_highCI, mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
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

renameGroupedData <- function(groupedData_all) {  
  if("dMask" %in% colnames(groupedData_all))
  {
    cat("rename for dMask")
    groupedData_all$dMask = as.character(groupedData_all$dMask)
    groupedData_all$dMask[groupedData_all$dMask == "easy"] = "Mask Easy"
    groupedData_all$dMask[groupedData_all$dMask == "medium"] = "Mask Medium"
    groupedData_all$dMask[groupedData_all$dMask == "hard"] = "Mask Hard"
    groupedData_all$orderMaskComplex <- factor(groupedData_all$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
  }
  if ("dComplex_focus" %in% colnames(groupedData_all))
  {
    cat("rename for dComplex_focus")
    groupedData_all$dComplex_focus = as.character(groupedData_all$dComplex_focus)
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "E"] = "Focus Easy"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "M"] = "Focus Medium"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "H"] = "Focus Hard"
    groupedData_all$orderFocusComplex <- factor(groupedData_all$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
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
      cat("scaling")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff)
      # cat("\n ~~~~ in setGroupDataCI, summ_sclAll_diffAx: ",summ_sclAll_diffAx,"\n strMean_t0: ",strMean_t0,", strlow_ci: ",strlow_ci,", strhigh_ci: ",strhigh_ci);
      for (j in 1:3){
        groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
      }
    } 
    else if (scaling & !focus & dMask & !dComplex_focus) {
      cat("scaling X dMask");
      # & groupedData_all$scaling == arrDMask[i]
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,"",arrDMask[i])
        # cat("\n ~~~~ in setGroupDataCI MASK, summ_sclAll_diffAx: ",summ_sclAll_diffAx,"\n strMean_t0: ",strMean_t0,", strlow_ci: ",strlow_ci,", strhigh_ci: ",strhigh_ci,", arrDMask[i]: ",arrDMask[i]);
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
      cat("scaling X focus");
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
      cat("scaling X dComplex_focus");
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
      cat("distractor")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      # cat("\n ~~~~ in setGroupDataCI, summ_sclAll_diffAx: ",summ_sclAll_diffAx,"\n strMean_t0: ",strMean_t0,", strlow_ci: ",strlow_ci,", strhigh_ci: ",strhigh_ci);
      for (j in 1:length(arrDistractor)){
        groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
      }
    } 
    else if (distractor & !focus & dMask & !dComplex_focus) {
      cat("distractor X dMask");
      # & groupedData_all$scaling == arrDMask[i]
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,"",arrDMask[i])
        # cat("\n ~~~~ in setGroupDataCI MASK, summ_sclAll_diffAx: ",summ_sclAll_diffAx,"\n strMean_t0: ",strMean_t0,", strlow_ci: ",strlow_ci,", strhigh_ci: ",strhigh_ci,", arrDMask[i]: ",arrDMask[i]);
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
      cat("distractor X focus X dComplex_focus");
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
      cat("distractor X focus");
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
      cat("distractor X dComplex_focus");
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
      cat("measurment")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      # cat("\n ~~~~ in setGroupDataCI, summ_sclAll_diffAx: ",summ_sclAll_diffAx,"\n strMean_t0: ",strMean_t0,", strlow_ci: ",strlow_ci,", strhigh_ci: ",strhigh_ci);
      groupedData_all[[strMean_t0]] <- rep(summ_sclAll_diffAx[1], dim(groupedData_all)[1])
      groupedData_all[[strlow_ci]] <- rep(summ_sclAll_diffAx[2], dim(groupedData_all)[1])
      groupedData_all[[strhigh_ci]] <- rep(summ_sclAll_diffAx[3], dim(groupedData_all)[1])
    } 
    else if (!scaling & !distractor & !focus & dMask & !dComplex_focus) {
      cat("measurement X dMask");
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
      cat("measurement X focus X dComplex_focus");
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
      cat("measurement X focus");
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
      cat("measurement X dComplex_focus");
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
      cat("category missing?")
    }
  }
  
  return (groupedData_all);
}


genDF_scaling_correctB_dMask_focus <- function (d_scl0,d_scl1,d_scl2){
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

# Check that it doesn't match any non-number
numbers_only <- function(x) !grepl("\\D", x)

d_h <- d_distr_h;d_n <- d_distr_n
# d_distr_n$trustA1
# d_h <- subset(d_h, is.numeric(d_h$trustA1) & is.numeric(d_h$trustA2) & is.numeric(d_h$trustA3) & is.numeric(d_h$trustB) )
# d_n <- subset(d_n, is.numeric(d_n$trustA1) & is.numeric(d_n$trustA2) & is.numeric(d_n$trustA3) & is.numeric(d_n$trustB) )
d_h <- subset(d_h, numbers_only(d_h$trustA1) & numbers_only(d_h$trustA2) & numbers_only(d_h$trustA3) & numbers_only(d_h$trustB))
d_n <- subset(d_n, numbers_only(d_n$trustA1) & numbers_only(d_n$trustA2) & numbers_only(d_n$trustA3) & numbers_only(d_n$trustB))
length(d_h[6,]$trustA1)==1

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

genAndPlotTrust_distractor_overall(d_distr_h,d_distr_n)
# ---- Calls

# plot diffs
# ~~~~ According to mask
groupedData_all <- generateGroupedData(d_distr_all)
groupedData_all <- setGroupDataCI(groupedData_all,d_distr_all,FALSE,TRUE,FALSE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ orderMaskComplex , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ orderMaskComplex , dir="v", ncol=1)
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ orderMaskComplex , dir="v", ncol=1)
groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) #+

# ~~~~ diffAx according to scaling x focus (reminder function attr: groupedData_all,scaling=TRUE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
groupedData_all <- generateGroupedData(d_distr_all)
groupedData_all <- setGroupDataCI(groupedData_all,d_distr_all,FALSE,TRUE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) #+

# ~~~~ diffAx according to scaling x focus x dComplex_focus (reminder function attr: groupedData_all,scaling=TRUE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
groupedData_all <- generateGroupedData(d_distr_all)
groupedData_all <- setGroupDataCI(groupedData_all,d_distr_all,FALSE,TRUE,TRUE,FALSE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-55,55))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-55,55))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=distractor)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-55,55))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
# groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) # buggy. Not sure why...
grid.arrange(groupedPlotDiffA1, groupedPlotDiffA2, groupedPlotDiffA3, ncol=3)



# correctB - According to Mask
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
genAndPlotTrust_distractor_mask(d_h,d_n)

# correctB - According to dComplex_focus
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
genAndPlotTrust_distractor_dcomplex_focus (d_h,d_n)

# correctB - According to focus
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
genAndPlotTrust_distractor_focus(d_h,d_n)
