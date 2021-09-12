# Attempt to play around with R bootstrap resampling. Our objective is to use it on the pilot study we ran where participants had to compare means and variations of two groups depending of conditions matching.
# Heavily influenced by: https://www.cyclismo.org/tutorial/R/confidence.html#id1 and https://www.cyclismo.org/tutorial/R/pValues.html#t-test and https://www.geeksforgeeks.org/bootstrap-confidence-interval-with-r-programming/ and http://www.mayin.org/ajayshah/KB/R/documents/boot.html and https://stackoverflow.com/questions/14069629/how-can-i-plot-data-with-confidence-intervals#14069837

# File:    Confidence_Interval.r
# Author:  Kevin Allain, kevin.allain@city.ac.uk
# Date:    2020-11-17

# install.packages("name of the package")
# install.packages("gridExtra")
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
<<<<<<< HEAD
=======

>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)

# load file # Remember to move in the right folder, based on your own computer
setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
# d <- read.table(file="C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis/Pilot V2 Comparative Task with Conditions_October 19, 2020_12.11 - Tidy_Binary_OutlierRemoved_Enriched.csv", TRUE, ",")
# d <- read.table(file="data/transformed/survey_precise-study_1618325511833.csv", TRUE, ",")
# Example of file with randomly generated data to fill all the categories
# d <- read.table(file="data/transformed/survey_precise-study_randomlyfilled_1618325521753.csv", TRUE, ",")
# Added a new data file with neither randomly generated data nor empty lines
# d <- read.table(file="data/transformed/survey_precise-study_1618754171875.csv", TRUE, ",")

# Example with only 3 answers generated with a slider # No more answerA and correctA. Now it is answerA1,answerA2,answerA3, diffA1,diffA2,diffA3
# d <- read.table(file="data/transformed/survey_precise-study_1620402561191.csv", TRUE, ",")
# survey_precise-study_1628072432964
d <- read.table(file="data/transformed/survey_precise-study_1628960451341.csv", TRUE, ",")
<<<<<<< HEAD

<<<<<<< HEAD
# d_alt <- read.table(file="data/transformed/alt_survey_precise-study_1628960451341.csv", TRUE, ",")
<<<<<<< HEAD
d_alt <- read.table(file="data/transformed/survey_precise-study_1630250403862.csv",TRUE, ",")
=======

### Bootstrapping for testing hypotheses 
# ---- We have to do the bootstraping for each category we wish to run... Could we set this up as an array of categories to do?
set.seed(112358)
n <- length(d$ResponseId) # The number of observations to sample
n.Qn_E <- length(d$ResponseId[d$dComplex_Qn=="E"]) # Number of samples with data complexity Qn as E
n.Qn_M <- length(d$ResponseId[d$dComplex_Qn=="M"]) # Number of samples with data complexity Qn as M
n.Qn_H <- length(d$ResponseId[d$dComplex_Qn=="H"]) # Number of samples with data complexity Qn as H
# ... todo the other categories?
n.focus_WHAT_Qn <- length(d$ResponseId[d$focus=="WHAT_Qn"]) # Number of samples with the focus being WHAT_Qn
n.focus_WHAT_Ql <- length(d$ResponseId[d$focus=="WHAT_Ql"]) # Number of samples with the focus being WHAT_Ql
n.focus_WHERE <- length(d$ResponseId[d$focus=="WHERE"]) # Number of samples with the focus being WHERE
B <- 10000 # The number of bootstraps samples
variable <- d$PageSubmit

BootstrapSamples <- matrix( sample(variable, size=n*B, replace=TRUE), nrow=n, ncol=B)
dimBoot <- dim(BootstrapSamples)

# now, get those bootstrap samples (without loops!) ### IMPORTANT PART 1
# stick each Boot-sample in a column... # Let's make a set of groups of time selection depending on the focus of the question
Boot.focus_WHAT_Qn <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Qn"], size= B*n.focus_WHAT_Qn, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Qn)
Boot.focus_WHAT_Ql <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Ql"], size= B*n.focus_WHAT_Ql, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Ql)
Boot.focus_WHERE <- matrix( sample(d$PageSubmit[d$focus=="WHERE"], size= B*n.focus_WHERE, replace=TRUE), ncol=B, nrow=n.focus_WHERE)


Boot.t_focus_WHAT_Qn <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Qn"], size= B*n.focus_WHAT_Qn, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Qn)
Boot.t_focus_WHAT_Ql <- matrix( sample(d$PageSubmit[d$focus=="WHAT_Ql"], size= B*n.focus_WHAT_Ql, replace=TRUE), ncol=B, nrow=n.focus_WHAT_Ql)
Boot.t_focus_WHERE <- matrix( sample(d$PageSubmit[d$focus=="WHERE"] , size= B*n.focus_WHERE, replace=TRUE), ncol=B, nrow=n.focus_WHERE)
>>>>>>> 440affc (Updated the baseline file.)
=======

# d_alt <- read.table(file="data/transformed/alt_survey_precise-study_1628960451341.csv", TRUE, ",")
d_alt <- read.table(file="data/transformed/survey_precise-study_1630250403862.csv",TRUE, ",")
>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)
=======
# place holder for the data generation. (for the entirety of the )
d_alt <- read.table(file="data/transformed/survey_precise-study_1630250403862_mods.csv",TRUE, ",")
>>>>>>> a4b8459 (Fixed some typos)


# Confidence interval: average +- z score * standard error #### Issue here: our data as it is now doesn't have 
# 95% confidence interval means 2.5% on each side
#todo: add usage of bootstrap instead of d
<<<<<<< HEAD
mean(d$t)
# dim(d$t) # NULL?
dim(d)
=======
mean(d_alt$t)
# dim(d$t) # NULL?
dim(d_alt)
>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)

samplemean <- function(x, d) {
  return(mean(x[d]))
}
<<<<<<< HEAD
bootDuration_in_seconds = boot(d$t, samplemean, R=1000) # 1000 replications
=======
bootDuration_in_seconds = boot(d_alt$t, samplemean, R=1000) # 1000 replications
>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)
plot(bootDuration_in_seconds)

boot_diffA1 = boot(d$diffA1, samplemean, R=1000) # 1000 replications

bootCorrect_B = boot(d$correctB, samplemean, R=1000) # 1000 replications
plot(bootCorrect_B)
# Seems okay up to here

arrMaskDiffs = array(c("easy","medium","hard"))

<<<<<<< HEAD
# Storing all the boots
boot_d_focus_what_qn_DiffA1_mask_easy = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_mask_medium = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_mask_hard = boot(d$diffA1[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_mask_easy = boot(d$diffA2[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_mask_easy = boot(d$diffA3[d$focus=="WHAT_Qn" & d$dMask=="easy"],samplemean,R=10000)



boot_d_focus_what_qn_DiffA1_mask_easy
=======
# Storing all the boots... Should we care about these?! **** This method is very dirty but we are in a rush
# --+-- All the diffs with focus being WHAT_Qn and all focus diffs and all masks
#  easy diff
boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E" ],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)

ci_fcs_what_qn_DiffA1_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_E_mask_e <- ci_fcs_what_qn_DiffA1_diff_E_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_E_mask_e <- ci_fcs_what_qn_DiffA1_diff_E_mask_e$normal[3];
ci_fcs_what_qn_DiffA1_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_E_mask_m <- ci_fcs_what_qn_DiffA1_diff_E_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_E_mask_m <- ci_fcs_what_qn_DiffA1_diff_E_mask_m$normal[3];
ci_fcs_what_qn_DiffA1_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_E_mask_h <- ci_fcs_what_qn_DiffA1_diff_E_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_E_mask_h <- ci_fcs_what_qn_DiffA1_diff_E_mask_h$normal[3];
ci_fcs_what_qn_DiffA2_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_E_mask_e <- ci_fcs_what_qn_DiffA2_diff_E_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_E_mask_e <- ci_fcs_what_qn_DiffA2_diff_E_mask_e$normal[3];
ci_fcs_what_qn_DiffA2_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_E_mask_m <- ci_fcs_what_qn_DiffA2_diff_E_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_E_mask_m <- ci_fcs_what_qn_DiffA2_diff_E_mask_m$normal[3];
ci_fcs_what_qn_DiffA2_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_E_mask_h <- ci_fcs_what_qn_DiffA2_diff_E_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_E_mask_h <- ci_fcs_what_qn_DiffA2_diff_E_mask_h$normal[3];
ci_fcs_what_qn_DiffA3_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_E_mask_e <- ci_fcs_what_qn_DiffA3_diff_E_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_E_mask_e <- ci_fcs_what_qn_DiffA3_diff_E_mask_e$normal[3];
ci_fcs_what_qn_DiffA3_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_E_mask_m <- ci_fcs_what_qn_DiffA3_diff_E_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_E_mask_m <- ci_fcs_what_qn_DiffA3_diff_E_mask_m$normal[3];
ci_fcs_what_qn_DiffA3_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_E_mask_h <- ci_fcs_what_qn_DiffA3_diff_E_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_E_mask_h <- ci_fcs_what_qn_DiffA3_diff_E_mask_h$normal[3];
# boots_fcs_what_qn_diff_E_mask <- c(boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_easy,boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_medium,boot_d_focus_what_qn_DiffA1_diffFocus_E_mask_hard, boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_easy,boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_medium,boot_d_focus_what_qn_DiffA2_diffFocus_E_mask_hard, boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_easy,boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_medium,boot_d_focus_what_qn_DiffA3_diffFocus_E_mask_hard)
# boots_fcs_what_qn_diff_E_mask
# boots_fcs_what_qn_diff_E_mask[4]

# medium diff
boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M" ],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)

ci_fcs_what_qn_DiffA1_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_M_mask_e <- ci_fcs_what_qn_DiffA1_diff_M_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_M_mask_e <- ci_fcs_what_qn_DiffA1_diff_M_mask_e$normal[3];
ci_fcs_what_qn_DiffA1_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_M_mask_m <- ci_fcs_what_qn_DiffA1_diff_M_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_M_mask_m <- ci_fcs_what_qn_DiffA1_diff_M_mask_m$normal[3];
ci_fcs_what_qn_DiffA1_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_M_mask_h <- ci_fcs_what_qn_DiffA1_diff_M_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_M_mask_h <- ci_fcs_what_qn_DiffA1_diff_M_mask_h$normal[3];
ci_fcs_what_qn_DiffA2_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_M_mask_e <- ci_fcs_what_qn_DiffA2_diff_M_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_M_mask_e <- ci_fcs_what_qn_DiffA2_diff_M_mask_e$normal[3];
ci_fcs_what_qn_DiffA2_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_M_mask_m <- ci_fcs_what_qn_DiffA2_diff_M_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_M_mask_m <- ci_fcs_what_qn_DiffA2_diff_M_mask_m$normal[3];
ci_fcs_what_qn_DiffA2_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_M_mask_h <- ci_fcs_what_qn_DiffA2_diff_M_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_M_mask_h <- ci_fcs_what_qn_DiffA2_diff_M_mask_h$normal[3];
ci_fcs_what_qn_DiffA3_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_M_mask_e <- ci_fcs_what_qn_DiffA3_diff_M_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_M_mask_e <- ci_fcs_what_qn_DiffA3_diff_M_mask_e$normal[3];
ci_fcs_what_qn_DiffA3_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_M_mask_m <- ci_fcs_what_qn_DiffA3_diff_M_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_M_mask_m <- ci_fcs_what_qn_DiffA3_diff_M_mask_m$normal[3];
ci_fcs_what_qn_DiffA3_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_M_mask_h <- ci_fcs_what_qn_DiffA3_diff_M_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_M_mask_h <- ci_fcs_what_qn_DiffA3_diff_M_mask_h$normal[3];

# hard diff
boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H" ],samplemean,R=10000)
boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)

ci_fcs_what_qn_DiffA1_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_H_mask_e <- ci_fcs_what_qn_DiffA1_diff_H_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_H_mask_e <- ci_fcs_what_qn_DiffA1_diff_H_mask_e$normal[3];
ci_fcs_what_qn_DiffA1_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_H_mask_m <- ci_fcs_what_qn_DiffA1_diff_H_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_H_mask_m <- ci_fcs_what_qn_DiffA1_diff_H_mask_m$normal[3];
ci_fcs_what_qn_DiffA1_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA1_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA1_diff_H_mask_h <- ci_fcs_what_qn_DiffA1_diff_H_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA1_diff_H_mask_h <- ci_fcs_what_qn_DiffA1_diff_H_mask_h$normal[3];
ci_fcs_what_qn_DiffA2_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_H_mask_e <- ci_fcs_what_qn_DiffA2_diff_H_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_H_mask_e <- ci_fcs_what_qn_DiffA2_diff_H_mask_e$normal[3];
ci_fcs_what_qn_DiffA2_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_H_mask_m <- ci_fcs_what_qn_DiffA2_diff_H_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_H_mask_m <- ci_fcs_what_qn_DiffA2_diff_H_mask_m$normal[3];
ci_fcs_what_qn_DiffA2_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA2_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA2_diff_H_mask_h <- ci_fcs_what_qn_DiffA2_diff_H_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA2_diff_H_mask_h <- ci_fcs_what_qn_DiffA2_diff_H_mask_h$normal[3];
ci_fcs_what_qn_DiffA3_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_H_mask_e <- ci_fcs_what_qn_DiffA3_diff_H_mask_e$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_H_mask_e <- ci_fcs_what_qn_DiffA3_diff_H_mask_e$normal[3];
ci_fcs_what_qn_DiffA3_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_H_mask_m <- ci_fcs_what_qn_DiffA3_diff_H_mask_m$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_H_mask_m <- ci_fcs_what_qn_DiffA3_diff_H_mask_m$normal[3];
ci_fcs_what_qn_DiffA3_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_qn_DiffA3_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_qn_DiffA3_diff_H_mask_h <- ci_fcs_what_qn_DiffA3_diff_H_mask_h$normal[2];
h_ci_fcs_what_qn_DiffA3_diff_H_mask_h <- ci_fcs_what_qn_DiffA3_diff_H_mask_h$normal[3];

# --+-- All the diffs with focus being WHAT_Ql and all masks
#  easy diff
boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E" ],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)

ci_fcs_what_ql_DiffA1_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_E_mask_e <- ci_fcs_what_ql_DiffA1_diff_E_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_E_mask_e <- ci_fcs_what_ql_DiffA1_diff_E_mask_e$normal[3];
ci_fcs_what_ql_DiffA1_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_E_mask_m <- ci_fcs_what_ql_DiffA1_diff_E_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_E_mask_m <- ci_fcs_what_ql_DiffA1_diff_E_mask_m$normal[3];
ci_fcs_what_ql_DiffA1_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_E_mask_h <- ci_fcs_what_ql_DiffA1_diff_E_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_E_mask_h <- ci_fcs_what_ql_DiffA1_diff_E_mask_h$normal[3];
ci_fcs_what_ql_DiffA2_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_E_mask_e <- ci_fcs_what_ql_DiffA2_diff_E_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_E_mask_e <- ci_fcs_what_ql_DiffA2_diff_E_mask_e$normal[3];
ci_fcs_what_ql_DiffA2_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_E_mask_m <- ci_fcs_what_ql_DiffA2_diff_E_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_E_mask_m <- ci_fcs_what_ql_DiffA2_diff_E_mask_m$normal[3];
ci_fcs_what_ql_DiffA2_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_E_mask_h <- ci_fcs_what_ql_DiffA2_diff_E_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_E_mask_h <- ci_fcs_what_ql_DiffA2_diff_E_mask_h$normal[3];
ci_fcs_what_ql_DiffA3_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_E_mask_e <- ci_fcs_what_ql_DiffA3_diff_E_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_E_mask_e <- ci_fcs_what_ql_DiffA3_diff_E_mask_e$normal[3];
ci_fcs_what_ql_DiffA3_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_E_mask_m <- ci_fcs_what_ql_DiffA3_diff_E_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_E_mask_m <- ci_fcs_what_ql_DiffA3_diff_E_mask_m$normal[3];
ci_fcs_what_ql_DiffA3_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_E_mask_h <- ci_fcs_what_ql_DiffA3_diff_E_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_E_mask_h <- ci_fcs_what_ql_DiffA3_diff_E_mask_h$normal[3];
# boots_fcs_what_ql_diff_E_mask <- c(boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_easy,boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_medium,boot_d_focus_what_ql_DiffA1_diffFocus_E_mask_hard, boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_easy,boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_medium,boot_d_focus_what_ql_DiffA2_diffFocus_E_mask_hard, boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_easy,boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_medium,boot_d_focus_what_ql_DiffA3_diffFocus_E_mask_hard)
# boots_fcs_what_ql_diff_E_mask
# boots_fcs_what_ql_diff_E_mask[4]

# medium diff
boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M" ],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)

ci_fcs_what_ql_DiffA1_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_M_mask_e <- ci_fcs_what_ql_DiffA1_diff_M_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_M_mask_e <- ci_fcs_what_ql_DiffA1_diff_M_mask_e$normal[3];
ci_fcs_what_ql_DiffA1_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_M_mask_m <- ci_fcs_what_ql_DiffA1_diff_M_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_M_mask_m <- ci_fcs_what_ql_DiffA1_diff_M_mask_m$normal[3];
ci_fcs_what_ql_DiffA1_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_M_mask_h <- ci_fcs_what_ql_DiffA1_diff_M_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_M_mask_h <- ci_fcs_what_ql_DiffA1_diff_M_mask_h$normal[3];
ci_fcs_what_ql_DiffA2_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_M_mask_e <- ci_fcs_what_ql_DiffA2_diff_M_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_M_mask_e <- ci_fcs_what_ql_DiffA2_diff_M_mask_e$normal[3];
ci_fcs_what_ql_DiffA2_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_M_mask_m <- ci_fcs_what_ql_DiffA2_diff_M_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_M_mask_m <- ci_fcs_what_ql_DiffA2_diff_M_mask_m$normal[3];
ci_fcs_what_ql_DiffA2_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_M_mask_h <- ci_fcs_what_ql_DiffA2_diff_M_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_M_mask_h <- ci_fcs_what_ql_DiffA2_diff_M_mask_h$normal[3];
ci_fcs_what_ql_DiffA3_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_M_mask_e <- ci_fcs_what_ql_DiffA3_diff_M_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_M_mask_e <- ci_fcs_what_ql_DiffA3_diff_M_mask_e$normal[3];
ci_fcs_what_ql_DiffA3_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_M_mask_m <- ci_fcs_what_ql_DiffA3_diff_M_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_M_mask_m <- ci_fcs_what_ql_DiffA3_diff_M_mask_m$normal[3];
ci_fcs_what_ql_DiffA3_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_M_mask_h <- ci_fcs_what_ql_DiffA3_diff_M_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_M_mask_h <- ci_fcs_what_ql_DiffA3_diff_M_mask_h$normal[3];

# hard diff
boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H" ],samplemean,R=10000)
boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHAT_Ql" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)

ci_fcs_what_ql_DiffA1_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_H_mask_e <- ci_fcs_what_ql_DiffA1_diff_H_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_H_mask_e <- ci_fcs_what_ql_DiffA1_diff_H_mask_e$normal[3];
ci_fcs_what_ql_DiffA1_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_H_mask_m <- ci_fcs_what_ql_DiffA1_diff_H_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_H_mask_m <- ci_fcs_what_ql_DiffA1_diff_H_mask_m$normal[3];
ci_fcs_what_ql_DiffA1_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA1_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA1_diff_H_mask_h <- ci_fcs_what_ql_DiffA1_diff_H_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA1_diff_H_mask_h <- ci_fcs_what_ql_DiffA1_diff_H_mask_h$normal[3];
ci_fcs_what_ql_DiffA2_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_H_mask_e <- ci_fcs_what_ql_DiffA2_diff_H_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_H_mask_e <- ci_fcs_what_ql_DiffA2_diff_H_mask_e$normal[3];
ci_fcs_what_ql_DiffA2_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_H_mask_m <- ci_fcs_what_ql_DiffA2_diff_H_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_H_mask_m <- ci_fcs_what_ql_DiffA2_diff_H_mask_m$normal[3];
ci_fcs_what_ql_DiffA2_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA2_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA2_diff_H_mask_h <- ci_fcs_what_ql_DiffA2_diff_H_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA2_diff_H_mask_h <- ci_fcs_what_ql_DiffA2_diff_H_mask_h$normal[3];
ci_fcs_what_ql_DiffA3_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_H_mask_e <- ci_fcs_what_ql_DiffA3_diff_H_mask_e$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_H_mask_e <- ci_fcs_what_ql_DiffA3_diff_H_mask_e$normal[3];
ci_fcs_what_ql_DiffA3_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_H_mask_m <- ci_fcs_what_ql_DiffA3_diff_H_mask_m$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_H_mask_m <- ci_fcs_what_ql_DiffA3_diff_H_mask_m$normal[3];
ci_fcs_what_ql_DiffA3_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_what_ql_DiffA3_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_what_ql_DiffA3_diff_H_mask_h <- ci_fcs_what_ql_DiffA3_diff_H_mask_h$normal[2];
h_ci_fcs_what_ql_DiffA3_diff_H_mask_h <- ci_fcs_what_ql_DiffA3_diff_H_mask_h$normal[3];
# --+-- All the diffs with focus being WHERE and all masks
#  easy diff
boot_d_focus_where_DiffA1_diffFocus_E_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_E_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_E_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_E_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_E_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E" ],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_E_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_E_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_E_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_E_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="E"],samplemean,R=10000)

ci_fcs_where_DiffA1_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_E_mask_e <- ci_fcs_where_DiffA1_diff_E_mask_e$normal[2];
h_ci_fcs_where_DiffA1_diff_E_mask_e <- ci_fcs_where_DiffA1_diff_E_mask_e$normal[3];
ci_fcs_where_DiffA1_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_E_mask_m <- ci_fcs_where_DiffA1_diff_E_mask_m$normal[2];
h_ci_fcs_where_DiffA1_diff_E_mask_m <- ci_fcs_where_DiffA1_diff_E_mask_m$normal[3];
ci_fcs_where_DiffA1_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_E_mask_h <- ci_fcs_where_DiffA1_diff_E_mask_h$normal[2];
h_ci_fcs_where_DiffA1_diff_E_mask_h <- ci_fcs_where_DiffA1_diff_E_mask_h$normal[3];
ci_fcs_where_DiffA2_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_E_mask_e <- ci_fcs_where_DiffA2_diff_E_mask_e$normal[2];
h_ci_fcs_where_DiffA2_diff_E_mask_e <- ci_fcs_where_DiffA2_diff_E_mask_e$normal[3];
ci_fcs_where_DiffA2_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_E_mask_m <- ci_fcs_where_DiffA2_diff_E_mask_m$normal[2];
h_ci_fcs_where_DiffA2_diff_E_mask_m <- ci_fcs_where_DiffA2_diff_E_mask_m$normal[3];
ci_fcs_where_DiffA2_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_E_mask_h <- ci_fcs_where_DiffA2_diff_E_mask_h$normal[2];
h_ci_fcs_where_DiffA2_diff_E_mask_h <- ci_fcs_where_DiffA2_diff_E_mask_h$normal[3];
ci_fcs_where_DiffA3_diff_E_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_E_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_E_mask_e <- ci_fcs_where_DiffA3_diff_E_mask_e$normal[2];
h_ci_fcs_where_DiffA3_diff_E_mask_e <- ci_fcs_where_DiffA3_diff_E_mask_e$normal[3];
ci_fcs_where_DiffA3_diff_E_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_E_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_E_mask_m <- ci_fcs_where_DiffA3_diff_E_mask_m$normal[2];
h_ci_fcs_where_DiffA3_diff_E_mask_m <- ci_fcs_where_DiffA3_diff_E_mask_m$normal[3];
ci_fcs_where_DiffA3_diff_E_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_E_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_E_mask_h <- ci_fcs_where_DiffA3_diff_E_mask_h$normal[2];
h_ci_fcs_where_DiffA3_diff_E_mask_h <- ci_fcs_where_DiffA3_diff_E_mask_h$normal[3];
# boots_fcs_where_diff_E_mask <- c(boot_d_focus_where_DiffA1_diffFocus_E_mask_easy,boot_d_focus_where_DiffA1_diffFocus_E_mask_medium,boot_d_focus_where_DiffA1_diffFocus_E_mask_hard, boot_d_focus_where_DiffA2_diffFocus_E_mask_easy,boot_d_focus_where_DiffA2_diffFocus_E_mask_medium,boot_d_focus_where_DiffA2_diffFocus_E_mask_hard, boot_d_focus_where_DiffA3_diffFocus_E_mask_easy,boot_d_focus_where_DiffA3_diffFocus_E_mask_medium,boot_d_focus_where_DiffA3_diffFocus_E_mask_hard)
# boots_fcs_where_diff_E_mask
# boots_fcs_where_diff_E_mask[4]

# medium diff
boot_d_focus_where_DiffA1_diffFocus_M_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_M_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_M_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_M_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_M_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M" ],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_M_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_M_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_M_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_M_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="M"],samplemean,R=10000)

ci_fcs_where_DiffA1_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_M_mask_e <- ci_fcs_where_DiffA1_diff_M_mask_e$normal[2];
h_ci_fcs_where_DiffA1_diff_M_mask_e <- ci_fcs_where_DiffA1_diff_M_mask_e$normal[3];
ci_fcs_where_DiffA1_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_M_mask_m <- ci_fcs_where_DiffA1_diff_M_mask_m$normal[2];
h_ci_fcs_where_DiffA1_diff_M_mask_m <- ci_fcs_where_DiffA1_diff_M_mask_m$normal[3];
ci_fcs_where_DiffA1_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_M_mask_h <- ci_fcs_where_DiffA1_diff_M_mask_h$normal[2];
h_ci_fcs_where_DiffA1_diff_M_mask_h <- ci_fcs_where_DiffA1_diff_M_mask_h$normal[3];
ci_fcs_where_DiffA2_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_M_mask_e <- ci_fcs_where_DiffA2_diff_M_mask_e$normal[2];
h_ci_fcs_where_DiffA2_diff_M_mask_e <- ci_fcs_where_DiffA2_diff_M_mask_e$normal[3];
ci_fcs_where_DiffA2_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_M_mask_m <- ci_fcs_where_DiffA2_diff_M_mask_m$normal[2];
h_ci_fcs_where_DiffA2_diff_M_mask_m <- ci_fcs_where_DiffA2_diff_M_mask_m$normal[3];
ci_fcs_where_DiffA2_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_M_mask_h <- ci_fcs_where_DiffA2_diff_M_mask_h$normal[2];
h_ci_fcs_where_DiffA2_diff_M_mask_h <- ci_fcs_where_DiffA2_diff_M_mask_h$normal[3];
ci_fcs_where_DiffA3_diff_M_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_M_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_M_mask_e <- ci_fcs_where_DiffA3_diff_M_mask_e$normal[2];
h_ci_fcs_where_DiffA3_diff_M_mask_e <- ci_fcs_where_DiffA3_diff_M_mask_e$normal[3];
ci_fcs_where_DiffA3_diff_M_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_M_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_M_mask_m <- ci_fcs_where_DiffA3_diff_M_mask_m$normal[2];
h_ci_fcs_where_DiffA3_diff_M_mask_m <- ci_fcs_where_DiffA3_diff_M_mask_m$normal[3];
ci_fcs_where_DiffA3_diff_M_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_M_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_M_mask_h <- ci_fcs_where_DiffA3_diff_M_mask_h$normal[2];
h_ci_fcs_where_DiffA3_diff_M_mask_h <- ci_fcs_where_DiffA3_diff_M_mask_h$normal[3];

# hard diff
boot_d_focus_where_DiffA1_diffFocus_H_mask_e <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_H_mask_m <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA1_diffFocus_H_mask_h <- boot(d_alt$diffA1[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_H_mask_e <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy"& d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_H_mask_m <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H" ],samplemean,R=10000)
boot_d_focus_where_DiffA2_diffFocus_H_mask_h <- boot(d_alt$diffA2[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_H_mask_e <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_H_mask_m <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)
boot_d_focus_where_DiffA3_diffFocus_H_mask_h <- boot(d_alt$diffA3[d_alt$focus=="WHERE" & d_alt$dMask=="easy" & d_alt$dComplex_focus=="H"],samplemean,R=10000)

ci_fcs_where_DiffA1_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_H_mask_e <- ci_fcs_where_DiffA1_diff_H_mask_e$normal[2];
h_ci_fcs_where_DiffA1_diff_H_mask_e <- ci_fcs_where_DiffA1_diff_H_mask_e$normal[3];
ci_fcs_where_DiffA1_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_H_mask_m <- ci_fcs_where_DiffA1_diff_H_mask_m$normal[2];
h_ci_fcs_where_DiffA1_diff_H_mask_m <- ci_fcs_where_DiffA1_diff_H_mask_m$normal[3];
ci_fcs_where_DiffA1_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA1_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA1_diff_H_mask_h <- ci_fcs_where_DiffA1_diff_H_mask_h$normal[2];
h_ci_fcs_where_DiffA1_diff_H_mask_h <- ci_fcs_where_DiffA1_diff_H_mask_h$normal[3];
ci_fcs_where_DiffA2_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_H_mask_e <- ci_fcs_where_DiffA2_diff_H_mask_e$normal[2];
h_ci_fcs_where_DiffA2_diff_H_mask_e <- ci_fcs_where_DiffA2_diff_H_mask_e$normal[3];
ci_fcs_where_DiffA2_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_H_mask_m <- ci_fcs_where_DiffA2_diff_H_mask_m$normal[2];
h_ci_fcs_where_DiffA2_diff_H_mask_m <- ci_fcs_where_DiffA2_diff_H_mask_m$normal[3];
ci_fcs_where_DiffA2_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA2_diff_H_mask_h <- ci_fcs_where_DiffA2_diff_H_mask_h$normal[2];
h_ci_fcs_where_DiffA2_diff_H_mask_h <- ci_fcs_where_DiffA2_diff_H_mask_h$normal[3];
ci_fcs_where_DiffA3_diff_H_mask_e <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_H_mask_e, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_H_mask_e <- ci_fcs_where_DiffA3_diff_H_mask_e$normal[2];
h_ci_fcs_where_DiffA3_diff_H_mask_e <- ci_fcs_where_DiffA3_diff_H_mask_e$normal[3];
ci_fcs_where_DiffA3_diff_H_mask_m <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_H_mask_m, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_H_mask_m <- ci_fcs_where_DiffA3_diff_H_mask_m$normal[2];
h_ci_fcs_where_DiffA3_diff_H_mask_m <- ci_fcs_where_DiffA3_diff_H_mask_m$normal[3];
ci_fcs_where_DiffA3_diff_H_mask_h <- boot.ci(boot.out = boot_d_focus_where_DiffA3_diffFocus_H_mask_h, type = c("norm", "basic", "perc", "bca")) ; 
l_ci_fcs_where_DiffA3_diff_H_mask_h <- ci_fcs_where_DiffA3_diff_H_mask_h$normal[2];
h_ci_fcs_where_DiffA3_diff_H_mask_h <- ci_fcs_where_DiffA3_diff_H_mask_h$normal[3];
#  ---- ****

plot(boot_d_focus_where_DiffA3_diffFocus_H_mask_h)
# boot_d_focus_what_qn_DiffA1_mask_easy
# boot_d_focus_what_qn_DiffA1_mask_medium
# boot_d_focus_what_qn_DiffA1_mask_hard
# Trying to include the boot in the groupedData one... So we should extract the calculations of the boot.
boot_d_focus_where_DiffA3_diffFocus_H_mask_h["t0"]
boot.ci(boot.out=boot_d_focus_where_DiffA3_diffFocus_H_mask_h,type=c("norm","basic","perc","bca"))
# # This works... but annoying to do that for the 81 combinations existing. And yet that's what we'll do...
# ci_boot_d_focus_where_DiffA2_diffFocus_H_mask_medium <- boot.ci(boot.out = boot_d_focus_where_DiffA2_diffFocus_H_mask_medium, type = c("norm", "basic", "perc", "bca")) 
# l_ci_boot_d_focus_where_DiffA2_diffFocus_H_mask_medium <- ci_boot_d_focus_where_DiffA2_diffFocus_H_mask_medium$normal[2];
# h_ci_boot_d_focus_where_DiffA2_diffFocus_H_mask_medium <- ci_boot_d_focus_where_DiffA2_diffFocus_H_mask_medium$normal[3];


>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)

# TIME-global Function to find the bootstrap Confidence Intervals 
timeCI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca")) 
lowerTimeCI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
vecTimeCI <- c(lowerTimeCI, higherTimeCI)

bootDuration_in_seconds_WHAT_Qn = boot(d$t[d$focus=="WHAT_Qn"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHAT_Ql = boot(d$t[d$focus=="WHAT_Ql"], samplemean, R=1000) # 1000 replications
bootDuration_in_seconds_WHERE = boot(d$t[d$focus=="WHERE"], samplemean, R=1000) # 1000 replications
timeCI_WHAT_Qn <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Qn, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHAT_Ql <- boot.ci(boot.out = bootDuration_in_seconds_WHAT_Ql, type = c("norm", "basic", "perc", "bca")) 
timeCI_WHERE <- boot.ci(boot.out = bootDuration_in_seconds_WHERE, type = c("norm", "basic", "perc", "bca"))
<<<<<<< HEAD

indexesV <- 1:length(c(timeCI_WHAT_Qn$normal[2],timeCI_WHAT_Qn$normal[3]))
indexesCategories <- 1:3
meansCategories <- c(mean(d$t[d$focus=="WHAT_Qn"]), mean(d$t[d$focus=="WHAT_Ql"]), mean(d$t[d$focus=="WHERE"]))

timeCI_WHAT_Qn$normal[2]

# Display of confidence intervals based on the focus, with the y axis displaying the time taken to answer. Currently using randomly generated data
dfCategories <- data.frame(x= c("WHAT_Qn","WHAT_Ql","WHERE") ,
                           F = meansCategories, 
                           L = c(timeCI_WHAT_Qn$normal[2], timeCI_WHAT_Ql$normal[2], timeCI_WHERE$normal[2]),
                           U = c(timeCI_WHAT_Qn$normal[3], timeCI_WHAT_Ql$normal[3], timeCI_WHERE$normal[3])
                           )

# ?geom_errorbar
# dfCategories doesn't have all the categories of d
ggplot(dfCategories, aes(x = F, y = x)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmax = U, xmin = L) #+ facet_wrap(~dMask)
  )



d$orderFocusAllComplex <- factor( d$info_focus_dComplex_dMask,c("WHAT_Qn_E_E","WHAT_Qn_E_M","WHAT_Qn_E_H","WHAT_Qn_M_E","WHAT_Qn_M_M","WHAT_Qn_M_H","WHAT_Qn_H_E","WHAT_Qn_H_M","WHAT_Qn_H_H", "WHAT_Ql_E_E","WHAT_Ql_E_M","WHAT_Ql_E_H","WHAT_Ql_M_E","WHAT_Ql_M_M","WHAT_Ql_M_H","WHAT_Ql_H_E","WHAT_Ql_H_M","WHAT_Ql_H_H", "WHERE_E_E","WHERE_E_M","WHERE_E_H","WHERE_M_E","WHERE_M_M","WHERE_M_H","WHERE_H_E","WHERE_H_M","WHERE_H_H"))
d$orderAllComplex <- factor(d$info_dComplex_dMask,c("E_E","E_M","E_H","M_E","M_M","M_H","H_E","H_M","H_H") )
# Test for display with facetting
ggplot(d, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_wrap(~orderAllComplex,ncol=1)


# Test for display with facetting
d$orderFocusComplex <- factor(d$dComplex_focus,c("E","M","H"))
d$orderMaskComplex <- factor(d$dMask,c("easy","medium","hard"))
diffA1CI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca"))
lowerdiffA1CI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
ggplot(d, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_grid(orderMaskComplex ~ orderFocusComplex)



# ---- Error bars for questions A1, A2, A3 and trust levels

groupedData <- d_alt %>%
  group_by(focus,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
            ) 
# %>%
#   mutate(percCorrectB = count/sum(count))


groupedData$dMask = as.character(groupedData$dMask)
groupedData$dMask[groupedData$dMask == "easy"] = "Mask Easy"
groupedData$dMask[groupedData$dMask == "medium"] = "Mask Medium"
groupedData$dMask[groupedData$dMask == "hard"] = "Mask Hard"
groupedData$dComplex_focus = as.character(groupedData$dComplex_focus)
groupedData$dComplex_focus[groupedData$dComplex_focus == "E"] = "Focus Easy"
groupedData$dComplex_focus[groupedData$dComplex_focus == "M"] = "Focus Medium"
groupedData$dComplex_focus[groupedData$dComplex_focus == "H"] = "Focus Hard"

# groupedData$orderFocusComplex <- factor(groupedData$dComplex_focus,c("E","M","H"))
# groupedData$orderMaskComplex <- factor(groupedData$dMask,c("easy","medium","hard"))
groupedData$orderFocusComplex <- factor(groupedData$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
groupedData$orderMaskComplex <- factor(groupedData$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

groupedPlotDiffA1 <- ggplot(groupedData, aes(x=mean_diffA1,y=focus)) +
  geom_errorbar(aes(xmin=mean_diffA1-se_diffA1, xmax=mean_diffA1+se_diffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

# d_alt[d_alt$dComplex_focus=="E" & d_alt$dMask=="easy"]
# View(groupedData)
groupedPlotDiffA1

# ---- Stacked bar charts for question B and trust levels
d_alt$dMask = as.character(d_alt$dMask)
d_alt$dMask[d_alt$dMask == "easy"] = "Mask Easy"
d_alt$dMask[d_alt$dMask == "medium"] = "Mask Medium"
d_alt$dMask[d_alt$dMask == "hard"] = "Mask Hard"
d_alt$dComplex_focus = as.character(d_alt$dComplex_focus)
d_alt$dComplex_focus[d_alt$dComplex_focus == "E"] = "Focus Easy"
d_alt$dComplex_focus[d_alt$dComplex_focus == "M"] = "Focus Medium"
d_alt$dComplex_focus[d_alt$dComplex_focus == "H"] = "Focus Hard"
d_alt$orderFocusComplex <- factor(d_alt$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
d_alt$orderMaskComplex <- factor(d_alt$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

# Question B
groupedPlotCorrectB <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(correctB)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotCorrectB
# TrustA1
groupedPlotTrustA1 <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(trustA1)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)
groupedPlotTrustA1
=======



# indexesV <- 1:length(c(timeCI_WHAT_Qn$normal[2],timeCI_WHAT_Qn$normal[3])) # useless
# indexesCategories <- 1:3 # useless
meansCategories <- c(mean(d_alt$t[d_alt$focus=="WHAT_Qn"]), mean(d_alt$t[d_alt$focus=="WHAT_Ql"]), mean(d_alt$t[d_alt$focus=="WHERE"]))

timeCI_WHAT_Qn$normal

# Display of confidence intervals based on the focus, with the y axis displaying the time taken to answer. Currently using randomly generated data
dfCategories <- data.frame(x= c("WHAT_Qn","WHAT_Ql","WHERE") ,
                           mC = meansCategories, 
                           ci_low = c(timeCI_WHAT_Qn$normal[2], timeCI_WHAT_Ql$normal[2], timeCI_WHERE$normal[2]),
                           ci_end = c(timeCI_WHAT_Qn$normal[3], timeCI_WHAT_Ql$normal[3], timeCI_WHERE$normal[3])
)

# ?geom_errorbar
# dfCategories doesn't have all the categories of d
ggplot(dfCategories, aes(x = mC, y = x)) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmax = ci_end, xmin = ci_low) #+ facet_wrap(~dMask)
  )

d_alt$orderFocusAllComplex <- factor( d_alt$info_focus_dComplex_dMask,c("WHAT_Qn_E_E","WHAT_Qn_E_M","WHAT_Qn_E_H","WHAT_Qn_M_E","WHAT_Qn_M_M","WHAT_Qn_M_H","WHAT_Qn_H_E","WHAT_Qn_H_M","WHAT_Qn_H_H", "WHAT_Ql_E_E","WHAT_Ql_E_M","WHAT_Ql_E_H","WHAT_Ql_M_E","WHAT_Ql_M_M","WHAT_Ql_M_H","WHAT_Ql_H_E","WHAT_Ql_H_M","WHAT_Ql_H_H", "WHERE_E_E","WHERE_E_M","WHERE_E_H","WHERE_M_E","WHERE_M_M","WHERE_M_H","WHERE_H_E","WHERE_H_M","WHERE_H_H"))
d_alt$orderAllComplex <- factor(d_alt$info_dComplex_dMask,c("E_E","E_M","E_H","M_E","M_M","M_H","H_E","H_M","H_H") )
# Test for display with facetting
ggplot(d_alt, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_wrap(~orderAllComplex,ncol=1)

# checking
# Test for display with facetting
d$orderFocusComplex <- factor(d$dComplex_focus,c("E","M","H"))
d$orderMaskComplex <- factor(d$dMask,c("easy","medium","hard"))
diffA1CI <- boot.ci(boot.out = bootDuration_in_seconds, type = c("norm", "basic", "perc", "bca"))
lowerdiffA1CI <- timeCI$normal[2];higherTimeCI <- timeCI$normal[3]
ggplot(d, aes(x=diffA1,y=focus)) +
  geom_point(size=4,col="black",fill="black") +
  facet_grid(orderMaskComplex ~ orderFocusComplex)


# ---- Attempt to add calculations of the bootstraps values to the d_alt object
class(d_alt)
xTest<-rep(c(42),each=10)
# doubts about whether info_focus_dComplex_dMask is something calculated in the JavaScript code...
numCatEach <- d_alt$info_focus_dComplex_dMask[d_alt$focus == "WHAT_Qn" & d_alt$dMask=="easy" & d_alt$dComplex_focus == "E"]
length(numCatEach)
numCatEach2 <- d_alt$info_focus_dComplex_dMask[d_alt$info_focus_dComplex_dMask=="WHAT_Qn_E_E"]
length(numCatEach2)
# ---- Error bars for questions A1, A2, A3 and trust levels

#  IMPORTANT NOTE: there are apparently 29 cases of info_focus_dComplex_dMaskfocus with 2 repetitions... I don't think this is supposed to happen. Not terrible, but not great...
groupedData <- d_alt %>%
  group_by(info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
  summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
            mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
            mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
            mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
  )

groupedData["low_ci_DiffA1"] <-NA; groupedData["high_ci_DiffA1"] <-NA; groupedData["low_ci_DiffA2"] <-NA; groupedData["high_ci_DiffA2"] <-NA; groupedData["low_ci_DiffA3"] <-NA; groupedData["high_ci_DiffA3"] <-NA;
groupedData["mean_t0_DiffA1"]<-NA;groupedData["mean_t0_DiffA2"]<-NA;groupedData["mean_t0_DiffA3"]<-NA;

# E_E
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- l_ci_fcs_what_qn_DiffA1_diff_E_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- h_ci_fcs_what_qn_DiffA1_diff_E_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- l_ci_fcs_what_qn_DiffA2_diff_E_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- h_ci_fcs_what_qn_DiffA2_diff_E_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- l_ci_fcs_what_qn_DiffA3_diff_E_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_E"] <- h_ci_fcs_what_qn_DiffA3_diff_E_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- l_ci_fcs_what_ql_DiffA1_diff_E_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- h_ci_fcs_what_ql_DiffA1_diff_E_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- l_ci_fcs_what_ql_DiffA2_diff_E_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- h_ci_fcs_what_ql_DiffA2_diff_E_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- l_ci_fcs_what_ql_DiffA3_diff_E_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_E"] <- h_ci_fcs_what_ql_DiffA3_diff_E_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- l_ci_fcs_where_DiffA1_diff_E_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- h_ci_fcs_where_DiffA1_diff_E_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- l_ci_fcs_where_DiffA2_diff_E_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- h_ci_fcs_where_DiffA2_diff_E_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- l_ci_fcs_where_DiffA3_diff_E_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_E"] <- h_ci_fcs_where_DiffA3_diff_E_mask_e;
# E_M
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- l_ci_fcs_what_qn_DiffA1_diff_E_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- h_ci_fcs_what_qn_DiffA1_diff_E_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- l_ci_fcs_what_qn_DiffA2_diff_E_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- h_ci_fcs_what_qn_DiffA2_diff_E_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- l_ci_fcs_what_qn_DiffA3_diff_E_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_M"] <- h_ci_fcs_what_qn_DiffA3_diff_E_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- l_ci_fcs_what_ql_DiffA1_diff_E_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- h_ci_fcs_what_ql_DiffA1_diff_E_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- l_ci_fcs_what_ql_DiffA2_diff_E_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- h_ci_fcs_what_ql_DiffA2_diff_E_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- l_ci_fcs_what_ql_DiffA3_diff_E_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_M"] <- h_ci_fcs_what_ql_DiffA3_diff_E_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- l_ci_fcs_where_DiffA1_diff_E_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- h_ci_fcs_where_DiffA1_diff_E_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- l_ci_fcs_where_DiffA2_diff_E_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- h_ci_fcs_where_DiffA2_diff_E_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- l_ci_fcs_where_DiffA3_diff_E_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_M"] <- h_ci_fcs_where_DiffA3_diff_E_mask_m;
# E_H
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- l_ci_fcs_what_qn_DiffA1_diff_E_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- h_ci_fcs_what_qn_DiffA1_diff_E_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- l_ci_fcs_what_qn_DiffA2_diff_E_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- h_ci_fcs_what_qn_DiffA2_diff_E_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- l_ci_fcs_what_qn_DiffA3_diff_E_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_E_H"] <- h_ci_fcs_what_qn_DiffA3_diff_E_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- l_ci_fcs_what_ql_DiffA1_diff_E_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- h_ci_fcs_what_ql_DiffA1_diff_E_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- l_ci_fcs_what_ql_DiffA2_diff_E_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- h_ci_fcs_what_ql_DiffA2_diff_E_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- l_ci_fcs_what_ql_DiffA3_diff_E_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_E_H"] <- h_ci_fcs_what_ql_DiffA3_diff_E_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- l_ci_fcs_where_DiffA1_diff_E_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- h_ci_fcs_where_DiffA1_diff_E_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- l_ci_fcs_where_DiffA2_diff_E_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- h_ci_fcs_where_DiffA2_diff_E_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- l_ci_fcs_where_DiffA3_diff_E_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_E_H"] <- h_ci_fcs_where_DiffA3_diff_E_mask_h;
# M_E
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- l_ci_fcs_what_qn_DiffA1_diff_M_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- h_ci_fcs_what_qn_DiffA1_diff_M_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- l_ci_fcs_what_qn_DiffA2_diff_M_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- h_ci_fcs_what_qn_DiffA2_diff_M_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- l_ci_fcs_what_qn_DiffA3_diff_M_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_E"] <- h_ci_fcs_what_qn_DiffA3_diff_M_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- l_ci_fcs_what_ql_DiffA1_diff_M_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- h_ci_fcs_what_ql_DiffA1_diff_M_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- l_ci_fcs_what_ql_DiffA2_diff_M_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- h_ci_fcs_what_ql_DiffA2_diff_M_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- l_ci_fcs_what_ql_DiffA3_diff_M_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_E"] <- h_ci_fcs_what_ql_DiffA3_diff_M_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- l_ci_fcs_where_DiffA1_diff_M_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- h_ci_fcs_where_DiffA1_diff_M_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- l_ci_fcs_where_DiffA2_diff_M_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- h_ci_fcs_where_DiffA2_diff_M_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- l_ci_fcs_where_DiffA3_diff_M_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_E"] <- h_ci_fcs_where_DiffA3_diff_M_mask_e;
# M_M
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- l_ci_fcs_what_qn_DiffA1_diff_M_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- h_ci_fcs_what_qn_DiffA1_diff_M_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- l_ci_fcs_what_qn_DiffA2_diff_M_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- h_ci_fcs_what_qn_DiffA2_diff_M_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- l_ci_fcs_what_qn_DiffA3_diff_M_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_M"] <- h_ci_fcs_what_qn_DiffA3_diff_M_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- l_ci_fcs_what_ql_DiffA1_diff_M_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- h_ci_fcs_what_ql_DiffA1_diff_M_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- l_ci_fcs_what_ql_DiffA2_diff_M_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- h_ci_fcs_what_ql_DiffA2_diff_M_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- l_ci_fcs_what_ql_DiffA3_diff_M_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_M"] <- h_ci_fcs_what_ql_DiffA3_diff_M_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- l_ci_fcs_where_DiffA1_diff_M_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- h_ci_fcs_where_DiffA1_diff_M_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- l_ci_fcs_where_DiffA2_diff_M_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- h_ci_fcs_where_DiffA2_diff_M_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- l_ci_fcs_where_DiffA3_diff_M_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_M"] <- h_ci_fcs_where_DiffA3_diff_M_mask_m;
# M_H
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- l_ci_fcs_what_qn_DiffA1_diff_M_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- h_ci_fcs_what_qn_DiffA1_diff_M_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- l_ci_fcs_what_qn_DiffA2_diff_M_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- h_ci_fcs_what_qn_DiffA2_diff_M_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- l_ci_fcs_what_qn_DiffA3_diff_M_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_M_H"] <- h_ci_fcs_what_qn_DiffA3_diff_M_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- l_ci_fcs_what_ql_DiffA1_diff_M_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- h_ci_fcs_what_ql_DiffA1_diff_M_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- l_ci_fcs_what_ql_DiffA2_diff_M_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- h_ci_fcs_what_ql_DiffA2_diff_M_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- l_ci_fcs_what_ql_DiffA3_diff_M_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_M_H"] <- h_ci_fcs_what_ql_DiffA3_diff_M_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- l_ci_fcs_where_DiffA1_diff_M_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- h_ci_fcs_where_DiffA1_diff_M_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- l_ci_fcs_where_DiffA2_diff_M_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- h_ci_fcs_where_DiffA2_diff_M_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- l_ci_fcs_where_DiffA3_diff_M_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_M_H"] <- h_ci_fcs_where_DiffA3_diff_M_mask_h;
# H_E
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- l_ci_fcs_what_qn_DiffA1_diff_H_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- h_ci_fcs_what_qn_DiffA1_diff_H_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- l_ci_fcs_what_qn_DiffA2_diff_H_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- h_ci_fcs_what_qn_DiffA2_diff_H_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- l_ci_fcs_what_qn_DiffA3_diff_H_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_E"] <- h_ci_fcs_what_qn_DiffA3_diff_H_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- l_ci_fcs_what_ql_DiffA1_diff_H_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- h_ci_fcs_what_ql_DiffA1_diff_H_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- l_ci_fcs_what_ql_DiffA2_diff_H_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- h_ci_fcs_what_ql_DiffA2_diff_H_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- l_ci_fcs_what_ql_DiffA3_diff_H_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_E"] <- h_ci_fcs_what_ql_DiffA3_diff_H_mask_e;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- l_ci_fcs_where_DiffA1_diff_H_mask_e; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- h_ci_fcs_where_DiffA1_diff_H_mask_e;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- l_ci_fcs_where_DiffA2_diff_H_mask_e; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- h_ci_fcs_where_DiffA2_diff_H_mask_e;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- l_ci_fcs_where_DiffA3_diff_H_mask_e; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_E"] <- h_ci_fcs_where_DiffA3_diff_H_mask_e;
# H_M
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- l_ci_fcs_what_qn_DiffA1_diff_H_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- h_ci_fcs_what_qn_DiffA1_diff_H_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- l_ci_fcs_what_qn_DiffA2_diff_H_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- h_ci_fcs_what_qn_DiffA2_diff_H_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- l_ci_fcs_what_qn_DiffA3_diff_H_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_M"] <- h_ci_fcs_what_qn_DiffA3_diff_H_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- l_ci_fcs_what_ql_DiffA1_diff_H_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- h_ci_fcs_what_ql_DiffA1_diff_H_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- l_ci_fcs_what_ql_DiffA2_diff_H_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- h_ci_fcs_what_ql_DiffA2_diff_H_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- l_ci_fcs_what_ql_DiffA3_diff_H_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_M"] <- h_ci_fcs_what_ql_DiffA3_diff_H_mask_m;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- l_ci_fcs_where_DiffA1_diff_H_mask_m; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- h_ci_fcs_where_DiffA1_diff_H_mask_m;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- l_ci_fcs_where_DiffA2_diff_H_mask_m; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- h_ci_fcs_where_DiffA2_diff_H_mask_m;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- l_ci_fcs_where_DiffA3_diff_H_mask_m; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_M"] <- h_ci_fcs_where_DiffA3_diff_H_mask_m;
# H_H
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- l_ci_fcs_what_qn_DiffA1_diff_H_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- h_ci_fcs_what_qn_DiffA1_diff_H_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- l_ci_fcs_what_qn_DiffA2_diff_H_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- h_ci_fcs_what_qn_DiffA2_diff_H_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- l_ci_fcs_what_qn_DiffA3_diff_H_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Qn_H_H"] <- h_ci_fcs_what_qn_DiffA3_diff_H_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- l_ci_fcs_what_ql_DiffA1_diff_H_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- h_ci_fcs_what_ql_DiffA1_diff_H_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- l_ci_fcs_what_ql_DiffA2_diff_H_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- h_ci_fcs_what_ql_DiffA2_diff_H_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- l_ci_fcs_what_ql_DiffA3_diff_H_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHAT_Ql_H_H"] <- h_ci_fcs_what_ql_DiffA3_diff_H_mask_h;
groupedData$low_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- l_ci_fcs_where_DiffA1_diff_H_mask_h; groupedData$high_ci_DiffA1[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- h_ci_fcs_where_DiffA1_diff_H_mask_h;
groupedData$low_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- l_ci_fcs_where_DiffA2_diff_H_mask_h; groupedData$high_ci_DiffA2[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- h_ci_fcs_where_DiffA2_diff_H_mask_h;
groupedData$low_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- l_ci_fcs_where_DiffA3_diff_H_mask_h; groupedData$high_ci_DiffA3[groupedData$info_focus_dComplex_dMask=="WHERE_H_H"] <- h_ci_fcs_where_DiffA3_diff_H_mask_h;




# Testing...
# new <- 42
# groupedData_new <- cbind(groupedData,new)
# groupedData['new'] <- new
# modifNew <- -1
# groupedData$new[groupedData$focus=="WHERE"] <- modifNew
# groupedData
# d_alt <- d_alt %>% group_by(focus,dComplex_focus,dMask)
# d_alt_grouped <- d_alt %>% group_by(focus,dComplex_focus,dMask)
# d_alt_grouped 

# Rename the categories for readability
groupedData$dMask = as.character(groupedData$dMask)
groupedData$dMask[groupedData$dMask == "easy"] = "Mask Easy"
groupedData$dMask[groupedData$dMask == "medium"] = "Mask Medium"
groupedData$dMask[groupedData$dMask == "hard"] = "Mask Hard"
groupedData$dComplex_focus = as.character(groupedData$dComplex_focus)
groupedData$dComplex_focus[groupedData$dComplex_focus == "E"] = "Focus Easy"
groupedData$dComplex_focus[groupedData$dComplex_focus == "M"] = "Focus Medium"
groupedData$dComplex_focus[groupedData$dComplex_focus == "H"] = "Focus Hard"
groupedData$orderFocusComplex <- factor(groupedData$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
groupedData$orderMaskComplex <- factor(groupedData$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

# Example of previous approach to store...# geom_errorbar(aes(xmin=mean_diffA1-se_diffA1, xmax=mean_diffA1+se_diffA1)) +
groupedPlotDiffA1 <- ggplot(groupedData, aes(x=mean_diffA1,y=focus)) +
  geom_errorbar(aes(xmin=low_ci_DiffA1, xmax=high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotDiffA2 <- ggplot(groupedData, aes(x=mean_diffA2,y=focus)) +
  geom_errorbar(aes(xmin=low_ci_DiffA2, xmax=high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotDiffA3 <- ggplot(groupedData, aes(x=mean_diffA3,y=focus)) +
  geom_errorbar(aes(xmin=low_ci_DiffA3, xmax=high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

# d_alt[d_alt$dComplex_focus=="E" & d_alt$dMask=="easy"]
# View(groupedData)
groupedPlotDiffA1
grid.arrange(groupedPlotDiffA1, groupedPlotDiffA2, groupedPlotDiffA3, ncol=3)

# ---- Stacked bar charts for question B and trust levels
d_alt$dMask = as.character(d_alt$dMask)
d_alt$dMask[d_alt$dMask == "easy"] = "Mask Easy"
d_alt$dMask[d_alt$dMask == "medium"] = "Mask Medium"
d_alt$dMask[d_alt$dMask == "hard"] = "Mask Hard"
d_alt$dComplex_focus = as.character(d_alt$dComplex_focus)
d_alt$dComplex_focus[d_alt$dComplex_focus == "E"] = "Focus Easy"
d_alt$dComplex_focus[d_alt$dComplex_focus == "M"] = "Focus Medium"
d_alt$dComplex_focus[d_alt$dComplex_focus == "H"] = "Focus Hard"
d_alt$orderFocusComplex <- factor(d_alt$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
d_alt$orderMaskComplex <- factor(d_alt$dMask,c("Mask Easy","Mask Medium","Mask Hard"))

# Question B
groupedPlotCorrectB <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(correctB)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotCorrectB

# TrustA1
groupedPlotTrustA1 <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(trustA1)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotTrustA2 <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(trustA2)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

groupedPlotTrustA3 <- ggplot(d_alt, aes(x=(..count../sum(..count..)), y=focus)) +
  geom_bar(aes(fill=factor(trustA3)),position=position_stack(reverse=TRUE)) +
  theme(legend.position = "top") +
  facet_wrap( ~ orderMaskComplex + orderFocusComplex  , dir="v", ncol=1)

grid.arrange(groupedPlotTrustA1, groupedPlotTrustA2, groupedPlotTrustA3, ncol=3)
>>>>>>> 3314768 (First very dirty but successful approach to use the bootstrap to generate the error bars. We noticed some cases of redundancies of categories which need to be verified again. Still, progress!)

