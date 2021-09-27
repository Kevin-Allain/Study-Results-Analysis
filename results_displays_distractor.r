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
d_distr_n <- d_distr_all[d_distr_all$distractor == "n",1];
d_distr_h <- d_distr_all[d_distr_all$distractor == "h",1];

# ---- Test calls
dim(d_distr_n)
dim(d_distr_h)
dim(d_distr_n[d_distr_n$cntrQ==381,])
dim(d_distr_h[d_distr_h$cntrQ==381,])

d_selecA <- d_distr_n[d_distr_n$focus=="WHERE",]
d_selecB <- d_distr_h[d_distr_h$focus=="WHERE",]
d_selecA[10,]$cntrQ
dim(d_selecA %>% distinct(cntrQ,.keep_all=TRUE))
dim(d_selecB %>% distinct(cntrQ,.keep_all=TRUE))
unique(d_selecB$cntrQ)
dim(d_selecA[d_selecA$cntrQ==381,])
dim(d_selecB[d_selecB$cntrQ==381,])

d_distr_all[d_distr_all$distractor=="h" & d_distr_all$focus=="WHAT_Ql" & d_distr_all$dComplex_focus == "M",] # empty...!



# ---- Functions

# ---- Calls