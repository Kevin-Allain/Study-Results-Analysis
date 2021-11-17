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
library(stringr)

setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
source("functions_dataStudies.r") 

# ---- Data loading
# d_scl0 <-  read.table(file="data/transformed/survey_complete_scaling_0_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_scl1 <-  read.table(file="data/transformed/survey_complete_scaling_1_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_scl2 <-  read.table(file="data/transformed/survey_complete_scaling_2_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_sclAll <- read.table(file="data/transformed/survey_complete_scaling_all_2021_09_19_headerAdapted.csv",TRUE, ",")
# d_scl0 <-  read.table(file="data/transformed/survey_complete_scaling_0_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
# d_scl1 <-  read.table(file="data/transformed/survey_complete_scaling_1_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
# d_scl2 <-  read.table(file="data/transformed/survey_complete_scaling_2_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_sclAll <- read.table(file="data/transformed/survey_complete_scaling_all_2021_09_19_headerAdapted_MMM_replaced.csv",TRUE,",")
d_sclAll <- na.omit(d_sclAll)

d_sclAll <- filter_someTrust0or5_impossibleQualAnswer(d_sclAll)
d_sclFiltered <- filter_someTrust0or5_impossibleQualAnswer(d_sclAll)

d_scl0 <- d_sclAll[d_sclAll$scaling==0,] 
d_scl1 <- d_sclAll[d_sclAll$scaling==1,]
d_scl2 <- d_sclAll[d_sclAll$scaling==2,] 

d_scl0$alt_dMask <- NA
arr_alt_dMask <- c()
for(nM in d_scl0$nMasks){
  if (nM <5){
    arr_alt_dMask <- c(arr_alt_dMask,"easy")
  } else if (nM <9){
    arr_alt_dMask <- c(arr_alt_dMask,"medium")
  } else {
    arr_alt_dMask <- c(arr_alt_dMask,"hard")
  }
}

d_scl0$alt_dMask <- arr_alt_dMask
length(d_scl0$dMask[d_scl0$dMask=="easy"])
length(d_scl0$dMask[d_scl0$dMask=="medium"])
length(d_scl0$dMask[d_scl0$dMask=="hard"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="easy"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="easy" & d_scl0$focus=="WHAT_Qn"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHAT_Ql" & d_scl0$dComplex_focus=="E"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHAT_Ql" & d_scl0$dComplex_focus=="M"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHAT_Ql" & d_scl0$dComplex_focus=="H"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="easy" & d_scl0$focus=="WHAT_Ql"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="easy" & d_scl0$focus=="WHERE"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="medium"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="medium" & d_scl0$focus=="WHAT_Qn"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="medium" & d_scl0$focus=="WHAT_Ql"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="medium" & d_scl0$focus=="WHERE"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHAT_Qn"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHAT_Ql"])
length(d_scl0$alt_dMask[d_scl0$alt_dMask=="hard" & d_scl0$focus=="WHERE"])



length(unique(d_sclAll$ResponseId))

# ---- Global variables
arrCategories_measurement <- c("EEE", "EME", "EHE", "MEE", "MME", "MHE", "HEE", "HME", "HHE", "EEM", "EMM", "EHM", "MEM", "MMM", "MHM", "HEM", "HMM", "HHM", "EEH", "EMH", "EHH", "MEH", "MMH", "MHH", "HEH", "HMH", "HHH")
arrCategories_distractor <- c("EEE", "EHE", "HEE", "MMM", "HHH")
arrCategories_scaling <- c("EE", "EH", "HE", "MM", "HH")

# ---- Plots 
dfCI_global_scalingStudy_scalingXfocus_dMask <- genAndPlot_differences_factorBased(d=d_sclAll,factorDifference = "dMask",
                                                          factorScaling = TRUE, 
                                                          factorFocus = TRUE,
                                                          factorDMask= FALSE, 
                                                          factorDComplex_focus=FALSE)

dfCI_global_scalingStudy_scalingXdMask_focus <- genAndPlot_differences_factorBased(d=d_sclAll,factorDifference = "focus",
                                                                              factorScaling = TRUE, 
                                                                              factorFocus = FALSE,
                                                                              factorDMask= TRUE, 
                                                                              factorDComplex_focus=FALSE)

dfCI_global_scalingStudy_focusXdMask_scaling <- genAndPlot_differences_factorBased(d=d_sclAll,factorDifference = "scaling",
                                                                              factorScaling = FALSE, 
                                                                              factorFocus = TRUE,
                                                                              factorDMask= TRUE, 
                                                                              factorDComplex_focus=FALSE)

dfCI_global_scalingStudy_dMask_scaling <- genAndPlot_differences_factorBased(d=d_sclAll,factorDifference = "scaling",
                                                                              factorScaling = FALSE, 
                                                                              factorFocus = FALSE,
                                                                              factorDMask= TRUE, 
                                                                              factorDComplex_focus=FALSE)

dfCI_global_scalingStudy_noFactor_scaling <- genAndPlot_differences_factorBased(d=d_sclAll,factorDifference = "scaling",
                                                                                     factorScaling = FALSE, 
                                                                                     factorFocus = FALSE,
                                                                                     factorDMask= FALSE, 
                                                                                     factorDComplex_focus=FALSE)



# ~~~~ diffAx according to scaling X orderMaskComplex (modified for test: dMask X focus) (reminder function attr: groupedData_all,scaling=TRUE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
# Set up
groupedData_all <- generateGroupedData(d_sclAll)
groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,
                    scaling=TRUE,
                    distractor=FALSE,
                    focus=FALSE,
                    dMask=TRUE,
                    dComplex_focus=FALSE)
# groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,TRUE,FALSE,FALSE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
factorA <- "orderMaskComplex"
factorB <- "focus"
factorC <- "orderFocusComplex"

groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  # facet_wrap( ~ dMask + focus, dir="v",ncol=1)
  facet_wrap( as.formula(paste("~",factorA,"+",factorB)) , dir="v", ncol=1)
groupedPlotDiffA1
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  # facet_wrap( ~ dMask + focus, dir="v",ncol=1)
  facet_wrap( as.formula(paste("~",factorA,"+",factorB)) , dir="v", ncol=1)
groupedPlotDiffA2
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  # facet_wrap( ~ dMask + focus, dir="v",ncol=1)
  facet_wrap( as.formula(paste("~",factorA,"+",factorB)) , dir="v", ncol=1)
groupedPlotDiffA3
# groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3) #+
grid.arrange(groupedPlotDiffA1, groupedPlotDiffA2, groupedPlotDiffA3, ncol=3)
# layout(title="Confidence intervals of the answers according to mask complexity and scaling")

# ~~~~ diffAx according to scaling x focus (reminder function attr: groupedData_all,scaling=TRUE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
genAndPlot_diffAx_scaling_focus <- function (d_sclAll,groupedData_all) {
groupedData_all <- generateGroupedData(d_sclAll)
groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,TRUE,FALSE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=scaling)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) #+
}
genAndPlot_diffAx_scaling_focus(d_sclAll,groupedData_all)

# ~~~~ diffAx according to scaling x focus x dComplex_focus (reminder function attr: groupedData_all,scaling=TRUE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
genAndPlot_diffAx_scaling_focus_dComplex_focus<- function (d_sclAll,groupedData_all){
  groupedData_all <- generateGroupedData(d_sclAll)
  groupedData_all <- setGroupDataCI(groupedData_all,d_sclAll,TRUE,FALSE,TRUE,FALSE,TRUE)
  groupedData_all <- renameGroupedData(groupedData_all)
  groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-50,50))  +
    facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
  groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-50,50))  +
    facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
  groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-50,50))  +
    facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
  # groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) # buggy. Not sure why...
  grid.arrange(groupedPlotDiffA1, groupedPlotDiffA2, groupedPlotDiffA3, ncol=3)
}
genAndPlot_diffAx_scaling_focus_dComplex_focus(d_sclAll,groupedData_all)

# ~~~~ trusts -overAll
genAndPlotTrust_scaling_overall(d_scl0,d_scl1,d_scl2)

# correctB - overAll
df_scaling_correctB_overAll <- genDF_scaling_correctB_overall(d_scl0,d_scl1,d_scl1)
plotCorrectB_scl0_overAll <- ggplot(df_scaling_correctB_overAll[df_scaling_correctB_overAll$scaling==0,], aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 0", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
plotCorrectB_scl1_overAll <- ggplot(df_scaling_correctB_overAll[df_scaling_correctB_overAll$scaling==1,], aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 1", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
plotCorrectB_scl2_overAll <- ggplot(df_scaling_correctB_overAll[df_scaling_correctB_overAll$scaling==2,], aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Scale 2", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
grid_overAll_correctB <- grid.arrange(plotCorrectB_scl0_overAll, plotCorrectB_scl1_overAll, plotCorrectB_scl2_overAll,ncol=3,
                                      top = textGrob("overall",gp=gpar(fontsize=20,font=3)))


# correctB - According to Mask
genAndPlotCorrectB_scaling_mask(d_scl0,d_scl1,d_scl2)

# correctB - According to dComplex_focus
genAndPlotTrust_scaling_dcomplex_focus(d_scl0,d_scl1,d_scl2)

# correctB - According to focus
genAndPlotTrust_scaling_focus(d_scl0,d_scl1,d_scl2)
