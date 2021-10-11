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
source("functions_dataStudies.r") 

# ---- Data loading
# survey_complete_distractor_n_2021_09_18_headerAdapted.csv
# survey_complete_distractor_h_2021_09_18_headerAdapted.csv
d_distr_n <-  read.table(file="data/transformed/survey_complete_distractor_n_2021_09_18_headerAdapted.csv",TRUE, ",")
d_distr_h <-  read.table(file="data/transformed/survey_complete_distractor_h_2021_09_18_headerAdapted.csv",TRUE, ",")
d_distr_all <-  read.table(file="data/transformed/survey_complete_distractor_all_2021_09_18_headerAdapted_MMM_replaced.csv",TRUE, ",")
d_distr_all <- na.omit(d_distr_all)
d_distr_all <- filter_getRightParticipants(d_distr_all)
d_distrFiltered <- filter_getRightParticipants(d_distr_all)

d_distr_n <- d_distr_all[d_distr_all$distractor == "n",];
d_distr_h <- d_distr_all[d_distr_all$distractor == "h",];

length(unique(d_distr_all$ResponseId))

# ---- Global variables
arrCategories_measurement <- c("EEE", "EME", "EHE", "MEE", "MME", "MHE", "HEE", "HME", "HHE", "EEM", "EMM", "EHM", "MEM", "MMM", "MHM", "HEM", "HMM", "HHM", "EEH", "EMH", "EHH", "MEH", "MMH", "MHH", "HEH", "HMH", "HHH")
arrCategories_distractor <- c("EEE", "EHE", "HEE", "MMM", "HHH")
arrCategories_scaling <- c("EE", "EH", "HE", "MM", "HH")

# ---- Functions

# genAndPlot_errorRate_correctB_distractor(d_h,d_n)
# genAndPlot_differencesBoot_distractor(d_h,d_n)


# ---- Calls
genAndPlotTrust_distractor_overall(d_distr_h,d_distr_n)

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


# correctB - overAll
df_distractor_correctB_overAll <- genDF_distractor_correctB_overall(d_h,d_n)
plotCorrectB_h_overAll <- ggplot(df_distractor_correctB_overAll[df_distractor_correctB_overAll$distractor=="h",], aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Hidden", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
plotCorrectB_n_overAll <- ggplot(df_distractor_correctB_overAll[df_distractor_correctB_overAll$distractor=="n",], aes(x=0,y = perc*100, fill = factor(correctB))) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Normal", y = "percent", fill = "correctB") +
  theme_minimal(base_size = 14)+ 
  theme(text = element_text(size = 10)) +
  theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
grid_overAll_correctB <- grid.arrange(plotCorrectB_h_overAll, plotCorrectB_n_overAll, ncol=2,
                                      top = textGrob("overall",gp=gpar(fontsize=20,font=3)))


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


# confidence intervals differences
dfCI_global_distractorStudy_distractorXfocus_dMask <- genAndPlot_differences_factorBased(d=d_distr_all,
                                                 factorDifference = "dMask",
                                                 factorScaling = FALSE, 
                                                 factorDistractor = TRUE,
                                                 factorFocus = TRUE,
                                                 factorDMask= FALSE, 
                                                 factorDComplex_focus=FALSE)

dfCI_global_distractorStudy_focus_distractor <- genAndPlot_differences_factorBased(d=d_distr_all,
                                                 factorDifference = "distractor",
                                                 factorScaling = FALSE, 
                                                 factorDistractor = FALSE,
                                                 factorFocus = FALSE,
                                                 factorDMask= FALSE, 
                                                 factorDComplex_focus=FALSE)
