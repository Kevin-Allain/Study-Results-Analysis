# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
source("functions_dataStudies.r") 

# ---- Data loading
d_alt_f <- read.table(file="data/transformed/survey_complete_measurement_f_2021_09_18_headerAdapted.csv",TRUE,",")
d_alt_nf <- read.table(file="data/transformed/survey_complete_measurement_nf_2021_09_18_headerAdapted.csv",TRUE,",")
d_alt <- read.table(file="data/transformed/survey_complete_measurement_all_2021_09_18_headerAdapted.csv",TRUE, ",")
d_alt <- filter_getRightParticipants(d_alt)
d_measurement_filtered <- filter_getRightParticipants(d_alt)
length(d_alt$ResponseId)
length(unique(d_alt$ResponseId))
length(unique(d_alt_f$ResponseId))
length(unique(d_alt_nf$ResponseId))

# correctB - According to Mask


# correctB - According to dComplex_focus

# correctB - According to focus




# ---- Calls
genAndPlotTrust_measurement_focus(d_alt)
genAndPlotTrust_measurement_dcomplex_focus(d_alt)

genAndPlotTrust_measurement_mask(d_alt)
genAndPlotTrust_measurement_overall(d_alt)
genAndPlot_errorRate_correctB(d_alt)


# Plot diffs

dfCI_global_measurementStudy_focusXdComplex_focus_dMask <- genAndPlot_differences_factorBased(
  d=d_alt,
  factorDifference = "dComplex_focus",
  factorScaling = FALSE, 
  factorDistractor = FALSE,
  factorFocus = TRUE,
  factorDMask= TRUE, 
  factorDComplex_focus=FALSE)

dfCI_global_measurementStudy_dComplex_focusXdMask_focus <- genAndPlot_differences_factorBased(
  d=d_alt,
  factorDifference = "focus",
  factorScaling = FALSE, 
  factorDistractor = FALSE,
  factorFocus = FALSE,
  factorDMask= TRUE, 
  factorDComplex_focus=TRUE)

dfCI_global_measurementStudy_activeForScreenshots <- genAndPlot_differences_factorBased(
  d=d_alt,
  factorDifference = "dComplex_focus",
  factorScaling = FALSE, 
  factorDistractor = FALSE,
  factorFocus = TRUE,
  factorDMask= FALSE, 
  factorDComplex_focus=FALSE)

# ~~~~ diffAx according to scaling x focus (reminder function attr: groupedData_all,scaling=TRUE,0=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE)
groupedData_all <- generateGroupedData(d_alt)
groupedData_all <- setGroupDataCI(groupedData_all,d_alt,FALSE,FALSE,TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=0)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-20,20))  +
  facet_wrap( ~ focus , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=0)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-20,20))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=0)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-20,20))  +
  facet_wrap( ~ focus , dir="v", ncol=1)
groupedPlotDiffA1 + groupedPlotDiffA2 +groupedPlotDiffA3 + plot_layout(ncol = 3, widths = c(1, 1)) #+


# ~~~~ diffAx 
groupedData_all <- generateGroupedData(d_alt)
groupedData_all <- setGroupDataCI (groupedData_all,d_alt,
                                   scaling=FALSE,
                                   distractor=FALSE,
                                   focus=TRUE,
                                   dMask=TRUE,
                                   dComplex_focus=TRUE)
groupedData_all <- renameGroupedData(groupedData_all)
groupedData_all
groupedPlotDiffA1 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA1,y=orderMaskComplex)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA1, xmax=groupedData_all$high_ci_DiffA1)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
groupedPlotDiffA2 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA2,y=orderMaskComplex)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA2, xmax=groupedData_all$high_ci_DiffA2)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
groupedPlotDiffA3 <- ggplot(groupedData_all, aes(x=mean_t0_DiffA3,y=orderMaskComplex)) +
  geom_vline(xintercept = 0) +
  geom_errorbar(aes(xmin=groupedData_all$low_ci_DiffA3, xmax=groupedData_all$high_ci_DiffA3)) +
  geom_point(size=3,col="black",fill="white", shape=1) +
  xlim(c(-30,30))  +
  facet_wrap( ~ focus + orderFocusComplex , dir="v", ncol=1) 
grid.arrange(groupedPlotDiffA1, groupedPlotDiffA2, groupedPlotDiffA3, ncol=3)

