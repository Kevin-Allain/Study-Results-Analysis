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

# ---- Characterization loading
characterizations <- read.table(file="data/transformed/alt_characterization/study_measurement_all_numqualitychanges_filteredFocus_numMask_flip.csv",TRUE, ",")
alt_masks <- read.table(file="data/transformed/alt_characterization/study_alt_masks.csv",TRUE, ",")

# ---- Data loading
d_alt_f <- read.table(file="data/transformed/survey_complete_measurement_f_2021_09_18_headerAdapted.csv",TRUE,",")
d_alt_nf <- read.table(file="data/transformed/survey_complete_measurement_nf_2021_09_18_headerAdapted.csv",TRUE,",")
d_alt <- read.table(file="data/transformed/survey_complete_measurement_all_2021_09_18_headerAdapted.csv",TRUE, ",")
dim(d_alt)
d_alt_fluentAndFirst <- read.table(file="data/transformed/survey_complete_measurement_all_2021_09_18_October_22_headerAdapted.csv",TRUE,",")
dim(d_alt_fluentAndFirst)
d_rigorous_fluent <- filter_getRightParticipants(d_alt)
dim(d_rigorous_fluent)
d_measurement_filtered_fluentAndFirst <- filter_getRightParticipants(d_alt_fluentAndFirst)
dim(d_measurement_filtered_fluentAndFirst)
length(d_alt$ResponseId)
length(d_rigorous_enriched$ResponseId[d_rigorous_enriched$flip=="nf"])
length(unique(d_alt$ResponseId))
length(unique(d_measurement_filtered$ResponseId))
# length(unique(d_alt_f$ResponseId))
# length(unique(d_alt_nf$ResponseId))

# ---- Merge with alt characterization
# jointdataset <- merge(d_alt, characterizations, by = c('flips','idc'),all=TRUE)
# jointdataset <- merge(d_alt, alt_masks, by = c('flips','idc',"focus"),all=TRUE)
jointdataset <- merge(d_alt, alt_masks[c('flips','idc',"focus","alt_mask","numMask","image_name","cntrQ")], by = c('flips','idc',"focus","cntrQ"),all=TRUE)
# jointdataset <- merge(d_alt,unique(alt_masks[c('idc')]),by="idc")
# jointdataset$alt_mask
# dim(alt_masks)
# jointdataset <- cbind(d_alt, alt_masks$alt_mask)
# jointdataset <- d_alt %>% full_join(alt_masks)
# dim(d_alt)
# dim(jointdataset)
jointdataset <- merge(jointdataset, characterizations,by=c("flips","idc","focus","cntrQ"),all=TRUE)


length(jointdataset$ResponseId[jointdataset$alt_mask=="easy"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Qn"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="E"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="M"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="H"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Ql"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="easy"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="medium"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="hard"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHERE"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHERE" & jointdataset$dComplex_focus=="E"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHERE" & jointdataset$dComplex_focus=="M"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="easy" & jointdataset$focus=="WHERE" & jointdataset$dComplex_focus=="H"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="medium"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="medium" & jointdataset$focus=="WHAT_Qn"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="medium" & jointdataset$focus=="WHAT_Ql"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="medium" & jointdataset$focus=="WHERE"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHAT_Qn"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="E"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="M"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHAT_Qn" & jointdataset$dComplex_focus=="H"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHAT_Ql"])
length(jointdataset$ResponseId[jointdataset$alt_mask=="hard" & jointdataset$focus=="WHERE"])

# jointdataset2 <- cbind(d_alt, characterizations)

d_measurment_Sep <- d_alt_fluentAndFirst[unlist(str_split_fixed(d_alt_fluentAndFirst$RecordedDate, "/", 3))[,2] == "09",]
dim(d_measurment_Sep)
d_measurment_Oct <- d_alt_fluentAndFirst[unlist(str_split_fixed(d_alt_fluentAndFirst$RecordedDate, "/", 3))[,2] == "10",]
dim(d_measurment_Oct)

d_alt_fluentAndFirst$alt_dMask <- NA
arr_alt_dMask <- c()
for(nM in d_alt_fluentAndFirst$nMasks){
  if (nM <6){
    arr_alt_dMask <- c(arr_alt_dMask,"easy")
  } else if (nM <10){
    arr_alt_dMask <- c(arr_alt_dMask,"medium")
  } else {
    arr_alt_dMask <- c(arr_alt_dMask,"hard")
  }
}
d_alt_fluentAndFirst$alt_dMask <- arr_alt_dMask
length(d_alt_fluentAndFirst$dMask[d_alt_fluentAndFirst$dMask=="easy"])
length(d_alt_fluentAndFirst$dMask[d_alt_fluentAndFirst$dMask=="medium"])
length(d_alt_fluentAndFirst$dMask[d_alt_fluentAndFirst$dMask=="hard"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="easy"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="easy" & d_alt_fluentAndFirst$focus=="WHAT_Qn"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHAT_Ql" & d_alt_fluentAndFirst$dComplex_focus=="E"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHAT_Ql" & d_alt_fluentAndFirst$dComplex_focus=="M"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHAT_Ql" & d_alt_fluentAndFirst$dComplex_focus=="H"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="easy" & d_alt_fluentAndFirst$focus=="WHAT_Ql"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="easy" & d_alt_fluentAndFirst$focus=="WHERE"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="medium"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="medium" & d_alt_fluentAndFirst$focus=="WHAT_Qn"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="medium" & d_alt_fluentAndFirst$focus=="WHAT_Ql"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="medium" & d_alt_fluentAndFirst$focus=="WHERE"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHAT_Qn"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHAT_Ql"])
length(d_alt_fluentAndFirst$alt_dMask[d_alt_fluentAndFirst$alt_dMask=="hard" & d_alt_fluentAndFirst$focus=="WHERE"])



strSplitDate <- strsplit(d_measurement_filtered_fluentAndFirst$RecordedDate[1],split="/")
typeof(matrix(unlist(strSplitDate))[2,1])
length(d_measurement_filtered_fluentAndFirst$RecordedDate)
length(d_measurement_filtered_fluentAndFirst$RecordedDate[unlist(str_split_fixed(d_measurement_filtered_fluentAndFirst$RecordedDate, "/", 3))[,2] == "09"])
length(d_measurement_filtered_fluentAndFirst$RecordedDate[unlist(str_split_fixed(d_measurement_filtered_fluentAndFirst$RecordedDate, "/", 3))[,2] == "10"])

d_measurement_filtered_fluentAndFirst$RecordedDate[ matrix(unlist (strsplit(d_measurement_filtered_fluentAndFirst$RecordedDate[1],split="/")) ) [2,1] =="09" ]
d_measurement_filtered_fluentAndFirst$RecordedDate[ matrix(unlist (strsplit(d_measurement_filtered_fluentAndFirst$RecordedDate[1],split="/")) ) [2,1] =="10" ]
as.Date(d_measurement_filtered_fluentAndFirst$RecordedDate[1])
# correctB - According to Mask

as.numeric(as.Date("18/09/21 11:33"), as.Date("18/10/21 11:33"), units="days")/(365.25/12)

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

