# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)

setwd("C:/Users/keval/Documents/Programming/R/Study-Results-Analysis")
source("functions_dataStudies.r")
source("functions_math_boot.r")

# #### Data to load
answers_measurement <- read.table(file="Data_2022_05_12/Outputs/noFilter_survey_measurement_all_headerAdapted_flipWritten_2022_5_12_16.csv",TRUE,",")

answers_measurement_finished <- answers_measurement[answers_measurement$Finished=="TRUE",]

answers_measurement_finished_passedIntro <- answers_measurement_finished[answers_measurement_finished$passedIntroQuestions=="true",]

answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5 <- filter_allTrust0or5_impossibleQualAnswer(answers_measurement_finished_passedIntro)

# #### Adapt string indicating binary values to binary values for R # necessity uncertain
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer=="true"] <- TRUE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer=="false"] <- FALSE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$passedIntroQuestions[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$passedIntroQuestions=="true"] <- TRUE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$passedIntroQuestions[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$passedIntroQuestions=="false"] <- FALSE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$stimuliAllTrust0or5[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$stimuliAllTrust0or5=="true"] <- TRUE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$stimuliAllTrust0or5[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$stimuliAllTrust0or5=="false"] <- FALSE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$responseAllTrust0or5[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$responseAllTrust0or5=="true"] <- TRUE
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$responseAllTrust0or5[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$responseAllTrust0or5=="false"] <- FALSE

answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer[is.na(answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5$impossibleQualAnswer)] <- "x"

answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs <- enrich_absDiffs(answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5)

# removal of a set of responses not previously present
answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same <- 
  answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs[answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs$ResponseId!="R_2xXYR8J3CkW7dME",]

# Previous data for test
d_measurement_all_noTikTok <- read.table(file="data/transformed/noFilter_measurement_all_2022_03_22.csv",TRUE,",")
d_measurement_all_noTikTok <- enrich_absDiffs(d_measurement_all_noTikTok)
d_measurement_all_noTikTok <- enrich_absDiffs(d_measurement_all_noTikTok)
d_measurement_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
d_measurement_all_noTikTok_filteredSemiRigorous_noNeither <- filter_neitherLikert(d_measurement_all_noTikTok_filteredSemiRigorous)
d_noneither_semi_rigorous <- filter_neitherLikert(d_measurement_all_noTikTok_filteredSemiRigorous)
d_measurement_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_measurement_all_noTikTok_filteredSemiRigorous))


# #### The objective of the graphs we wish to produce is to see the confidence intervals and error rates from the responses of the Measurement study
# #### Note that the data should also be exported to facilitate the comparison between our output and the Observable code.
# #### For the sake of clarity in this file we shall only use data collected from participants registered prior to Prolific being viral on TikTok



# ~~~~ Confidence intervals
# consider usage of answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same or answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs
dfCI_measurement_factored <- combine_genPlot_CIandDifferences(answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same,
                                                                          factorScaling=FALSE,
                                                                          factorDistractor=FALSE,
                                                                          factorDMask= FALSE,
                                                                          factorFocus=FALSE,
                                                                          factorDComplex_focus=FALSE,
                                                                          factorDifference="dComplex_focus",
                                                                          logFunction=FALSE,
                                                                          useLogDiff=TRUE)


dfCI_base_and_differences_measurement_factored <- combine_genPlot_CIandDifferences(answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same,
                                                              factorScaling=FALSE,
                                                              factorDistractor=FALSE,
                                                              factorDMask= FALSE,
                                                              factorFocus=TRUE,
                                                              factorDComplex_focus=FALSE,
                                                              factorDifference="dComplex_focus",
                                                              logFunction=FALSE,
                                                              useLogDiff=TRUE)

dfCI_global_TikTok_measurement_factored <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok_filteredSemiRigorous,
                                                                            factorScaling=FALSE,
                                                                            factorDistractor=FALSE,
                                                                            factorDMask= FALSE,
                                                                            factorFocus=FALSE,
                                                                            factorDComplex_focus=FALSE,
                                                                            factorDifference="focus",
                                                                            logFunction=FALSE,
                                                                            useLogDiff=TRUE)


# ~~~~ Confidence intervals according to filter by stability comparison
dfCI_base_and_differences_measurement_factored <- combine_genPlot_CIandDifferences_filterSC(answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same,
                                                                                   factorScaling=FALSE,
                                                                                   factorDistractor=FALSE,
                                                                                   factorDMask= FALSE,
                                                                                   factorFocus=FALSE,
                                                                                   factorDComplex_focus=FALSE,
                                                                                   factorDifference="focus",
                                                                                   logFunction=FALSE,
                                                                                   useLogDiff=TRUE)
arrValsFilter <- c(0,5,10,15,20,25,30,35,40,45,50)
# arrValsFilter <- c(5,10)
dfCI_all <- data.frame()
strFactDiff <- "focus"
boolFactDMask <- FALSE
boolFactFocus <- FALSE
boolFactDComplexFocus <- FALSE
for(i in arrValsFilter){
  cat("\ni: ",i)
  dfCI_I <- combine_genPlot_CIandDifferences_filterSC(d=answers_measurement_finished_passedIntro_noImpossibleQualAnswer_notAllTrust0or5_enrich_absDiffs_same,
                                                        factorFocus = boolFactFocus, 
                                                        factorDComplex_focus = boolFactDComplexFocus, 
                                                        factorDMask = boolFactDMask, 
                                                        factorDifference=strFactDiff,
                                                        useLogDiff=TRUE,                                                                
                                                        filterType = "p",filterValue = i
  )
  dfCI_all <- bind_rows(dfCI_all,dfCI_I) %>% group_by(filterType)
  
}
# data_er <- data.frame(dfER_arr)
strFilename <-paste("confidence_interval_filter_percentile_",strFactDiff,"_boolFactFocus_",boolFactFocus,"_boolFactDComplexFocus_",boolFactDComplexFocus,"_boolFactDMask_",boolFactDMask,".csv",sep="")
write.csv(dfCI_all,paste("Data_2022_05_12/Filtering/",strFilename,sep=""))




# ~~~~ Error rate # TODO old but important to check based on new approach: note that the results are the same for d_measurement_all_noTikTok_filteredSemiRigorous and d_measurement_all_noTikTok_filteredSemiRigorous_noNeither. FIX!
dfci_global_er <- combine_genPlot_ErrorRate_CIandDifferences(d=d_measurement_all_noTikTok_filteredSemiRigorous,
                                                             factorFocus = FALSE, 
                                                             factorDComplex_focus = FALSE, 
                                                             factorDMask = FALSE, 
                                                             factorDifference="focus",
                                                             filterNeither = TRUE
)




# ~~~~ Error rate according to filter by stability comparison
dfci_global_er <- combine_genPlot_ErrorRate_CIandDifferences_filterSC(d=d_measurement_all_noTikTok_filteredSemiRigorous_noNeither,
                                                             factorFocus = FALSE, 
                                                             factorDComplex_focus = FALSE, 
                                                             factorDMask = FALSE, 
                                                             factorDifference="focus",
                                                             filterNeither = TRUE
)


arrValsFilter <- c(0,5,10,15,20,25,30,35,40,45,50)
dfER_all <- data.frame()
strFactDiff <- "dComplex_focus"
boolFactDMask <- FALSE
boolFactFocus <- TRUE
boolFactDComplexFocus <- FALSE
for(i in arrValsFilter){
  cat("\ni: ",i)
  dfER_I <- combine_genPlot_ErrorRate_CIandDifferences_filterSC(d=d_measurement_all_noTikTok_filteredSemiRigorous_noNeither,
                                                                factorFocus = boolFactFocus, 
                                                                factorDComplex_focus = boolFactDComplexFocus, 
                                                                factorDMask = boolFactDMask, 
                                                                factorDifference=strFactDiff,
                                                                filterNeither = TRUE,
                                                                filterType = "p",filterValue = i
  )
  dfER_all <- bind_rows(dfER_all,dfER_I) %>% group_by(filterType)
  
}
strFilename <-paste("error_rate_filter_percentile_",strFactDiff,"_boolFactFocus_",boolFactFocus,"_boolFactDComplexFocus_",boolFactDComplexFocus,"_boolFactDMask_",boolFactDMask,".csv",sep="")
write.csv(dfER_all,paste("Data_2022_05_12/Filtering/",strFilename,sep=""))

# Further TODO
# Add another point for different methods of statistically significant 
# Add indication of the data set used for stimuli?! Sort of done, but looks weird
  

  
  