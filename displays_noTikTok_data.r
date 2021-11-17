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
d_measurement_all_noTikTok <- read.table(file="data/transformed/survey_measurement_all_headerAdapted_noTikTok.csv",TRUE,",")
length( unique (d_measurement_all_noTikTok$ResponseId ) )
d_measurement_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
d_measurement_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_measurement_all_noTikTok))
length ( unique(d_measurement_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_measurement_all_noTikTok_enriched$ResponseId[d_measurement_all_noTikTok_enriched$allSameTrust == TRUE | d_measurement_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )

d_distractor_all_noTikTok <- read.table(file="data/transformed/survey_distractor_all_headerAdapted_noTikTok.csv",TRUE,",")
d_distractor_all_noTikTok
d_distractor_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_distractor_all_noTikTok)
d_distractor_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_distractor_all_noTikTok))
length ( unique(d_distractor_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_distractor_all_noTikTok_enriched$ResponseId[d_distractor_all_noTikTok_enriched$allSameTrust == TRUE | d_distractor_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )


d_scaling_all_noTikTok <- read.table(file="data/transformed/survey_scaling_all_headerAdapted_noTikTok.csv",TRUE,",")
d_scaling_all_noTikTok
d_scaling_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_scaling_all_noTikTok)
d_scaling_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_scaling_all_noTikTok))
length ( unique(d_scaling_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_scaling_all_noTikTok_enriched$ResponseId[d_scaling_all_noTikTok_enriched$allSameTrust == TRUE | d_scaling_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )



dim(d_measurement_all_noTikTok)

dim(d_distractor_all_noTikTok)

dim(d_scaling_all_noTikTok)

# ---- Confidence intervals
dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="dComplex_focus")

# ---- CorrectB
combine_genPlot_ErrorRate_CIandDifferences( d=d_measurement_all_noTikTok,factorFocus = TRUE, factorDComplex_focus = FALSE, factorDMask = TRUE, factorDifference="dComplex_focus" )


# ---- Trusts # only examples for now
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok)

genAndPlotTrust_distractor_mask(d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="h",], d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="n",] )
