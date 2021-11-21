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

# original answers
answers_untransformed <- read.table(file="Studies_2021_11_12/Results/measurement_all_headerAdapted.csv",TRUE,",")
length( unique ( answers_untransformed$ResponseId[answers_untransformed$Finished=="True"]) ) # number of answers for measurement
length( unique ( answers_untransformed$ResponseId[answers_untransformed$Finished=="True" & (answers_untransformed$Q11!="Down" | answers_untransformed$Q12!="From time 10 to 30 and time 60 to 70" | answers_untransformed$Q13!="Time 30" | answers_untransformed$Q17!="On the first long straight section" | answers_untransformed$Q14!="From a little before time 70 up to a little before time 80")] ) ) # number of answers for measurement who don't pass first test

# いいい Measurement いいい
# ---- Data loading
# d_measurement_all_noTikTok <- read.table(file="data/transformed/survey_measurement_all_headerAdapted_noTikTok.csv",TRUE,",")
d_measurement_all_noTikTok <- read.table(file="data/transformed/survey_measurement_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
length( unique (d_measurement_all_noTikTok$ResponseId ) )
# d_measurement_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
d_measurement_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
length( unique (d_measurement_all_noTikTok_filteredSemiRigorous$ResponseId ) )

d_measurement_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_measurement_all_noTikTok))
length ( unique(d_measurement_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_measurement_all_noTikTok_enriched$ResponseId[d_measurement_all_noTikTok_enriched$allSameTrust == TRUE | d_measurement_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )

d_distractor_all_noTikTok <- read.table(file="data/transformed/survey_distractor_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
d_distractor_all_noTikTok
d_distractor_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_distractor_all_noTikTok)
d_distractor_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_distractor_all_noTikTok))
length ( unique(d_distractor_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_distractor_all_noTikTok_enriched$ResponseId[d_distractor_all_noTikTok_enriched$allSameTrust == TRUE | d_distractor_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )

d_scaling_all_noTikTok <- read.table(file="data/transformed/survey_scaling_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
d_scaling_all_noTikTok
d_scaling_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_scaling_all_noTikTok)
d_scaling_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_scaling_all_noTikTok))
length ( unique(d_scaling_all_noTikTok_enriched$ResponseId ) )
length ( unique(d_scaling_all_noTikTok_enriched$ResponseId[d_scaling_all_noTikTok_enriched$allSameTrust == TRUE | d_scaling_all_noTikTok_enriched$impossibleQualAnswer == TRUE] ) )

dim(d_measurement_all_noTikTok)
dim(d_distractor_all_noTikTok)
dim(d_scaling_all_noTikTok)

# ---- Confidence intervals # dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask 
dfCI_global_TikTok_measurement_factored <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="dComplex_focus", logFunction=FALSE)

# ---- CorrectB # looks weird with logFunction = TRUE
combine_genPlot_ErrorRate_CIandDifferences( d=d_measurement_all_noTikTok,factorFocus = TRUE, factorDComplex_focus = FALSE, factorDMask = TRUE, factorDifference="dComplex_focus" , logFunction=FALSE)

# Distributions of errors: get the numbers to get the CHI square
correctBdistribution <- df_Distribution_correctB_per_idc(d_measurement_all_noTikTok)
write.csv(correctBdistribution,"distributionsB_measurement_noTikTok.csv",row.names=TRUE)
length(correctBdistribution$dMask[correctBdistribution$dMask == "easy"])

# ---- Trusts
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok)
# # Attempt to run Kruskal-Wallis, but it does not work on socscistatistics.com for more than 300 values in the same block (we have 990)
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA1[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA2[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustB[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])

# --trust according to question type
trust_overAll <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA1, d_measurement_all_noTikTok$trustA2, d_measurement_all_noTikTok$trustA3, d_measurement_all_noTikTok$trustB) )
# -- trust according to focus
trustA1_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA2_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA3_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustB_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="hard"] ) )
# -- trust according to Mask
trustA1_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA2_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA3_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustB_Mask <-  kruskal.test( x= list( d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHAT_Ql"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHAT_Qn"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHERE"] ) )

genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok[d_measurement_all_noTikTok$focus=="WHAT_Qn",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok[d_measurement_all_noTikTok$focus=="WHAT_Ql",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok[d_measurement_all_noTikTok$focus=="WHERE",])

# Not interesting but useful for tests?
# genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok[d_measurement_all_noTikTok$focus=="WHAT_Ql",])


# いいい Distractor いいい 
d_distractor_all_noTikTok <- read.table(file="data/transformed/survey_distractor_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
length( unique (d_distractor_all_noTikTok$ResponseId ) )
length( unique (d_distractor_all_noTikTok$ResponseId[d_distractor_all_noTikTok$distractor=="h"] ) )
length( unique (d_distractor_all_noTikTok$ResponseId[d_distractor_all_noTikTok$distractor=="n"] ) )
d_distractor_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_distractor_all_noTikTok) 
length( unique (d_distractor_all_noTikTok_filteredSemiRigorous$ResponseId ) ) # 18 only!!!


# ---- Confidence intervals # dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask 
dfCI_global_TikTok_distractor_factored <- combine_genPlot_CIandDifferences(
  d_distractor_all_noTikTok,
  factorScaling=FALSE,
  factorDistractor=FALSE,
  factorDMask= FALSE,
  factorFocus=TRUE,
  factorDComplex_focus=FALSE, 
  factorDifference="distractor",
  useLogDiff=TRUE)

# Error rates
d_distractor_all_noTikTok_errorRates <- combine_genPlot_ErrorRate_CIandDifferences( 
  d=d_distractor_all_noTikTok,factorFocus = TRUE, factorDComplex_focus = TRUE, factorDMask = FALSE, factorDifference="distractor" )

d_distractor_all_noTikTok_errorRates2 <- combine_genPlot_ErrorRate_CIandDifferences( 
  d=d_distractor_all_noTikTok,factorFocus = TRUE, factorDComplex_focus = FALSE, factorDMask = TRUE, factorDifference="distractor" )

# Trust
genAndPlotTrust_measurement_overall(d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="h",])
genAndPlotTrust_measurement_overall(d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="n",])
# -- trust according to distractor
trustA1_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="n"]  ) )
trustA2_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="n"] ) )
trustA3_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="n"] ) )
trustB_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="n"] ) )



# いいい Scaling いいい
d_scaling_all_noTikTok <- read.table(file="data/transformed/survey_scaling_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
length( unique (d_scaling_all_noTikTok$ResponseId ) )
d_scaling_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_scaling_all_noTikTok)
length( unique (d_scaling_all_noTikTok_filteredSemiRigorous$ResponseId ) ) # 13 only!!!

dfCombinationCI_differences_test__CIandDiff_distractor_factoredby_focus_dMask <- combine_genPlot_CIandDifferences(d_scaling_all_noTikTok,
                                         factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,
                                         factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="scaling",
                                         useLogDiff=TRUE);

dfCombinationCI_differences_distractor_factoredby_focus <- combine_genPlot_CIandDifferences(d_scaling_all_noTikTok,
                                                                 factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,
                                                                 factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="scaling",
                                                                 useLogDiff=TRUE);


dfCombinationCI_differences_distractor_factoredby_NA <- combine_genPlot_CIandDifferences(d_scaling_all_noTikTok,
                                         factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,
                                         factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="scaling",
                                         useLogDiff=TRUE);

dfCombination_errorRates_dMask_factoredby_scaling_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_scaling_all_noTikTok,
                                       factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,
                                       factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="scaling");


genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==0,])
genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==1,])
genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==2,])


trustA1_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==2]  ) )
trustA2_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==2] ) )
trustA3_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==2] ) )
trustB_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==2] ) )


length ( unique ( d_scaling_all_noTikTok$ResponseId ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==0] ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==1] ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==2] ) )
