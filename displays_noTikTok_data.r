# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
# library(FSA)
# library(see) # buggy...


# adapt this section according to the computer ran on (potential TODO adapt according to current posiiton)
# setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")
setwd("C:/Users/keval/Documents/Programming/R/Study-Results-Analysis")
source("functions_dataStudies.r")

# original answers
answers_untransformed <- read.table(file="Studies_2021_11_12/Results/measurement_all_headerAdapted.csv",TRUE,",")
length( unique ( answers_untransformed$ResponseId[answers_untransformed$Finished=="True"]) ) # number of answers for measurement
length( unique ( answers_untransformed$ResponseId[answers_untransformed$Finished=="True" & (answers_untransformed$Q11!="Down" | answers_untransformed$Q12!="From time 10 to 30 and time 60 to 70" | answers_untransformed$Q13!="Time 30" | answers_untransformed$Q17!="On the first long straight section" | answers_untransformed$Q14!="From a little before time 70 up to a little before time 80")] ) ) # number of answers for measurement who don't pass first test

answers_onlyForIntros <- read.table(file="Studies_2021_11_12/Results/forIntros_all_headerAdapted.csv",TRUE,",")
answers_onlyForIntros_nf <- read.table(file="Studies_2021_11_12/Results/forIntros_all_noflip_headerAdapted.csv",TRUE,",")

length(answers_untransformed$Q11)
length(answers_untransformed$Q11[answers_untransformed$Q11 =="Down" ])

# summary attempts...
length(d_measurement_all_noTikTok_filteredSemiRigorous$abs_diffA1)
sum_d_measurement <- data.frame(unclass(summary(d_measurement_all_noTikTok_filteredSemiRigorous)), check.names = FALSE, stringsAsFactors = FALSE)
View(sum_d_measurement)
sum_d_measurement$`  abs_diffA1`[4]

write.csv(sum_d_measurement , file = 'tst_sum_d_measurement.csv')
summ_load <- read.table(file="tst_sum_d_measurement.csv",TRUE,",")



# differences of stability (condition - no condition) 
diffB_cntrQ <- read.table(file="data/glbl_diffB_cntrQ_all.csv",TRUE,",")

# ~~~~ worst performances
measurement_rigorous_withDiffB <- merge(d_measurement_all_noTikTok_filteredSemiRigorous, diffB_cntrQ)
dim(d_measurement_all_noTikTok_filteredSemiRigorous)
dim(measurement_rigorous_withDiffB)
fcs_ql_measurement_rigorous_withDiffB <- measurement_rigorous_withDiffB[measurement_rigorous_withDiffB$focus=="WHAT_Ql",] ; 
fcs_qn_measurement_rigorous_withDiffB <- measurement_rigorous_withDiffB[measurement_rigorous_withDiffB$focus=="WHAT_Qn",]
fcs_where_measurement_rigorous_withDiffB <- measurement_rigorous_withDiffB[measurement_rigorous_withDiffB$focus=="WHERE",]

# ql A1, A2, A3
d_ql <- fcs_ql_measurement_rigorous_withDiffB[order(as.numeric(as.character(fcs_ql_measurement_rigorous_withDiffB$log_diffA1)) ), ];
arr_ql_CntrQ_focusWorst_A1 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
d_ql <- d_ql[order(as.numeric(as.character(d_ql$log_diffA2)) ), ];
arr_ql_CntrQ_focusWorst_A2 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
d_ql <- d_ql[order(as.numeric(as.character( d_ql$log_diffA3)) ), ];
arr_ql_CntrQ_focusWorst_A3 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
# qn A1, A2, A3
d_qn <- fcs_qn_measurement_rigorous_withDiffB[order(as.numeric(as.character(fcs_qn_measurement_rigorous_withDiffB$log_diffA1)) ), ];
arr_qn_CntrQ_focusWorst_A1 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
d_qn <- d_qn[order(as.numeric(as.character(d_qn$log_diffA2)) ), ];
arr_qn_CntrQ_focusWorst_A2 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
d_qn <- d_qn[order(as.numeric(as.character( d_qn$log_diffA3)) ), ];
arr_qn_CntrQ_focusWorst_A3 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
# where A1, A2, A3
d_where <- fcs_where_measurement_rigorous_withDiffB[order(as.numeric(as.character(fcs_where_measurement_rigorous_withDiffB$log_diffA1)) ), ];
arr_where_CntrQ_focusWorst_A1 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )
d_where <- d_where[order(as.numeric(as.character(d_where$log_diffA2)) ), ];
arr_where_CntrQ_focusWorst_A2 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )
d_where <- d_where[order(as.numeric(as.character( d_where$log_diffA3)) ), ];
arr_where_CntrQ_focusWorst_A3 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )

# worst SC
# 1 - filter neither for SC
d_noneither <- filter_neitherLikert(measurement_rigorous_withDiffB)
# 2 - filter for focus
d_noneither_ql <- d_noneither[d_noneither$focus=="WHAT_Ql",]
d_noneither_qn <- d_noneither[d_noneither$focus=="WHAT_Qn",]
d_noneither_where <- d_noneither[d_noneither$focus=="WHERE",]
# 3 - order the returned df of getDF_WrongBCntrQ and get last 3
df_noneither_ql <- getDF_WrongBCntrQ(d_noneither_ql)
df_noneither_qn <- getDF_WrongBCntrQ(d_noneither_qn)
df_noneither_where <- getDF_WrongBCntrQ(d_noneither_where)

df_noneither_ql <- df_noneither_ql[order(df_noneither_ql$arr_amountWrong), ];
df_noneither_qn <- df_noneither_qn[order(df_noneither_qn$arr_amountWrong), ];
df_noneither_where <- df_noneither_where[order(df_noneither_where$arr_amountWrong), ];

df_noneither_ql <- df_noneither_ql[!duplicated(df_noneither_ql), ]
df_noneither_qn <- df_noneither_qn[!duplicated(df_noneither_qn), ]
df_noneither_where <- df_noneither_where[!duplicated(df_noneither_where), ]



# ~~~~~~ Raw Data Source ~~~~~~
# rawData <- fromJSON(file="allStimuli_rawData.json")
# rawData_WHAT_Ql <- fromJSON(file="allStimuli_rawData_WHAT_Ql.json")
# rawData <- fromJSON(file="glbl_obj_cntrQ_1-486_shape.json")
rawData <- fromJSON(file="glbl_allObjects_2022_03_03.json") 
View(rawData)
rawData[[1]]$bslnA1
rawData[[1]]$bslnA2
# rawData_WHAT_Ql <- rawData[rawData[["focus"]] == "WHAT_Ql"]; # returns an empty list...
# plot(rawData_WHAT_Ql[[1]]$valQl)


# we should mix tables together...?
min_mwm_what_ql <- min(d_measurement_all_noTikTok_filteredSemiRigorous$log_diffA1[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
d_measurement_all_noTikTok_filteredSemiRigorous$cntrQ[d_measurement_all_noTikTok_filteredSemiRigorous$log_diffA1==min_mwm_what_ql]
rawData[[440]]$cntrQ
rawData[[441]]$cntrQ
plot(rawData[[440]]$valQl)


# ~~~~~~ Measurement ~~~~~~
# ---- Data loading
# d_measurement_all_noTikTok <- read.table(file="data/transformed/survey_measurement_all_headerAdapted_noTikTok.csv",TRUE,",")
# d_measurement_all_noTikTok <- read.table(file="data/transformed/survey_measurement_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
d_measurement_all_noTikTok <- read.table(file="data/transformed/noFilter_measurement_all_2022_03_22.csv",TRUE,",")
d_measurement_all_noTikTok <- enrich_absDiffs(d_measurement_all_noTikTok)
length( d_measurement_all_noTikTok$ResponseId)
# d_measurement_all_noTikTok_filteredRigorous <- filter_someTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
d_measurement_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_measurement_all_noTikTok)
View(d_measurement_all_noTikTok_filteredSemiRigorous)
# // Code for JavaScript code // with new data, we could see the prolific id we should keep in our JavaScript code... or can we use ResponseId?
length(d_measurement_all_noTikTok_filteredSemiRigorous$ResponseId)
# make a list of responses that passed the semi rigorous filter
l_all_second_filter <- unique(d_measurement_all_noTikTok$ResponseId)
l_passed_second_filter <- unique(d_measurement_all_noTikTok_filteredSemiRigorous$ResponseId)
l_failed_second_filter <- unique(d_measurement_all_noTikTok$ResponseId[ !d_measurement_all_noTikTok$ResponseId %in% l_passed_second_filter])
# l_second_filter <- vector(mode="list", length=2)
# l_second_filter[[1]] <- c(l_passed_second_filter)
# l_second_filter[[2]] <- rep(TRUE, length(l_passed_second_filter))
left_all_second_filter <- append ( l_passed_second_filter, l_failed_second_filter )
right_all_second_filter <- append ( rep(TRUE, length(l_passed_second_filter)), rep(FALSE, length(l_failed_second_filter)) )
l_second_filter <- vector(mode="list", length=2)
l_second_filter[[1]] <- left_all_second_filter
l_second_filter[[2]] <- right_all_second_filter
jsonSecondFilter <- toJSON(l_second_filter)
write(jsonSecondFilter, "responsesSecondFilter.json")



d_measurement_all_noTikTok_filteredSemiRigorous_noNeither <- filter_neitherLikert(d_measurement_all_noTikTok_filteredSemiRigorous)
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId)
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="easy"] )
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="medium"] )
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="hard"] )

length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="easy" & d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$focus=="WHAT_Ql"] )
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="easy" & d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$focus=="WHAT_Qn"] )
length(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$ResponseId[d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$dMask=="easy" & d_measurement_all_noTikTok_filteredSemiRigorous_noNeither$focus=="WHERE"] )

d_noneither_semi_rigorous <- filter_neitherLikert(d_measurement_all_noTikTok_filteredSemiRigorous)


d_measurement_all_noTikTok_enriched <- enrichData_impossibleQualAnswer(enrichData_withTrustAll0or5(d_measurement_all_noTikTok_filteredSemiRigorous))
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

dfCI_global_TikTok_measurement_factored <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok_filteredSemiRigorous,
                                                                            factorScaling=FALSE,
                                                                            factorDistractor=FALSE,
                                                                            factorDMask= TRUE,
                                                                            factorFocus=TRUE,
                                                                            factorDComplex_focus=FALSE,
                                                                            factorDifference="dComplex_focus",
                                                                            logFunction=FALSE,
                                                                            useLogDiff=TRUE)

dfCI_global_TikTok_measurement_factored_dMaskOnly <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok_filteredSemiRigorous_noNeither,
                                                                                      factorScaling=FALSE,
                                                                                      factorDistractor=FALSE,
                                                                                      factorDMask= TRUE,
                                                                                      factorFocus=TRUE,
                                                                                      factorDComplex_focus=FALSE,
                                                                                      factorDifference="dComplex_focus",
                                                                                      logFunction=FALSE,
                                                                                      useLogDiff=TRUE)

dfCI_global_TikTok_measurement_factored_dComplex_focus_only <- combine_genPlot_CIandDifferences(d_measurement_all_noTikTok_filteredSemiRigorous,
                                                                                                factorScaling=FALSE,
                                                                                                factorDistractor=FALSE,
                                                                                                factorDMask= FALSE,
                                                                                                factorFocus=TRUE,
                                                                                                factorDComplex_focus=FALSE,
                                                                                                factorDifference="dMask",
                                                                                                logFunction=FALSE,
                                                                                                useLogDiff=TRUE)


# ||||\\\\ test about display according to trust...
plot(d_measurement_all_noTikTok_filteredSemiRigorous$log_diffA1,d_measurement_all_noTikTok_filteredSemiRigorous$trustA1)
ggplot(d_measurement_all_noTikTok_filteredSemiRigorous, aes( log_diffA1, trustA1, col= focus, fill = focus )) +    # Plotting with ggplot2 package
  geom_boxplot(alpha=0.3) 

filt_0trust <- d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$trustA1==0,]
ggplot( filt_0trust , aes( x=log_diffA1, y=focus, fill=focus, col=focus )) +    # Plotting with ggplot2 package
  geom_violin( alpha=0.3)  
# +
#   geom_jitter(data=d_measurement_all_noTikTok_filteredSemiRigorous ,  aes (x= log_diffA1 , y = trustA1, fill=focus) ) # d_measurement_all_noTikTok_filteredSemiRigorous, aes(x=log_diffA1, y=trustA1, col=focus)

# performance according to trust... not too bad?
display_res_trust_violin <- gen_res_trust_violin(d_measurement_all_noTikTok_filteredSemiRigorous, 
                                                 factorFocus = TRUE,
                                                 factorDMask = FALSE,
                                                 factorDComplex_focus = TRUE,
                                                 filterNeither = TRUE)

display_res_trust_violin <- gen_res_trust_violin(d_measurement_all_noTikTok_filteredSemiRigorous, factorFocus = TRUE)
display_res_trust_violin <- gen_res_trust_violin(
  d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql",], 
  factorFocus = TRUE, factorDComplex_focus = TRUE)
display_res_trust_violin <- gen_res_trust_violin(d_measurement_all_noTikTok_filteredSemiRigorous, factorFocus = TRUE, factorDMask = TRUE)

# correctB... distribution of correct, neithers, incorrect
distrib_performancesB <- performancesB_correct_neither_incorrect(measurement_rigorous_withDiffB, factorFocus = TRUE, factorDComplex_focus =  TRUE)

# get worst cntrQ # bits of this code is alright, but not the entirety
res_best_worst_cntrQ <- get_best_worst_perfoms_cntrQ(measurement_rigorous_withDiffB)
View(res_best_worst_cntrQ)

View(d_measurement_all_noTikTok_filteredSemiRigorous)
View(dfCI_global_TikTok_measurement_factored)
unique(dfCI_global_TikTok_measurement_factored$orderCategoryCombination)
dfCI_global_TikTok_measurement_factored$orderCategoryCombination[1]

d_measurement_all_noTikTok_filteredSemiRigorous$dComplex_focus[80]
View(renamed_d)
View(dfCI_global_TikTok_measurement_factored)

addedOrderCategory_focusOnly <- prettyEnrichOrderCategory(d_measurement_all_noTikTok_filteredSemiRigorous,dfCI_global_TikTok_measurement_factored_focusOnly)
addedOrderCategory_dMaskOnly <- prettyEnrichOrderCategory(d_measurement_all_noTikTok_filteredSemiRigorous,dfCI_global_TikTok_measurement_factored_dMaskOnly)

unique(addedOrderCategory$orderCategoryCombination)
unique(dfCI_global_TikTok_measurement_factored$orderCategoryCombination)
View(addedOrderCategory)

length(dfCI_global_TikTok_measurement_factored$orderCategoryCombination)
str_detect(dfCI_global_TikTok_measurement_factored$orderCategoryCombination[1] , "focus complexity:" )
selecColName <- "focus"
modifiedColumn <- c()
select(d_measurement_all_noTikTok_filteredSemiRigorous,selecColName)
for (i in select(d_measurement_all_noTikTok_filteredSemiRigorous,selecColName) ){
  modifiedColumn <- c(modifiedColumn, paste( selecColName,": ",i, sep="" ) )
}
modifiedColumn

d_measurement_forViolin <- prettyEnrichOrderCategory(d_measurement_all_noTikTok_filteredSemiRigorous, dfCI_global_TikTok_measurement_factored )
d_measurement_forViolin$orderCategoryCombination
View(d_measurement_forViolin)

# ---- CorrectB # looks weird with logFunction = TRUE
# rabbits TO PRETTIFY


dfci_global_er <- combine_genPlot_ErrorRate_CIandDifferences(d=d_measurement_all_noTikTok_filteredSemiRigorous_noNeither,
                                                             factorFocus = FALSE, 
                                                             factorDComplex_focus = FALSE, 
                                                             factorDMask = FALSE, 
                                                             factorDifference="dComplex_focus",
                                                             filterNeither = TRUE
)

# -- CorrectB filtere of neither # d_measurement_all_noTikTok_filteredSemiRigorous_noNeither
dfci_global_er <- combine_genPlot_ErrorRate_CIandDifferences(d=d_measurement_all_noTikTok_filteredSemiRigorous_noNeither,
                                                             factorFocus = TRUE, 
                                                             factorDComplex_focus = FALSE, 
                                                             factorDMask = TRUE, 
                                                             factorDifference="dComplex_focus",
                                                             filterNeither = TRUE
)


# Distributions of errors: get the numbers to get the CHI square
correctBdistribution <- df_Distribution_correctB_per_idc(d_measurement_all_noTikTok)
write.csv(correctBdistribution,"distributionsB_measurement_noTikTok.csv",row.names=TRUE)
length(correctBdistribution$dMask[correctBdistribution$dMask == "easy"])

# ---- Trusts
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous)
# # Attempt to run Kruskal-Wallis, but it does not work on socscistatistics.com for more than 300 values in the same block (we have 990)
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA1[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA2[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])
# toString(d_measurement_all_noTikTok_filteredSemiRigorous$trustB[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql"])

# --trust according to question type
trust_overAll <- kruskal.test( x= list( d_measurement_all_noTikTok_filteredSemiRigorous$trustA1, 
                                        d_measurement_all_noTikTok_filteredSemiRigorous$trustA2, 
                                        d_measurement_all_noTikTok_filteredSemiRigorous$trustA3, 
                                        d_measurement_all_noTikTok_filteredSemiRigorous$trustB) )
trust_overAll

# dunnTest(data= list( d_measurement_all_noTikTok$trustA1, d_measurement_all_noTikTok$trustA2, d_measurement_all_noTikTok$trustA3, d_measurement_all_noTikTok$trustB))
# -- trust according to focus
trustA1_focus <- kruskal.test( x= list( d_measurement_all_noTikTok_filteredSemiRigorous$trustA1[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"], 
                                        d_measurement_all_noTikTok_filteredSemiRigorous$trustA1[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="medium"],
                                        d_measurement_all_noTikTok_filteredSemiRigorous$trustA1[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="hard"] ) )
trustA1_focus

kruskal.test(trustA1 ~ dMask, data = d_measurement_all_noTikTok_filteredSemiRigorous)

# package not working anymore?
# dunnTest(x = list( d_measurement_all_noTikTok$trustA1, d_measurement_all_noTikTok$trustA2, d_measurement_all_noTikTok$trustA3, d_measurement_all_noTikTok$trustB), g =  ~ dMask)
# this method works
dunnTest( trustA1~focus, data = d_measurement_all_noTikTok_filteredSemiRigorous, method="bonferroni" )


# Strange result here...
mean(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])
mean(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="medium"])
mean(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="hard"])

length(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy" & d_measurement_all_noTikTok_filteredSemiRigorous$trustA3==5])
length(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="medium" & d_measurement_all_noTikTok_filteredSemiRigorous$trustA3==5])
length(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="hard"& d_measurement_all_noTikTok_filteredSemiRigorous$trustA3==5])

plot(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3 )

min(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])
min(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])
min(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])

max(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])
max(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])
max(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy"])


dunnTest( trustA1~focus, data = d_measurement_all_noTikTok_filteredSemiRigorous, method="bonferroni" )
dunnTest( trustA2~focus, data = d_measurement_all_noTikTok_filteredSemiRigorous, method="bonferroni" )
dunnTest( trustA3~focus, data = d_measurement_all_noTikTok_filteredSemiRigorous, method="bonferroni" )
dunnTest( trustB~focus, data = d_measurement_all_noTikTok_filteredSemiRigorous, method="bonferroni" )
dunnTest( trustB~trustA1, data = d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql",], method="bonferroni" )
class(d_measurement_all_noTikTok$trustA1[5])


trustA2_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA3_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustB_focus <- kruskal.test( x= list( d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$dMask=="hard"] ) )
# -- trust according to Mask
trustA1_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA1[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA2_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA2[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustA3_Mask <- kruskal.test( x= list( d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="easy"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="medium"], d_measurement_all_noTikTok$trustA3[d_measurement_all_noTikTok$dMask=="hard"] ) )
trustB_Mask <-  kruskal.test( x= list( d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHAT_Ql"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHAT_Qn"], d_measurement_all_noTikTok$trustB[d_measurement_all_noTikTok$focus=="WHERE"] ) )

genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous)
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Ql",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHAT_Qn",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$focus=="WHERE",])

median(d_measurement_all_noTikTok_filteredSemiRigorous$trustA1)
median(d_measurement_all_noTikTok_filteredSemiRigorous$trustA3)


genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="easy",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="medium",])
genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok_filteredSemiRigorous[d_measurement_all_noTikTok_filteredSemiRigorous$dMask=="hard",])


# Not interesting but useful for tests?
# genAndPlotTrust_measurement_overall(d_measurement_all_noTikTok[d_measurement_all_noTikTok$focus=="WHAT_Ql",])


# ~~~~~~ Distractor ~~~~~~ 
d_distractor_all_noTikTok <- read.table(file="data/transformed/survey_distractor_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
length( unique (d_distractor_all_noTikTok$ResponseId ) )
length( unique (d_distractor_all_noTikTok$ResponseId[d_distractor_all_noTikTok$distractor=="h"] ) )
length( unique (d_distractor_all_noTikTok$ResponseId[d_distractor_all_noTikTok$distractor=="n"] ) )
d_distractor_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_distractor_all_noTikTok) 
length( unique (d_distractor_all_noTikTok_filteredSemiRigorous$ResponseId ) ) # 18 only!!!

d_distractor_all_noTikTok_noneither <- filter_neitherLikert(d_distractor_all_noTikTok)

# res_trust
test_distr <- gen_res_trust_violin(d_distractor_all_noTikTok)
display_res_trust_violin_distractor <- gen_res_trust_violin(d_distractor_all_noTikTok, factorDistractor = TRUE)

# ---- Confidence intervals # dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask 
dfCI_global_TikTok_distractor_factored <- combine_genPlot_CIandDifferences( d_distractor_all_noTikTok,
                                                                            factorScaling=FALSE,
                                                                            factorDistractor=FALSE,
                                                                            factorDMask= FALSE,
                                                                            factorFocus= TRUE,
                                                                            factorDComplex_focus=TRUE,
                                                                            factorDifference="distractor",
                                                                            useLogDiff=TRUE)

# Error rates
d_distractor_all_noTikTok_errorRates <- combine_genPlot_ErrorRate_CIandDifferences( 
  d=d_distractor_all_noTikTok_noneither,
  factorFocus = TRUE,
  factorDComplex_focus = FALSE,
  factorDMask = TRUE,
  factorDifference="distractor", 
  filterNeither = TRUE)

d_distractor_all_noTikTok$correctB[d_distractor_all_noTikTok$focus=="WHAT_Qn" & d_distractor_all_noTikTok$dComplex_focus=="H"]

d_distractor_all_noTikTok_errorRates2 <- combine_genPlot_ErrorRate_CIandDifferences( 
  d=d_distractor_all_noTikTok,
  factorFocus = TRUE, 
  factorDComplex_focus = TRUE, 
  factorDMask = FALSE, 
  factorDifference="distractor",
  filterNeither = TRUE )

# Trust
genAndPlotTrust_measurement_overall(d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="h",])
genAndPlotTrust_measurement_overall(d_distractor_all_noTikTok[d_distractor_all_noTikTok$distractor=="n",])
# -- trust according to distractor
trustA1_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="n"]  ) )
trustA2_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="n"] ) )
trustA3_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="n"] ) )
trustB_distractor <- kruskal.test( x= list( d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="h"], d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="n"] ) )


median(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="h"])
mean(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="h"])
median(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="h"]) - mean(d_distractor_all_noTikTok$trustA1[d_distractor_all_noTikTok$distractor=="n"])

median(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="h"])
mean(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="h"])
median(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="h"]) - mean(d_distractor_all_noTikTok$trustA2[d_distractor_all_noTikTok$distractor=="n"])


median(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="h"])
mean(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="h"])
median(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="h"]) - mean(d_distractor_all_noTikTok$trustA3[d_distractor_all_noTikTok$distractor=="n"])

median(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="h"])
mean(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="h"])
median(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="n"])
mean(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="h"]) - mean(d_distractor_all_noTikTok$trustB[d_distractor_all_noTikTok$distractor=="n"])


# ~~~~~~ Scaling ~~~~~~
d_scaling_all_noTikTok <- read.table(file="data/transformed/survey_scaling_all_headerAdapted_noTikTok_withLog2.csv",TRUE,",")
length( unique (d_scaling_all_noTikTok$ResponseId ) )
d_scaling_all_noTikTok_filteredSemiRigorous <- filter_allTrust0or5_impossibleQualAnswer(d_scaling_all_noTikTok)
length( unique (d_scaling_all_noTikTok_filteredSemiRigorous$ResponseId ) ) # 13 only!!!

display_res_trust_violin <- gen_res_trust_violin(d_scaling_all_noTikTok, factorScaling = TRUE)

d_scaling_all_noTikTok_noneither <- filter_neitherLikert(d_scaling_all_noTikTok)

dfCombinationCI_differences_test__CIandDiff_distractor_factoredby_focus_dMask <- combine_genPlot_CIandDifferences(d_distractor_all_noTikTok,
                                                                                                                  factorScaling=FALSE,factorDistractor=FALSE,
                                                                                                                  factorDMask= FALSE,
                                                                                                                  factorFocus=TRUE,
                                                                                                                  factorDComplex_focus=FALSE, 
                                                                                                                  factorDifference="distractor",
                                                                                                                  useLogDiff=TRUE,
);


dfCombinationCI_differences_distractor_factoredby_focus <- combine_genPlot_CIandDifferences(d_scaling_all_noTikTok,
                                                                                            factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,
                                                                                            factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="scaling",
                                                                                            useLogDiff=TRUE);


dfCombinationCI_differences_distractor_factoredby_NA <- combine_genPlot_CIandDifferences(d_scaling_all_noTikTok,
                                                                                         factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,
                                                                                         factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="scaling",
                                                                                         useLogDiff=TRUE);

dfCombination_errorRates_dMask_factoredby_scaling_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_scaling_all_noTikTok_noneither,
                                                                                                      factorScaling=FALSE, factorDistractor=FALSE,
                                                                                                      factorDMask= FALSE,
                                                                                                      factorFocus=TRUE,
                                                                                                      factorDComplex_focus=TRUE, 
                                                                                                      factorDifference="scaling");


d_scaling_all_noTikTok_errorRates2 <- combine_genPlot_ErrorRate_CIandDifferences( d=d_scaling_all_noTikTok_noneither,
                                                                                  factorFocus = TRUE, 
                                                                                  factorDComplex_focus = FALSE, 
                                                                                  factorDMask = FALSE, 
                                                                                  factorDifference="scaling",
                                                                                  filterNeither = TRUE)

display_scaling_res_trust_violin <- gen_res_trust_violin(d_scaling_all_noTikTok)
display_scaling_res_trust_violin <- gen_res_trust_violin(d_measurement_all_noTikTok_filteredSemiRigorous, factorFocus = TRUE)


genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==0,])
genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==1,])
genAndPlotTrust_measurement_overall(d_scaling_all_noTikTok[d_scaling_all_noTikTok$scaling==2,])

trustA1_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA1[d_scaling_all_noTikTok$scaling==2]  ) )
trustA2_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA2[d_scaling_all_noTikTok$scaling==2] ) )
trustA3_scaling <- kruskal.test( x= list( d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustA3[d_scaling_all_noTikTok$scaling==2] ) )
trustB_scaling <-  kruskal.test( x= list( d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==0], d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==1], d_scaling_all_noTikTok$trustB[d_scaling_all_noTikTok$scaling==2] ) )



length ( unique ( d_scaling_all_noTikTok$ResponseId ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==0] ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==1] ) )
length ( unique ( d_scaling_all_noTikTok$ResponseId[d_scaling_all_noTikTok$scaling==2] ) )
