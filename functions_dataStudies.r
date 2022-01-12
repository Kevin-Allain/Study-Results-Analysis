library(gridExtra)
library(grid)
library(boot) 
library(ggplot2)
library(ggrepel)
library(dplyr)
library(lattice)
library(scales)
library(cowplot)
library(patchwork)
library(stringr)
library(rlist)
# library(simpleaffy)
library(rlang)
library(skimr)
library(agricolae)
# library(ggmap)
# library(plotly) # not our version?!


setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")

# ---- Functions
generateAlternateCategories <- function (d, alt_masks, characterizations){
  jointdataset <- merge(d, alt_masks[c('flips','idc',"focus","alt_mask","numMask","image_name","cntrQ","refined_mask")], by = c('flips','idc',"focus","cntrQ"),all=TRUE);
  jointdataset <- merge(jointdataset, characterizations,by=c("flips","idc","focus","cntrQ"),all=TRUE);
  # modify the old categories...
  jointdataset$dMask <- jointdataset$alt_mask
  jointdataset$alt_diff_qual[jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="easy"] <- "E"
  jointdataset$alt_diff_qual[jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="medium"] <- "M"
  jointdataset$alt_diff_qual[jointdataset$focus=="WHAT_Ql" & jointdataset$alt_diff_qual=="hard"] <- "H"
  jointdataset$dMask <- jointdataset$refined_mask.x
  jointdataset$dComplex_focus[jointdataset$focus=="WHAT_Ql"] <- jointdataset$alt_diff_qual[jointdataset$focus=="WHAT_Ql"]
  return (jointdataset);
}
# jointTest <- generateAlternateCategories(d_alt,alt_masks,characterizations)
# # dim(d_alt); dim(jointTest)
# jointTest$dMask == jointTest$refined_mask.x

filterAccordingToMonth <- function (d,month){
  d_measurment_month <- d[unlist(str_split_fixed(d$RecordedDate, "/", 3))[,2] == month,]
  return (d_measurment_month);
}

# Rename the categories for readability
renameGroupedData <- function(groupedData_all){
  # cat("\n____renameGroupedData, colnames: ",toString(colnames(groupedData_all)))
  # cat("\nAt entry, length(groupedData_all$question): ",length(groupedData_all$question))
  if ("category_combination" %in% colnames(groupedData_all)) {
    # cat("\nRename for category_combination, example: ",groupedData_all$category_combination[1]) # TODO add verifications and changes for other factoring approaches
    if (grepl("Mask",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\n%%%%%%%%\tcase of dMask to change%%%%%%%%\n")
      if (str_count(groupedData_all$category_combination[1],"_") > 0){
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,medium_hard"] = "Mask: Medium-Hard"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,easy_medium"] = "Mask: Easy-Medium"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,easy_hard"] = "Mask: Easy-Hard"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("Mask: Easy-Hard","Mask: Medium-Hard","Mask: Easy-Medium"))
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Medium-Hard"] = "dMask,medium_hard"
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Easy-Medium"] = "dMask,easy_medium"
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Easy-Hard"] = "dMask,easy_hard"
      }
      else {
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,easy"] = "Mask: Easy"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,medium"] = "Mask: Medium"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dMask,hard"] = "Mask: Hard"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("Mask: Hard","Mask: Medium","Mask: Easy"))
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Easy"] = "dMask,easy"
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Medium"] = "dMask,medium"
        groupedData_all$category_combination[groupedData_all$category_combination == "Mask: Hard"] = "dMask,hard"
      }
    }
    else if(grepl("scaling",groupedData_all$category_combination[1],fixed=TRUE)){
      # cat("\ncase of scaling to change")
      if (str_count(groupedData_all$category_combination[1],"_") > 0){
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,0_1"] = "scaling: 0-1"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,0_2"] = "scaling: 0-2"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,1_2"] = "scaling: 1-2"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("scaling: 0-2","scaling: 1-2","scaling: 0-1"))
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 0-1"] = "scaling,0_1"
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 0-2"] = "scaling,0_2"
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 1-2"] = "scaling,1_2"
      } 
      else {
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,0"] = "scaling: 0"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,1"] = "scaling: 1"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "scaling,2"] = "scaling: 2"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("scaling: 2","scaling: 1","scaling: 0"))
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 0"] = "scaling,0"
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 1"] = "scaling,1"
        groupedData_all$category_combination[groupedData_all$category_combination == "scaling: 2"] = "scaling,2"
      }
    } 
    else if(grepl("distractor",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\n\t\t... case of distractor to change")
      if (str_count(groupedData_all$category_combination[1],"_") > 0){
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "distractor,h_n"] = "distractor: h-n"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("distractor: h-n"))
        groupedData_all$category_combination[groupedData_all$category_combination == "distractor: h-n"] = "distractor,h_n"
      } else {
        groupedData_all$category_combination = as.character(groupedData_all$category_combination)
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "distractor,h"] = "distractor: h"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "distractor,n"] = "distractor: n"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("distractor: h","distractor: n"))
        groupedData_all$category_combination[groupedData_all$category_combination == "distractor: h"] = "distractor,h"
        groupedData_all$category_combination[groupedData_all$category_combination == "distractor: n"] = "distractor,n"
      }
    } 
    else if(grepl("dComplex_focus",groupedData_all$category_combination[1],fixed=TRUE)){
      # cat("\ncase of dComplex_focus to change")
      # cat('\nstr_count(groupedData_all$category_combination[1],"_") > 0: ',(str_count(groupedData_all$category_combination[1],"_") > 0))
      if (str_count(groupedData_all$category_combination[1],"_") > 1){
        groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,E_M"] = "focus complexity: Easy-Medium"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,E_H"] = "focus complexity: Easy-Hard"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,M_H"] = "focus complexity: Medium-Hard"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus complexity: Easy-Hard","focus complexity: Medium-Hard","focus complexity: Easy-Medium"))
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy-Medium"] = "dComplex_focus,E_M"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy-Hard"] = "dComplex_focus,E_H"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Medium-Hard"] = "dComplex_focus,M_H"
      } else if (str_count(groupedData_all$category_combination[1],"_") > 0){
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,E"] = "focus complexity: Easy"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,M"] = "focus complexity: Medium"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,H"] = "focus complexity: Hard"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus complexity: Hard","focus complexity: Medium","focus complexity: Easy"))
        # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("1 - focus complexity: Easy","2 - focus complexity: Medium",   "3 - focus complexity: Hard"))
        
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy"] = "dComplex_focus,E"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Medium"] = "dComplex_focus,M"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Hard"] = "dComplex_focus,H"
      } else {
        groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,E"] = "focus complexity: Easy"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,M"] = "focus complexity: Medium"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "dComplex_focus,H"] = "focus complexity: Hard"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus complexity: Hard","focus complexity: Medium","focus complexity: Easy"))
        # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("1 - focus complexity: Easy","2 - focus complexity: Medium",   "3 - focus complexity: Hard"))
        
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Easy"] = "dComplex_focus,E"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Medium"] = "dComplex_focus,M"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus complexity: Hard"] = "dComplex_focus,H"
      }
      # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHAT_Qn"))
    } 
    else if(grepl("focus",groupedData_all$category_combination[1],fixed=TRUE)){
      cat("\n\t\t\t\t!!!!!!--case of focus to change. First one is: ",(groupedData_all$category_combination[1]),"-- groupedData_all$category_combination: ",groupedData_all$category_combination,"!!!!!!")
      if (str_count(groupedData_all$category_combination[1],"_") > 1){
        groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Qn_WHAT_Ql"] = "focus: WHAT_Ql-WHAT_Qn"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Qn_WHERE"] = "focus: WHAT_Qn-WHERE"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Ql_WHERE"] = "focus: WHAT_Ql-WHERE"
        # cat("\n*__* groupedData_all$category_combination: ",toString(groupedData_all$category_combination))
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHERE","focus: WHAT_Qn-WHERE","focus: WHAT_Ql-WHAT_Qn"))
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql-WHAT_Qn"] = "focus,WHAT_Ql_WHAT_Qn"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn-WHERE"] = "focus,WHAT_Qn_WHERE"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql-WHERE"] = "focus,WHAT_Ql_WHERE"
        
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Ql_WHAT_Qn"] = "focus: WHAT_Ql-WHAT_Qn"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Qn_WHERE"] = "focus: WHAT_Qn-WHERE"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Ql_WHERE"] = "focus: WHAT_Ql-WHERE"
        if (is.na(groupedData_all$orderCategoryCombination[1])){
          groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHERE","focus: WHAT_Qn-WHERE","focus: WHAT_Ql-WHAT_Qn"))
        }
        
        # groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql-WHAT_Qn"] = "focus,WHAT_Ql_WHAT_Qn"
        # groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn-WHERE"] = "focus,WHAT_Qn_WHERE"
        # groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql-WHERE"] = "focus,WHAT_Ql_WHERE"
        # 
        # # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHAT_Qn"))
        # groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Ql"] = "focus: WHAT_Ql"
        # groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Qn"] = "focus: WHAT_Qn"
        # groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHERE"] = "focus: WHERE"
        # if (is.na(groupedData_all$orderCategoryCombination[1])){
        #   groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHERE","focus: WHAT_Qn","focus: WHAT_Ql"))
        # }
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql"] = "focus,WHAT_Ql"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn"] = "focus,WHAT_Qn"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHERE"] = "focus,WHERE"
      }
      else {
        # I don't think this is ever going to occur
        groupedData_all$category_combination = as.character(groupedData_all$category_combination) # TODO consider update for other orders of focuses?
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Qn"] = "focus: WHAT_Qn"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHAT_Ql"] = "focus: WHAT_Ql"
        groupedData_all$category_combination[str_replace_all(groupedData_all$category_combination, " ","") == "focus,WHERE"] = "focus: WHERE"
        groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHERE","focus: WHAT_Qn","focus: WHAT_Ql"))
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Qn"] = "focus,WHAT_Qn"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHAT_Ql"] = "focus,WHAT_Ql"
        groupedData_all$category_combination[groupedData_all$category_combination == "focus: WHERE"] = "focus,WHERE"
        # groupedData_all$orderCategoryCombination <- factor(groupedData_all$category_combination,c("focus: WHAT_Ql-WHAT_Qn"))
      }
    } 
  }
  if("dMask" %in% colnames(groupedData_all)) {
    # cat("\nrename for dMask")
    groupedData_all$dMask = as.character(groupedData_all$dMask)
    groupedData_all$dMask[groupedData_all$dMask == "easy"] = "Mask Easy"
    groupedData_all$dMask[groupedData_all$dMask == "medium"] = "Mask Medium"
    groupedData_all$dMask[groupedData_all$dMask == "hard"] = "Mask Hard"
    groupedData_all$orderMaskComplex <- factor(groupedData_all$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
    groupedData_all$dMask[groupedData_all$dMask == "Mask Easy"] = "easy"
    groupedData_all$dMask[groupedData_all$dMask == "Mask Medium"] = "medium"
    groupedData_all$dMask[groupedData_all$dMask == "Mask Hard"] = "hard"
  }
  if ("dComplex_focus" %in% colnames(groupedData_all)) {
    # cat("\n&&&&&&&&&&&rename for dComplex_focus")
    groupedData_all$dComplex_focus = as.character(groupedData_all$dComplex_focus)
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "E"] = "Focus Easy"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "M"] = "Focus Medium"
    groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "H"] = "Focus Hard"
    groupedData_all$orderFocusComplex <- factor(groupedData_all$dComplex_focus,c("Focus Easy","Focus Medium","Focus Hard"))
    # groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Easy"] = "E"
    # groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Medium"] = "M"
    # groupedData_all$dComplex_focus[groupedData_all$dComplex_focus == "Focus Hard"] = "H"
  }
  if ("scaling" %in% colnames(groupedData_all)) {
    cat("\n~~~\tscaling in colnames")
    groupedData_all$orderedScaling <- NA
    groupedData_all$scaling = as.character(groupedData_all$scaling)
    groupedData_all$orderedScaling[groupedData_all$scaling == 0] <- "Scaling 0"
    groupedData_all$orderedScaling[groupedData_all$scaling == 1] <- "Scaling 1"
    groupedData_all$orderedScaling[groupedData_all$scaling == 2] <- "Scaling 2"
    groupedData_all$orderedScaling <- factor(groupedData_all$orderedScaling,c("Scaling 0","Scaling 1","Scaling 2"))
    groupedData_all$scaling[groupedData_all$scaling == "Scaling 0"] = 0
    groupedData_all$scaling[groupedData_all$scaling == "Scaling 1"] = 1
    groupedData_all$scaling[groupedData_all$scaling == "Scaling 2"] = 2
  }
  # if ("focus" %in% colnames(groupedData_all)){
  #   cat("\n~~~~\tfocus in colnames")
  #   groupedData_all$orderedFocus <- NA
  #   groupedData_all$focus = as.character(groupedData_all$focus)
  #   groupedData_all$orderedFocus[groupedData_all$focus == "WHAT_Ql"] <- "WHAT_Ql"
  #   groupedData_all$orderedFocus[groupedData_all$focus == "WHAT_Qn"] <- "WHAT_Qn"
  #   groupedData_all$orderedFocus[groupedData_all$focus == "WHERE"] <- "WHERE"
  #   groupedData_all$orderedFocus <- factor(groupedData_all$focus,c("WHAT_Ql","WHAT_Qn","WHERE"))
  #   groupedData_all$focus[groupedData_all$focus == "WHAT_Ql"] = "WHAT_Ql"
  #   groupedData_all$focus[groupedData_all$focus == "WHAT_Qn"] = "WHAT_Qn"
  #   groupedData_all$focus[groupedData_all$focus == "WHERE"] = "WHERE"
  # 
  # }
  
  cat("\nSO how are the questions now...: ",length(groupedData_all$question),", and what of the orderCategoryCombination[1]: ",groupedData_all$orderCategoryCombination[1], ", colnames(groupedData_all)",colnames(groupedData_all),", groupedData_all$orderCategoryCombination[5]: ",groupedData_all$orderCategoryCombination[5],"\n\n")
  return (groupedData_all);
}

# Inspired by the function genAndPlot_differences_factorBased 
genAndPlotCI_factorBased <- function(d, factorScaling=FALSE, factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorVariation="dMask") {
  # d <- filter_allTrust0or5_impossibleQualAnswer(d)
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
  if (factorScaling | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("diffA1","diffA2","diffA3");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  arrFactorVariations <- c()
  if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  cat("\n about to loop arrQuestions")
  # generations of boot according to the number of factors for each question
  for (i in arrQuestions){
    cat("\nloop questions. i: ",i)
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)                  
                } 
                else {
                  # numFactor == 3 # should be fine, a) but testing necessary b) adaptation in cases where there 
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  # TODO consider that there could potentially be only 2 selec, for a factor like distractor!
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("\n\nHEEEEY length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              # numFactor==2
              # warning: remember that factorVariation can be distractor
              cat("\nHey what's numFactor ? ",numFactor)
              dfTest_CI <- NULL
              if (length(arrFactorVariations)== 2){
                cat("\n\tlength(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI);
              } 
              else {
                cat("\n\tfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              }
              is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI));
              dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
              dfTest_CI <- rename(dfTest_CI,low_CI=X2);
              dfTest_CI <- rename(dfTest_CI,high_CI=X3);
              dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI$question <- i
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            if (length(arrFactorVariations)== 2){
              # cat("length(arrFactorVariations)== 2")
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              dfTest_CI <- data.frame(group1_CI,group2_CI);
            } 
            else {
              # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
              # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
              # cat("\nand added the strings. Might be a typo in all the cases of this code...")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
            }
            is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI));
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
            dfTest_CI <- rename(dfTest_CI,low_CI=X2);
            dfTest_CI <- rename(dfTest_CI,high_CI=X3);
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
            dfTest_CI[factor1] <- curFactor1; 
            dfTest_CI$question <- i
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
      
    }else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorVariation can be distractor
      dfTest_CI <- NULL
      if (length(arrFactorVariations)== 2){
        cat("length(arrFactorVariations)== 2")
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI);
      } 
      else {
        cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
      }
      is.numeric(dfTest_CI$mean_CI[2])
      dfTest_CI <- data.frame(t(dfTest_CI));
      dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
      dfTest_CI <- rename(dfTest_CI,low_CI=X2);
      dfTest_CI <- rename(dfTest_CI,high_CI=X3);
      dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
      dfTest_CI[factor1] <- curFactor1; 
      dfTest_CI$question <- i
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      cat("\ngenerated the data to display, factorVariation-",factorVariation)
    }
  }
  
  # we should have the dfCI_global loaded now, but still need to display it.
  cat("\n####about to draw, are the plots null: ",(is.null(groupedPlotCI_1)),", ",(is.null(groupedPlotCI_2)),", ",(is.null(groupedPlotCI_3)) )
  cat("\nwhat of the global structure... ",dim(dfCI_global),", and its questions: ",length(dfCI_global$question))
  class(dfCI_global$category_combination)
  class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
  class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
  class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global)
  cat("\nrenaming done...")
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  }
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  }
  else if(numFactor ==1){
    if (factor1=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  }
  
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)  
  
  cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else {
    # no wrapping.
  } 
  
  if (numFactor!=0){  
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Mean with Mask") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+ 
      labs(title = 'Mean with Mask', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
      )
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Overall Mean") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Overall Mean', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank()
      )
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Mask Proportion") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") + 
      labs(title = 'Mask Proportion', y = "" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
    grid.arrange(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3, ncol=3,top=textGrob(paste("Confidence intervals for ",factorVariation,", factored by ",toString(factorArr),sep="")))
  } 
  else {
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for mean with Mask")
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for overall mean") 
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for diffA3")
    grid.arrange(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3, ncol=3,top=textGrob(paste("Confidence intervals for ",factorVariation,sep="")))
  }
  
  return (dfCI_global);
}
# dfCI_test <- genAndPlotCI_factorBased(d_sclAll,factorScaling=TRUE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=TRUE, factorVariation="dMask")
# dfCI_test
# cat("",toString(dfCI_test[0,])) # cat("",colnames(dfCI_test))

genAndPlot_differences_factorBased <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorDifference="dMask", logFunction = FALSE ){
  cat("\ngenAndPlot_differences_factorBased")
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");
  if (factorScaling | factorDifference=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("diffA1","diffA2","diffA3");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\n")
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  # food for thought... should these include scaling and distractor?! Need to adapt since distractor has only 2 categories
  arrFactorDifferences <- c()
  if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}
  # cat("\narrFactorDifferences: ",arrFactorDifferences)
  # d <- renameGroupedData(d) # test... seems to work fine # Calling this we change the names of the attributes... That's problematic
  # cat("\nfactors and arrFactors set. numFactor",numFactor,", factor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)
  
  # alternative idea: put together the data into one big structure... Another more clever loop can be made for generation of graphs...
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  
  for (i in arrQuestions){
    cat("\nloop questions. i: ",i)
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[1],]
                  selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[2],]
                  selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factor4]==arrFactor4[3],]
                  group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction = logFunction);
                  group2_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2=selec3,question=curQuestion, logFunction = logFunction);
                  group3_CI<- bootQuestionsDifferences_directSubstract(d=selec2, d2=selec3,question=curQuestion, logFunction = logFunction);
                  group1_CI <- c(group1_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[1]," and ",arrFactor4[2]))
                  group2_CI <- c(group2_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[1]," and ",arrFactor4[3]))
                  group3_CI <- c(group3_CI, paste("diff scaling 4 factors, with factor4: ",arrFactor4[2]," and ",arrFactor4[3]))
                  dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI))
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);dfTest_CI <- rename(dfTest_CI,low_CI=X2);dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"Diff_Type"=X4);
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  if (i==1){
                    groupedPlotCI_1 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  } 
                  else if (i==2){
                    groupedPlotCI_2 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  } 
                  else {
                    groupedPlotCI_3 <- ggplot(dfTest_CI, aes(x=mean_CI,y=factor4)) +
                      geom_vline(xintercept = 0) +
                      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
                      geom_point(size=3,col="black",fill="white", shape=1) +
                      xlim(c(-absGraphEdge,absGraphEdge)) +
                      facet_wrap( as.formula(paste("~",factor1,"+",factor2+","+factor3)) , dir="v", ncol=1) +
                      ggtitle(strSentence)
                  }
                } 
                else {
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                  selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                  selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                  group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
                  group2_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2=selec3,question=curQuestion, logFunction = logFunction);
                  group3_CI<- bootQuestionsDifferences_directSubstract(d=selec2, d2=selec3,question=curQuestion, logFunction = logFunction);
                  group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                  group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                  group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                  dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              # numFactor==2
              cat("\n numFactor==2 \t what do we have here.")
              # warning: remember that factorDifference can be distractor
              dfTest_CI <- NULL
              if (length(arrFactorDifferences)== 2){
                cat("length(arrFactorDifferences)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
                group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                dfTest_CI <- data.frame(group1_CI);
              } 
              else {
                cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
                group2_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2=selec3,question=curQuestion, logFunction=logFunction);
                group3_CI<- bootQuestionsDifferences_directSubstract(d=selec2, d2=selec3,question=curQuestion, logFunction=logFunction);
                group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              }
              cat("\nShould have set the group1_CI and other stuff")
              is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI));
              dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
              dfTest_CI <- rename(dfTest_CI,low_CI=X2);
              dfTest_CI <- rename(dfTest_CI,high_CI=X3);
              dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI$question <- i
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              cat("\ngenerated the data to display, factorDifference-",factorDifference,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorDifference can be distractor
            dfTest_CI <- NULL
            # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
            if (length(arrFactorDifferences)== 2){
              selec1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
              group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              dfTest_CI <- data.frame(group1_CI);
            } 
            else {
              selec1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
              group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
              group2_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2=selec3,question=curQuestion, logFunction=logFunction);
              group3_CI<- bootQuestionsDifferences_directSubstract(d=selec2, d2=selec3,question=curQuestion, logFunction=logFunction);
              group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
              group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
            }
            is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI));
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
            dfTest_CI <- rename(dfTest_CI,low_CI=X2);
            dfTest_CI <- rename(dfTest_CI,high_CI=X3);
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
            dfTest_CI[factor1] <- curFactor1; 
            dfTest_CI$question <- i
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Differences of confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            cat("\ngenerated the data to display, factorDifference-",factorDifference,", factor1-",factor1,": ",curFactor1)
          }
          }
        }
      }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorDifference can be distractor
      dfTest_CI <- NULL
      cat("\nfactorDifference: ",factorDifference,", arrFactorDifferences: ",toString(arrFactorDifferences))
      if (length(arrFactorDifferences)== 2){
        selec1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
        group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        dfTest_CI <- data.frame(group1_CI);
      } 
      else {
        selec1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        selec3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
        group1_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2= selec2,question=curQuestion, logFunction=logFunction);
        group2_CI<- bootQuestionsDifferences_directSubstract(d=selec1, d2=selec3,question=curQuestion, logFunction=logFunction);
        group3_CI<- bootQuestionsDifferences_directSubstract(d=selec2, d2=selec3,question=curQuestion, logFunction=logFunction);
        group1_CI <- c(group1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        group2_CI <- c(group2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
        group3_CI <- c(group3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
      }
      is.numeric(dfTest_CI$mean_CI[2])
      dfTest_CI <- data.frame(t(dfTest_CI));
      dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
      dfTest_CI <- rename(dfTest_CI,low_CI=X2);
      dfTest_CI <- rename(dfTest_CI,high_CI=X3);
      dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
      dfTest_CI$question <- i
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Differences of confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      # cat("\ngenerated the data to display, factorDifference-",factorDifference)
    }
  }
  cat("\n####about to draw, how are the plots: ",(is.null(groupedPlotCI_1)),", ",(is.null(groupedPlotCI_2)),", ",(is.null(groupedPlotCI_3)) )
  cat("\nwhat of the global structure... ",dim(dfCI_global))
  
  class(dfCI_global$category_combination)
  class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
  class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
  class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global)
  cat("\nrenaming done... ")
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";}    
  } 
  
  cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else {
    # no wrapping.
  } 
  
  # A call should be made to ensure the data generated has the info whether difference is significativee
  dfCI_global<-addInfoCiDifferenceSignificant(dfCI_global)
  
  passedBoolTest_1 <- TRUE
  passedBoolTest_2 <- FALSE
  
  if (numFactor!=0){  
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mean with Mask") +
      labs(title = 'Differences for Mean with Mask', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position="none"
      ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) 
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Overall Means")  + 
      labs(title = 'Differences for Overall Means', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      )+
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mask Proportion") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") + 
      labs(title = 'Differences for Mask Proportion', y = "" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    
    grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3), ncol=3,top=textGrob(paste("Confidence intervals for ",factorDifference,", factored by ",toString(factorArr),sep="")))
  } 
  else {
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="diffA1",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mean with Mask") +
      labs(title = 'Differences for Mean with Mask', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position="none"
      )
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question=="diffA2",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Overall Means")  + 
      labs(title = 'Differences for Overall Means', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      )
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question=="diffA3",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point(aes(x=-edgeSize, alpha = 1*significantDifference,col="red",size=0.5))+
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mask Proportion") +
      labs(title = 'Differences for Mask Proportion', y = "" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    
    grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_2,groupedPlotCI_3), ncol=3,top=textGrob(paste("Confidence intervals for ",factorDifference,sep="")))
  }
  return (dfCI_global)
}
# ## test calls
# dfCI_test_differences <- genAndPlot_differences_factorBased(d_sclAll,factorScaling=TRUE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=TRUE, factorDifference="dMask")
# dfCI_test_differences
# dfCI_test_differences2 <- genAndPlot_differences_factorBased(d_sclAll,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="scaling")
# dfCI_test_differences2

combine_genPlot_CIandDifferences  <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, 
                                               factorTrust = FALSE, factorDifference="dMask", arrMixOrderFormula=c(), logFunction=FALSE,useLogDiff = FALSE){

  # d[sort(d$focus)] # https://stackoverflow.com/questions/51501989/how-to-sort-data-by-column-in-descending-order-in-r
  d <- d[order(d$focus), ]
  if (!is.null(d$distractor)){d <- d[order(d$distractor), ]}  # necessary? useful?
  if (!is.null(d$scaling)){d <- d[order(d$scaling), ]}  # necessary? useful?
  
  factorVariation <- factorDifference
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Ql","WHAT_Qn","WHERE"); arrMask <- c("easy","medium","hard"); 
  arrDComplex_focus <- c("E","M","H");  
  arrTrust  <- c("trustA1","trustA2","trustA3","trustB")
  if (factorScaling | factorVariation=="scaling"){arrFocus <- c("WHAT_Ql","WHAT_Qn")}
  
  # IMPORTANT NOTE ABOUT logFunction. If we use the log_diffA1 instead of diffA1, then isn't the usage again of the formula transforming it again?! To think about
  arrQuestions <- c();
  if (!useLogDiff){ arrQuestions <- c("diffA1","diffA2","diffA3"); } else { arrQuestions <- c("log_diffA1","log_diffA2","log_diffA3"); }
  
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,factorTrust=factorTrust);
  # cat("\nfactorArr: ",factorArr);
  numFactor <- length(factorArr)
  if (length(arrMixOrderFormula) == 0){
    factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]  
  }
  else {
    factor1 <- factorArr[arrMixOrderFormula[1]]; factor2 <- factorArr[arrMixOrderFormula[2]]; factor3 <- factorArr[arrMixOrderFormula[3]]; factor4 <- factorArr[arrMixOrderFormula[4]];
  }
  
  numFactor <- length(factorArr);
  cat("\n}}}}factorArr: ",toString(factorArr)); cat("\nnumFactor: ",numFactor)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else if (factor1=="trust"){
      arrFactor1 <- arrTrust
    }
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
      else if (factor2=="trust"){
        arrFactor2 <- arrTrust
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
      else if (factor3 == "trust"){
        arrFactor3 <- arrTrust
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  arrFactorVariations <- c(); if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  arrFactorDifferences <- c(); if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}  
  
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  dfCI_global_differences <- data.frame(); dfCI_global_differences$mean_CI[0] <- 0; dfCI_global_differences$low_CI[0] <- 0;dfCI_global_differences$high_CI[0] <- 0;dfCI_global_differences$category_combination[0] <- 0; dfCI_global_differences$question[0] <- 0; dfCI_global$orderAdded[0] <- 0# doubt about this approach...
  
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}; if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}; if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor1=="focus"){dfCI_global_differences$focus[0] <- 0}; if(factor1=="scaling"){dfCI_global_differences$scaling[0] <- 0}; if(factor1=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}; if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}; if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor2=="focus"){dfCI_global_differences$focus[0] <- 0}; if(factor2=="scaling"){dfCI_global_differences$scaling[0] <- 0}; if(factor2=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}; if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}; if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor3=="focus"){dfCI_global_differences$focus[0] <- 0}; if(factor3=="scaling"){dfCI_global_differences$scaling[0] <- 0}; if(factor3=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}; if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}; if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor4=="focus"){dfCI_global_differences$focus[0] <- 0}; if(factor4=="scaling"){dfCI_global_differences$scaling[0] <- 0}; if(factor4=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  
  cat("\nAbout to loop arrQuestions")
  for (i in arrQuestions){
    cat("\nLoop questions. i: ",i)
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                  dfTest_CI <- NULL;
                  dfTest_CI_differences <- NULL;
                  if (length(arrFactorVariations)== 2){
                    # cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]                    
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3],]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="") 
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); 
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
                  dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)                  
                } 
                else {
                  # numFactor == 3 
                  dfTest_CI <- NULL
                  # DISTRACTOR CASE FIRST
                  if (length(arrFactorVariations)== 2){ 
                    cat("\t\n\n\n\t\t DISTRACTOR CASE. length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="");
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="");
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="");
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric);
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1);
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) );
                  strSentence <- paste("Confidence intervals, ",curQuestion);
                  dfCI_global <- rbind(dfCI_global, dfTest_CI);
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric);
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1);
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) );
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion);
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences);
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              # numFactor==2
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              dfTest_CI_differences <- NULL
              if (length(arrFactorVariations)== 2){
                # cat("\n HEY 2 length(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                
                dfTest_CI <- data.frame(group1_CI,group2_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI);
              } 
              else {
                # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
                group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
                
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
              }
              # is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
              
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI_differences[factor1] <- curFactor1; 
              dfTest_CI_differences[factor2] <- curFactor2;
              
              dfTest_CI$question <- i
              dfTest_CI_differences$question <- i
              
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
              
              # cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1"); # is this where we need to investigate X4?! # solved
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            dfTest_CI_differences <- NULL
            if (length(arrFactorVariations)== 2){
              cat("\n\t HEY 3 length(arrFactorVariations)== 2")
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
              
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")

              dfTest_CI <- data.frame(group1_CI,group2_CI);
              dfTest_CI_differences <- data.frame(group_differences1_CI); 
            } 
            else {
              # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              selec_differences3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
              # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
              group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
              group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
              # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
              group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
              
              # cat("\nand added the strings. Might be a typo in all the cases of this code...")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
            }

            # cat("\ncolnames(dfTest_CI): ",colnames(dfTest_CI))
            dfTest_CI <- data.frame(t(dfTest_CI)); 
            # cat("\ncolnames(dfTest_CI) right after turning data frame: ",colnames(dfTest_CI))
            # cat("\ndfTest_CI$X4  right after turning data frame: ", toString(dfTest_CI$X4))
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); 
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4); dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
            # cat("\ncolnames(dfTest_CI): ",colnames(dfTest_CI))
            # cat("\nRenamed dfTest_CI")
            # cat("\ncolnames(dfTest_CI_differences): ", colnames(dfTest_CI_differences)," ---- \n")
            dfTest_CI_differences <- data.frame(t(dfTest_CI_differences));
            # cat("\ncolnames(dfTest_CI_differences) right after turning data frame: ", colnames(dfTest_CI_differences),
            #     "\n-- dfTest_CI_differences$X1: ",toString(dfTest_CI_differences$X1),
            #     "\n-- dfTest_CI_differences$X2: ",toString(dfTest_CI_differences$X2),
            #     "\n-- dfTest_CI_differences$X3: ",toString(dfTest_CI_differences$X3),
            #     "\n-- dfTest_CI_differences$X4: ",toString(dfTest_CI_differences$X4),", dfTest_CI_differences$X5: ",toString(dfTest_CI_differences$X5)," ---- \n")
            dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
            dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences$question <- i
            cat("\nRenamed dfTest_CI_differences")
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Differences of confidence intervals, ",curQuestion)
            dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
            
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
      
    }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorVariation can be distractor
      dfTest_CI <- NULL
      dfTest_CI_differences <- NULL
      
      if (length(arrFactorVariations)== 2){
        cat("\nHEY 4 length(arrFactorVariations)== 2, factorVariation: ",factorVariation,", factorDifference: ",factorDifference,
            ", arrFactorVariations: ",arrFactorVariations,", arrFactorDifferences: ",arrFactorDifferences)
        cat("\nlength selec1: ",length(d[factorDifference]==arrFactorDifferences[1])," length selec2: ",length(d[factorDifference]==arrFactorDifferences[2]))
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        cat("\n__toString(selec1): ",toString(selec1),", toString(selec2): ",toString(selec2),", selec_differences1 and selec_differences2 made. Also, curQuestion is: ",curQuestion);
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        cat("\n__group1_CI and group2_CI");
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
        cat("\n__bootQuestionsDifferences_conservative passed");
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI);
      } 
      else {
        cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",arrFactorVariations,
            "\ntoString(arrFactorVariations): ",toString(arrFactorVariations),", curQuestion: ",curQuestion)
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        selec_differences3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
        
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
        group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion);
        group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion);
        
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
        group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
      }
      dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); 
      dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);dfTest_CI$question <- i;
      # cat("\n****length(dfTest_CI$question): ",length(dfTest_CI$question))
      # cat("\n****length(dfTest_CI_differences$question): ",length(dfTest_CI_differences$question))
      # dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
      # cat("\ndfTest_CI$category_combination: ",dfTest_CI$category_combination)
      dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); dfTest_CI_differences$question <- i;
      
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric);
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1);
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) );
      strSentence <- paste("Confidence intervals, ",curQuestion);
      dfCI_global <- rbind(dfCI_global, dfTest_CI);
      dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric);
      leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1);
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) );
      strSentence <- paste("Differences of confidence intervals, ",curQuestion);
      dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences);
      
      cat("\ngenerated the data to display, factorVariation-",factorVariation);
    }
  }
  
  # we should have the dfCI_global loaded now, but still need to display it.
  cat("\n####about to draw")
  cat("\nwhat of the global structure variations... ",dim(dfCI_global),", and their questions: ",length(dfCI_global$question),
  "\nwhat of the global structure differences... ",dim(dfCI_global_differences),", and their questions: ",length(dfCI_global_differences$question))
  # cat("\n\t\t****dfCI_global$category_combination: ",dfCI_global$category_combination,"****\t\t\n")
  class(dfCI_global$category_combination); class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI); class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI); class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI); class(dfCI_global_differences$category_combination); class(dfCI_global_differences$mean_CI); dfCI_global_differences$mean_CI <- as.numeric(dfCI_global_differences$mean_CI); class(dfCI_global_differences$mean_CI); class(dfCI_global_differences$low_CI); dfCI_global_differences$low_CI <- as.numeric(dfCI_global_differences$low_CI); class(dfCI_global_differences$low_CI); class(dfCI_global_differences$high_CI); dfCI_global_differences$high_CI <- as.numeric(dfCI_global_differences$high_CI); class(dfCI_global_differences$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global)
  dfCI_global_differences <- renameGroupedData(dfCI_global_differences)

  cat("\n\t---01---\tunique(dfCI_global$category_combination): ",unique(dfCI_global$category_combination)) # seems fine still. There are several of them, but I guess that's fine.
  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  minLow_cI_differences <- max(abs(dfCI_global_differences$low_CI));maxHigh_CI_differences <- max(abs(dfCI_global_differences$high_CI)); edgeSize_differences <- max(0.1+abs(minLow_cI_differences),0.1+abs(maxHigh_CI_differences)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize,",minLow_cI_differences: ",minLow_cI_differences,", maxHigh_CI_differences: ",maxHigh_CI_differences,",edgeSize_differences: ",edgeSize_differences)
  # cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize_CI <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI));
  minLow_cI_differences <- max(abs(dfCI_global_differences$low_CI));maxHigh_CI_differences <- max(abs(dfCI_global_differences$high_CI)); edgeSize_differences <- max(0.1+abs(minLow_cI_differences),0.1+abs(maxHigh_CI_differences));
  min_log_diffA1 <- min(d[["log_diffA1"]]);max_log_diffA1 <- max(d[["log_diffA1"]]);min_log_diffA2 <- min(d[["log_diffA2"]]);max_log_diffA2 <- max(d[["log_diffA2"]]);min_log_diffA3 <- min(d[["log_diffA3"]]);max_log_diffA3 <- max(d[["log_diffA3"]]);
  
  # edge according to the CI
  edgeSize <- max(edgeSize_CI,edgeSize_differences,max_log_diffA1,max_log_diffA2,max_log_diffA3);
  # edge according to the values
  # maxabsEdge <- max( select( d, arrQuestions[1])
  # maxabsEdgeValue <- max(max(select(d_measurement_all_noTikTok,arrQuestions[1])) , max(select(d_measurement_all_noTikTok,arrQuestions[2])) , max(select(d_measurement_all_noTikTok,arrQuestions[3])) )
  # edgeSize <- max(edgeSize, maxabsEdgeValue)
  
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize,
      "\n^^^^what's the length of question for dfCI_global now? ",length(dfCI_global$question), "\ndfCI_global$orderAdded: ", dfCI_global$orderAdded)
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling"); strFormula <- str_replace(strFormula,"dMask","orderMaskComplex"); strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling"); strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex"); strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  }
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
    
  }
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  }
  
  cat("\n\t---02---\tunique(dfCI_global$category_combination): ",unique(dfCI_global$category_combination))
  
  groupedPlotCI_1 <- NULL; groupedPlotCI_2 <- NULL; groupedPlotCI_3<- NULL;
  group_differencesedPlotCI_1<-NULL;group_differencesedPlotCI_2<-NULL;group_differencesedPlotCI_3<-NULL;
  cat("\n:::: dfCI_global colNames: ",colnames(dfCI_global)); cat("\n:::: dfCI_global_differences colNames: ",colnames(dfCI_global_differences))
  dfCI_global_differences <- addInfoCiDifferenceSignificant(dfCI_global_differences)

  # histogram... # http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
  # geom_histogram()
  # or violin? # geom_violin(aes(x=cut, y=price))
  
  cat("\n\n㎣㎣arrQuestions: ",arrQuestions,
      "\n\nunique(dfCI_global$orderCategoryCombination): ",unique(dfCI_global$orderCategoryCombination),
      "\n\ttoString(unique(dfCI_global$orderCategoryCombination)): ",toString(unique(dfCI_global$orderCategoryCombination)))
  
  d <- prettyEnrichOrderCategory(d,dfCI_global) # TODO CRITICAL MISSING SOME COLUMNS

  # d <- renameGroupedData((dfCI_global))
  cat("\n\n\t....Added column for unique(d$orderCategoryCombination): ",unique(d$orderCategoryCombination),
      "\nbtw strFormula: ",strFormula,
      "\ntoString(unique(d$orderCategoryCombination)): ",toString(unique(d$orderCategoryCombination)))
  cat("\n\n\ndfCI_global$orderCategoryCombination: ", dfCI_global$orderCategoryCombination,
      "\nunique(d$orderCategoryCombination): ",unique(d$orderCategoryCombination))
  
  if (grepl("dComplex_focus",strFormula,fixed=TRUE)){
    strFormula <- str_replace(strFormula, "dComplex_focus", "orderFocusComplex")
  }
  
  d <- orderData(d);
  # dfCI_global <- orderData(dfCI_global);
  
    
  strTitleTotal <- NULL;
  if (numFactor!=0){
    cat("\n\t\t\t))))numFactor!=0: ",(numFactor!=0))
    strTitleTotal <- paste("Confidence intervals and differences for ",factorDifference,", factored by ",toString(factorArr),sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[1] ,], aes(x=mean_CI,y=orderCategoryCombination, show.legend = FALSE )) +
      geom_vline(xintercept = -3) +
      geom_violin( data = d,  aes (x= log_diffA1 , y = orderCategoryCombination, alpha = 0.3), adjust = .75, show.legend = FALSE ) + # change alpha?
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-3,edgeSize)) +
      geom_point(data = d,  aes (x= log_diffA1 , y = orderCategoryCombination, alpha = 0.3) , show.legend = FALSE, size=1,col="red",fill="red", shape=1) +
      geom_jitter(data = d,  aes (x= log_diffA1, alpha = 0.1), size=0.5,col="blue",fill="blue", show.legend = FALSE) +
      ggtitle("Mean with Mask") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+ 
      labs(title = 'Mean with Mask', y = "" ) +
      # geom_violin( data = d,  aes (x= log_diffA1 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE ) + # change alpha?
      # geom_point(data = d,  aes (x= log_diffA1 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +      
      # annotate(geom="text", x=3, y=5, label="TEST LABEL", color="red")+
      # theme(
      #   strip.background = element_blank(), 
      #   strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      # ) + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[2],], aes(x=mean_CI,y=orderCategoryCombination, show.legend = FALSE )) +
      geom_vline(xintercept = -3) +
      geom_violin( data = d,  aes (x= log_diffA2 , y = orderCategoryCombination, alpha = 0.3), adjust = .75, show.legend = FALSE ) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      # geom_point(data = d[dfCI_global$question== arrQuestions[2] ,],  aes (x= log_diffA2 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +
      geom_point(data = d,  aes (x= log_diffA2 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +
      geom_jitter(data = d,  aes (x= log_diffA2, alpha = 0.1), width = 0.5, size=0.5,col="blue",fill="blue", show.legend = FALSE) +
      xlim(c(-3,edgeSize)) +
      ggtitle("Overall Mean") +
      # annotate(geom="text", x=3, y=5, label="TEST LABEL 2", color="blue")+
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Overall Mean', y = "" ) +
      # theme(
      #   strip.background = element_blank(), 
      #   strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      # ) + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[3],], aes(x=mean_CI,y=orderCategoryCombination, show.legend = FALSE )) +
      geom_vline(xintercept = -3) +
      # annotate(geom="text", x=3, y=1, label="TEST LABEL 3", color="orange")+
      # geom_violin( data = d[dfCI_global$question== arrQuestions[3] ,],  aes (x= log_diffA3 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE ) +
      geom_violin( data = d,  aes (x= log_diffA3 , y = orderCategoryCombination, alpha = 0.3), adjust = .75, show.legend = FALSE ) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      # geom_point(data = d[dfCI_global$question== arrQuestions[3] ,],  aes (x= log_diffA3 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +      
      geom_point(data = d,  aes (x= log_diffA3 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE, size=1,col="red",fill="red", shape=1) +      
      geom_jitter(data = d,  aes (x= log_diffA3, alpha = 0.1),width = 1, size=0.5,col="blue",fill="blue", show.legend = FALSE) +
      xlim(c(-3,edgeSize)) +
      ggtitle("Mask Proportion") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Mask Proportion', y = "" ) +
      # theme(
      #   strip.background = element_blank(), 
      #   strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      # ) + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    # 
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[1],], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5, show.legend = FALSE)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mean with Mask") +
      labs(title = 'Differences for Mean with Mask', y = "" ) +
      theme(
        strip.background = element_blank(), 
        # strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    
    groupedPlotCI_differences2 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[2] ,], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5, show.legend = FALSE)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Overall Means")  + 
      labs(title = 'Differences for Overall Means', y = "" ) +
      theme(
        strip.background = element_blank(), 
        # strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      )+
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    
    groupedPlotCI_differences3 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[3],], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5, show.legend = FALSE)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mask Proportion") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") + 
      labs(title = 'Differences for Mask Proportion', y = "" ) +
      # theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none") + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
  } 
  else {
    cat("\n))))numFactor==0. dim(dfCI_global):  ",dim(dfCI_global) )
    strTitleTotal <- paste("Confidence intervals and differences for ",factorDifference,sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[1],], aes(x=mean_CI,y= orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_violin( data = d[dfCI_global$question== arrQuestions[1] ,],  aes (x= log_diffA1 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE ) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-3,edgeSize)) +
      ggtitle("Mean with Mask")+
      theme(
        strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      ) +
      labs(title = "Mean with Mask",y="") + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    groupedPlotCI_2 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[2],], aes(x=mean_CI,y= orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_violin( data = d[dfCI_global$question== arrQuestions[2] ,],  aes (x= log_diffA2 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE ) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-3,edgeSize)) +
      ggtitle("Mean Overall") +
      theme(
        strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      ) +
      labs(title = "Mean Overall",y="") + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    groupedPlotCI_3 <- ggplot(dfCI_global[dfCI_global$question== arrQuestions[3],], aes(x=mean_CI,y= orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_violin( data = d[dfCI_global$question== arrQuestions[3] ,],  aes (x= log_diffA3 , y = orderCategoryCombination, alpha = 0.3), show.legend = FALSE ) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(-3,edgeSize)) +
      ggtitle("Mask Proportion")+
      theme(
        strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()
      ) +
      labs(title = "Mask Proportion",y="") + 
      guides(fill = FALSE) + 
      guides(col = FALSE)
    # 

    cat("\nAbout to generate the graphs for dfCI_global_differences")
    arrSizesDifferent <- c(0.1,5); arrSizesSamePositive <- c(5); arrSizesSameNegative <- c(0.1)
    arrSize1 <- c(); arrSize2 <- c(); arrSize3 <- c();
    cat("\nlength(dfCI_global_differences$significantDifference): ",length(dfCI_global_differences$significantDifference))
    boolArrSize1 <- dfCI_global_differences$significantDifference[1] & dfCI_global_differences$significantDifference[2] & dfCI_global_differences$significantDifference[3]
    boolArrSize2 <- boolArrSize1; boolArrSize3 <- boolArrSize1;
    if ( length(dfCI_global_differences$significantDifference) > 3 ){
      boolArrSize2 <- dfCI_global_differences$significantDifference[4] & dfCI_global_differences$significantDifference[5] & dfCI_global_differences$significantDifference[6]
      boolArrSize3 <- dfCI_global_differences$significantDifference[7] & dfCI_global_differences$significantDifference[8] & dfCI_global_differences$significantDifference[9]
    }
    if (boolArrSize1){arrSize1 <- arrSizesSamePositive} else {arrSize1 <- arrSizesDifferent}
    if (boolArrSize2){arrSize2 <- arrSizesSamePositive} else {arrSize2 <- arrSizesDifferent}
    if (boolArrSize3){arrSize3 <- arrSizesSamePositive} else {arrSize3 <- arrSizesDifferent}
    
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[1],], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      xlim(c(-edgeSize,edgeSize)) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference, size=significantDifference ), alpha = 0.5)+
      scale_size_manual(values= arrSize1 ) +
      ggtitle("Differences for Mean with Mask") +
      labs(title = 'Differences for Mean with Mask', y = "" ) +
      theme(
        strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(),
        legend.position="none"
      )
    groupedPlotCI_differences2 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[2],], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference, size= significantDifference ), alpha = 0.5)+
      scale_size_manual(values=  arrSize2 ) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Overall Means")  + 
      labs(title = 'Differences for Overall Means', y = "" ) +
      theme(
        strip.background = element_blank(), 
        strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(),
        legend.position="none"
      )
    groupedPlotCI_differences3 <- ggplot(dfCI_global_differences[dfCI_global_differences$question== arrQuestions[3],], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference, size= significantDifference ), alpha = 0.5) +
      scale_size_manual(values= arrSize3 ) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Mask Proportion") +
      labs(title = 'Differences for Mask Proportion', y = "" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
  }
  
  cat("\n\n\t\t WTH!!! toString( unique(dfCI_global$orderCategoryCombination) ): ", toString (unique(dfCI_global$orderCategoryCombination) )," dfCI_global$orderCategoryCombination[1]: ",dfCI_global$orderCategoryCombination[1])
  
  cat("\nThe plots are generated. But are they fine?\n")
  grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_differences1,
                          groupedPlotCI_2,groupedPlotCI_differences2,
                          groupedPlotCI_3,groupedPlotCI_differences3), ncol=6,top=textGrob( strTitleTotal ) )
  # cat("not sure what to return")
  # return (dfCI_global_differences)
  return (dfCI_global)
}


# dfCombinationCI_differences_test__CIandDiff_dMask_factoredby_scaling_focus <- combine_genPlot_CIandDifferences(d_sclAll,factorScaling=TRUE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=TRUE, factorDifference="dMask");
# dfCombinationCI_differences_test__CIandDiff_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_CIandDifferences(d_alt,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=TRUE, factorDifference="dMask")

# dfCombinationCI_differences_test__CIandDiff_dComplex_focus_factoredby_focus <- combine_genPlot_CIandDifferences(d_measurement_all,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="dComplex_focus")
# dfCombinationCI_differences_test__CIandDiff_dComplex_focus_factoredby_focus

# dfCombinationCI_differences_test__CIandDiff_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_CIandDifferences(d_measurement_all,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=TRUE,factorDComplex_focus=TRUE, factorDifference="dMask")
# dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask <- combine_genPlot_CIandDifferences(d_measurement_all,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="dComplex_focus")
# dfCombinationCI_differences_test__CIandDiff_dFocusComplexity_factoredby_focus_dMask <- combine_genPlot_CIandDifferences(d_measurement_all,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= TRUE,factorFocus=TRUE,factorDComplex_focus=FALSE, factorDifference="dComplex_focus", arrMixOrderFormula= c(2,1) )
# dfCI_valuestoCSV <- dfCombinationCI_differences_test__CIandDiff_dMask_factoredby_focus_dComplex_focus
# dfCI_valuestoCSV
# dfCI_differencestoCSV
# dfCombinationCI_differences_test__CIandDiff_scaling_factoredby_none <- combine_genPlot_CIandDifferences(d_sclAll,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="scaling")
# dfCombinationCI_differences_test__CIandDiff_scaling_factoredby_none
# dfCombinationCI_differences_test__CIandDiff_distractor_factoredby_none <- combine_genPlot_CIandDifferences(d_distr_all,factorScaling=FALSE,factorDistractor=FALSE,factorDMask= FALSE,factorFocus=FALSE,factorDComplex_focus=FALSE, factorDifference="distractor")


# potential alternative approach... one of each for each trust, and factor then...
# ggplot(d_measurement_all_noTikTok)+
#   geom_vline(xintercept = 0) +
#   geom_histogram( data=d_measurement_all_noTikTok , aes( x=abs(diffA1) ) )

# potential approach with histograms... can't put y. 
# ggplot(d_measurement_all_noTikTok)+
#   geom_vline(xintercept = 0) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==0,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  ) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==1,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  ) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==2,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  ) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==3,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  ) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==4,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  ) +
#   geom_histogram( data=d_measurement_all_noTikTok[d_measurement_all_noTikTok$trustA1==5,] , aes( x=abs(diffA1), fill=trustA1, alpha=0.3 )  )

gen_res_trust_violin <- function (d, factorScaling=FALSE, factorDistractor=FALSE, factorDMask= FALSE, factorFocus=FALSE, 
                                  factorDComplex_focus=FALSE, factorTrust=FALSE, useLogDiff=TRUE, filterNeither = FALSE) {

  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,factorTrust=factorTrust);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\ngen_res_trust }}}} factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  cat("\n\tfactor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)
  
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
  if (factorScaling ){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
    
  arrQuestions <- c();
  if (!useLogDiff){ arrQuestions <- c("diffA1","diffA2","diffA3"); } else { arrQuestions <- c("log_diffA1","log_diffA2","log_diffA3"); }
  arrTrusts <- c("trustA1","trustA2","trustA3","trustB")
  
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  }
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  }
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  }
  cat("\n\tstrFormula: ",strFormula)
  
  # we should consider how to make a confidence interval if possible. Comparisons of groups would be a pain as it would be groups 0-1,0-2,0-3,0-4,0-5,1-2,1-3, etc.
  # we need to make it again, can't reuse the same code... take cases and adapt for each of the trust... and factor
  dfCI_global <- data.frame();
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  
  d <- renameGroupedData(d)
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  
  if (numFactor > 0){
    # something along those lines could work!
    df_mean_diffA1_0 <- d[d$trustA1==0,] %>% 
      group_by( .data[[factor1]], trustA1  ) %>%
      summarise(mean = mean( abs( .data[["diffA1"]] ) , na.rm = TRUE))
    df_mean_diffA1_1 <- d[d$trustA1==1,] %>% 
      group_by( .data[[factor1]], trustA1  ) %>%
      summarise(mean = mean( abs( .data[["diffA1"]] ) , na.rm = TRUE))

    df_mean_diffA1 <- d %>% 
      group_by(.data[[factor1]],  trustA1  ) %>%
      summarise(mean = mean( abs( .data[["diffA1"]] ) ), n=n() )

    # and this for two
    # df_mean_diffA1 <- measurement_rigorous_withDiffB %>% 
    #   group_by( .data[["dMask"]], .data[["focus"]]  ) %>%
    #   summarise(total = mean(abs( .data[["diffA1"]] ), na.rm = TRUE))
    
      # geom_point(data= df_mean_diffA1, aes(x = mean, y = trustA1, group=dMask, colour= trustA1  )  ) + # , group= .data[[factor1]] # IT REPEATS ITSELF OVER FACTOR?! I don't understand why. Might have to drop it unfortunately. Alternative could be jitter points but that's annoying to read...!

    # #feedde #fdd0a2 #fdae6b #fd8d3c #e6550d #a63603
    plot_trustA1 <- ggplot(d)+
      geom_vline(xintercept = 0) +
      geom_violin( data=d[d$trustA1==0,] , aes( x=abs(diffA1), y=trustA1,alpha=0.3 )) #, draw_quantiles = c(0.25, 0.5, 0.75)  ) 
    # if (length (d$cntrQ[d$trustA1==0] <= 2)){ plot_trustA1 <- plot_trustA1 + geom_jitter( data=d[d$trustA1==0,] , aes( x=abs(diffA1), y=trustA1, alpha=0.3, width = .02 ) )}
    plot_trustA1 <- plot_trustA1 + geom_violin( data=d[d$trustA1==1,] , aes( x=abs(diffA1), y=trustA1, alpha=0.3 )) #, draw_quantiles = c(0.25, 0.5, 0.75)  )
    # if (length (d$cntrQ[d$trustA1==1] <= 2)){ plot_trustA1 <- plot_trustA1 + geom_jitter( data=d[d$trustA1==1,] , aes( x=abs(diffA1), y=trustA1, alpha=0.3, width = .02 ) )}
      # geom_jitter( data=d[d$trustA1==1,] , aes( x=abs(diffA1), y=trustA1, col=trustA1,alpha=0.3, width = .02 ) ) +
    plot_trustA1 <- plot_trustA1 +  geom_violin( data=d[d$trustA1==2,] , aes( x=abs(diffA1), y=trustA1, alpha=0.3 )) #, draw_quantiles = c(0.25, 0.5, 0.75)  ) 
      # geom_jitter( data=d[d$trustA1==2,] , aes( x=abs(diffA1), y=trustA1,col=trustA1, alpha=0.3, width = .02 ) ) +
    plot_trustA1 <- plot_trustA1 + geom_violin( data=d[d$trustA1==3,] , aes( x=abs(diffA1), y=trustA1,alpha=0.3 )) #, draw_quantiles = c(0.25, 0.5, 0.75)  ) 
      # geom_jitter( data=d[d$trustA1==3,] , aes( x=abs(diffA1), y=trustA1, col=trustA1, alpha=0.3, width = .02 ) ) +
    plot_trustA1 <- plot_trustA1 + geom_violin( data=d[d$trustA1==4,] , aes( x=abs(diffA1), y=trustA1,alpha=0.3 )) #, draw_quantiles = c(0.25, 0.5, 0.75)  )
      # geom_jitter( data=d[d$trustA1==4,] , aes( x=abs(diffA1), y=trustA1,col=trustA1,alpha=0.3, width = .02 ) ) +
    plot_trustA1 <- plot_trustA1 + geom_violin( data=d[d$trustA1==5,] , aes( x=abs(diffA1), y=trustA1,alpha=0.3 ))+ #, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA1==5,] , aes( x=abs(diffA1), y=trustA1,col=trustA1,alpha=0.3, width = .02 ) ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Mean with Mask', y = "" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
    
    plot_trustA2 <- ggplot(d)+
      geom_vline(xintercept = 0) +
      geom_violin( data=d[d$trustA2==0,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==0,] , aes( x=abs(diffA2), y=trustA2, ,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA2==1,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==1,] , aes( x=abs(diffA2), y=trustA2, col=trustA2,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA2==2,] , aes( x=abs(diffA2), y=trustA2, alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==2,] , aes( x=abs(diffA2), y=trustA2,col=trustA2, alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA2==3,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==3,] , aes( x=abs(diffA2), y=trustA2, col=trustA2, alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA2==4,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==4,] , aes( x=abs(diffA2), y=trustA2,col=trustA2,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA2==5,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA2==5,] , aes( x=abs(diffA2), y=trustA2,col=trustA2,alpha=0.3 ) ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Overall Mean', y = "" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
    
    plot_trustA3 <- ggplot(d)+
      geom_vline(xintercept = 0) +
      geom_violin( data=d[d$trustA3==0,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==0,] , aes( x=abs(diffA3), y=trustA3, ,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA3==1,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==1,] , aes( x=abs(diffA3), y=trustA3, col=trustA3,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA3==2,] , aes( x=abs(diffA3), y=trustA3, alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==2,] , aes( x=abs(diffA3), y=trustA3,col=trustA3, alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA3==3,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==3,] , aes( x=abs(diffA3), y=trustA3, col=trustA3, alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA3==4,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==4,] , aes( x=abs(diffA3), y=trustA3,col=trustA3,alpha=0.3 ) ) +
      geom_violin( data=d[d$trustA3==5,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 ))+#, draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_jitter( data=d[d$trustA3==5,] , aes( x=abs(diffA3), y=trustA3,col=trustA3,alpha=0.3 ) ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) + 
      labs(title = 'Mask Proportion', y = "" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
    
    d_noneither <- filter_neitherLikert(d);
    if (filterNeither){
      plot_trustB <- ggplot(d_noneither)+
        geom_vline(xintercept = 0) +
        geom_violin( data=d_noneither[d_noneither$trustB==0,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 ) )+
        geom_violin( data=d_noneither[d_noneither$trustB==1,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
        geom_violin( data=d_noneither[d_noneither$trustB==2,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
        geom_violin( data=d_noneither[d_noneither$trustB==3,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
        geom_violin( data=d_noneither[d_noneither$trustB==4,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
        geom_violin( data=d_noneither[d_noneither$trustB==5,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
        facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") + 
        labs(title = 'Stability Comparison', y = "" ) +
        theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    } else {
    plot_trustB <- ggplot(d)+
      geom_vline(xintercept = 0) +
      geom_violin( data=d[d$trustB==0,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 ) )+# , draw_quantiles = c(0.25, 0.5, 0.75)  ) +
      # geom_dotplot( data=d[d$trustB==0,] , aes( x=abs(1-correctB), y=trustB, ,alpha=0.3, width = .02 ) ) +
      geom_violin( data=d[d$trustB==1,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
      # geom_dotplot( data=d[d$trustB==1,] , aes( x=abs(1-correctB), y=trustB, col=trustB,alpha=0.3, width = .02 ) ) +
      geom_violin( data=d[d$trustB==2,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
      # geom_dotplot( data=d[d$trustB==2,] , aes( x=abs(1-correctB), y=trustB,col=trustB, alpha=0.3, width = .02 ) ) +
      geom_violin( data=d[d$trustB==3,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
      # geom_dotplot( data=d[d$trustB==3,] , aes( x=abs(1-correctB), y=trustB, col=trustB, alpha=0.3, width = .02 ) ) +
      geom_violin( data=d[d$trustB==4,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
      # geom_dotplot( data=d[d$trustB==4,] , aes( x=abs(1-correctB), y=trustB,col=trustB,alpha=0.3, width = .02 ) ) +
      geom_violin( data=d[d$trustB==5,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
      # geom_dotplot( data=d[d$trustB==5,] , aes( x=abs(1-correctB), y=trustB,col=trustB,alpha=0.3, width = .02 ) ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") + 
      labs(title = 'Stability Comparison', y = "" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    }
    
  }
  else {
    cat("\nnumFactor==0")
    plot_trustA1 <- ggplot(d)+
      geom_vline(xintercept = 0) +
      geom_violin( data=d[d$trustA1==0,] , aes( x=abs(diffA1), y=trustA1,alpha=0.4 )  ) +
      geom_violin( data=d[d$trustA1==1,] , aes( x=abs(diffA1), y=trustA1,alpha=0.4 )  ) +
      geom_violin( data=d[d$trustA1==2,] , aes( x=abs(diffA1), y=trustA1,,alpha=0.4 )  ) +
      geom_violin( data=d[d$trustA1==3,] , aes( x=abs(diffA1), y=trustA1,alpha=0.4 )  ) +
      geom_violin( data=d[d$trustA1==4,] , aes( x=abs(diffA1), y=trustA1,alpha=0.4 )  ) +
      geom_violin( data=d[d$trustA1==5,] , aes( x=abs(diffA1), y=trustA1,alpha=0.4 )  ) +
      labs(title = 'Mean with Mask', y = "" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
        
      plot_trustA2 <- ggplot(d)+
        geom_vline(xintercept = 0) +
        geom_violin( data=d[d$trustA2==0,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA2==1,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA2==2,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA2==3,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA2==4,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA2==5,] , aes( x=abs(diffA2), y=trustA2,alpha=0.3 )  ) +
        labs(title = 'Mean Overall', y = "" ) +
        theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
      
      plot_trustA3 <- ggplot(d)+
        geom_vline(xintercept = 0) +
        geom_violin( data=d[d$trustA3==0,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA3==1,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA3==2,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA3==3,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA3==4,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        geom_violin( data=d[d$trustA3==5,] , aes( x=abs(diffA3), y=trustA3,alpha=0.3 )  ) +
        labs(title = 'Mask Proportion', y = "" ) +
        theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )

      d_noneither <- filter_neitherLikert(d);
      cat("\ncreated d_noneither, strFormula: ",strFormula);
      cat("\nd_noneither$correctB[d_noneither$trustB==0]: ",toString(d_noneither$correctB[d_noneither$trustB==0]));
      cat("\nd_noneither$correctB[d_noneither$trustB==1]: ",toString(d_noneither$correctB[d_noneither$trustB==1]));
      cat("\nd_noneither$correctB[d_noneither$trustB==2]: ",toString(d_noneither$correctB[d_noneither$trustB==2]));
      cat("\nd_noneither$correctB[d_noneither$trustB==3]: ",toString(d_noneither$correctB[d_noneither$trustB==3]));
      cat("\nd_noneither$correctB[d_noneither$trustB==4]: ",toString(d_noneither$correctB[d_noneither$trustB==4]));
      cat("\nd_noneither$correctB[d_noneither$trustB==5]: ",toString(d_noneither$correctB[d_noneither$trustB==5]));
      if (filterNeither){
        plot_trustB <- ggplot(d_noneither)+
          geom_vline(xintercept = 0) +
          geom_violin( data=d_noneither[d_noneither$trustB==0,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 ) )+# , draw_quantiles = c(0.25, 0.5, 0.75)  ) +
          geom_violin( data=d_noneither[d_noneither$trustB==1,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
          geom_violin( data=d_noneither[d_noneither$trustB==2,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d_noneither[d_noneither$trustB==3,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
          geom_violin( data=d_noneither[d_noneither$trustB==4,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
          geom_violin( data=d_noneither[d_noneither$trustB==5,] , aes( x=abs(1-correctB), y=trustB,alpha=0.3 )  ) +
          labs(title = 'Stability Comparison', y = "" ) +
          theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
      } else {
        plot_trustB <- ggplot(d)+
          geom_vline(xintercept = 0) +
          geom_violin( data=d[d$trustB==0,] , aes( x=  abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d[d$trustB==1,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d[d$trustB==2,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d[d$trustB==3,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d[d$trustB==4,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          geom_violin( data=d[d$trustB==5,] , aes( x=abs(1-correctB), y=trustB, alpha=0.3 )  ) +
          labs(title = 'Stability Comparison', y = "" ) +
          theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(),legend.position="none" )
      }
      cat("\nshould have plot_trustB...")
  }
  cat("\nnumFactor: ",numFactor,", numFactor: ",numFactor)
  cat("\ntoString(factorArr): ",toString(factorArr))

  strLeg <- ""
  if (numFactor>0){
    strLeg<- paste0(", factored by ",toString(factorArr) , sep="")
  }
  cat("\nabout to arrange")
  grid.arrange(grobs=list(plot_trustA1, plot_trustA2, plot_trustA3,plot_trustB), ncol=4,top=textGrob( paste0("Distribution of performances according to self-reported trust",strLeg, sep="")  ) )
  return (dfCI_global)
}


performancesB_correct_neither_incorrect <- function (d, factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE,
                                                     factorTrust=FALSE){
  cat("\n----\tperformancesB_correct_neither_incorrect")
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,factorTrust=factorTrust);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\ngen_res_trust }}}} factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  cat("\n\tfactor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)

  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[dfCI_global$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  
  
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  } 
  cat("\n\tstrFormula: ",strFormula)
  
  d <- renameGroupedData(d)
  
  # we need to modify the structure of the table... something like: cases where correct are labeled "correct", incorrect as "incorrect" and "neither" as "neither"...
  d$txtCorrectB <- NA
  d$txtCorrectB[d$correctB==1] <- "correct"
  d$txtCorrectB[d$correctB==0 & d$answerB=="Neither agree nor disagree"] <- "neither"
  d$txtCorrectB[d$correctB==0 & d$answerB!="Neither agree nor disagree"] <- "incorrect"
  
  if (numFactor > 0 ){
    distribB <- ggplot(d) + 
      scale_fill_manual(values = c("correct" = "green",
                                   "incorrect" = "red",
                                   "neither" = "#353535")) +
      geom_bar( data=d , aes( x= txtCorrectB, alpha=0.3, fill=txtCorrectB )  ) + 
      geom_text(
        aes(x= txtCorrectB, label = paste("Count: " ,stat(count),"\nPercentage: ", scales::percent(prop.table(stat(count))), sep="" ) ),
        stat='count',
        vjust = 1+(numFactor*0.15),
        size = 3-(numFactor*0.25)) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") + 
      labs(title = 'Distribution of SC', y = "" , x="" ) +
      theme(
        strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none"
      ) 
  } 
  else {
  distribB <- ggplot(d) + 
    scale_fill_manual(values = c("correct" = "green",
                                 "incorrect" = "red",
                                 "neither" = "#111111")) +
    geom_bar( data=d , aes( x= txtCorrectB, alpha=0.3, fill=txtCorrectB )  ) + 
    geom_text(
      aes(x= txtCorrectB, label = paste("Count: " ,stat(count),"\nPercentage: ", scales::percent(prop.table(stat(count))), sep="" ) ),
      stat='count',
      vjust = 3,
      size = 3 ) + 
    labs(title = 'Distribution of SC', y = "" , x="" ) +
    theme(
      strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none"
    )
  }
    
  grid.arrange(grobs=list(distribB), ncol=1 ) # ,top=textGrob( "Distribution of SC" )
  
  return (d)
}

performancesB_accordingToImportanceDifference <- function (d,   factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE,
                                                           factorTrust=FALSE){
  cat("\n----\tpperformancesB_accordingToImportanceDifference")
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,factorTrust=factorTrust);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\n}}}} factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  cat("\n\tfactor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)
  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[dfCI_global$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  } 
  cat("\n\tstrFormula: ",strFormula)
  
  d <- renameGroupedData(d)
  # we need to modify the structure of the table... something like: cases where correct are labeled "correct", incorrect as "incorrect" and "neither" as "neither"...
  d$txtCorrectB <- NA
  d$txtCorrectB[d$correctB==1] <- "correct"
  d$txtCorrectB[d$correctB==0 & d$answerB=="Neither agree nor disagree"] <- "neither"
  d$txtCorrectB[d$correctB==0 & d$answerB!="Neither agree nor disagree"] <- "incorrect"
  
  if (numFactor>0){
    ggplot(data=d)+
      geom_vline(xintercept = 0) +
      scale_fill_manual(values = c("correct" = "green",
                                   "incorrect" = "red",
                                   "neither" = "#111111")) +
      geom_jitter( data=d[d$focus=="WHAT_Ql",], aes( x=diffB, y=txtCorrectB, fill=txtCorrectB, col=txtCorrectB, alpha=0.3 )  ) +
      geom_jitter( data=d[d$focus=="WHAT_Qn",], aes( x=diffB, y=txtCorrectB, fill=txtCorrectB, col=txtCorrectB, alpha=0.3 )  ) +
      geom_jitter( data=d[d$focus=="WHERE",], aes( x=diffB, y=txtCorrectB, fill=txtCorrectB, col=txtCorrectB, alpha=0.3 )  ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") + 
      labs(title = 'Distribution of SC', y = "" , x="" ) +
      theme(
        strip.background = element_blank(), 
        # strip.text.x = element_blank(), 
        axis.ticks.y = element_blank(),  
        # axis.text.y = element_blank(), 
        legend.position="none"
      )
  } 
  else {
    gplot_ql <- ggplot(data=d)+
      geom_vline(xintercept = 0) +
      scale_fill_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      scale_colour_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +      
      geom_violin( data=d[d$focus=="WHAT_Ql",], aes( x=abs(diffB),y = focus, col=txtCorrectB, fill=txtCorrectB, alpha=0.3 )  )+
      labs(title = 'Distribution of SC for WHAT_Ql', y = "" , x="" )
    
    gplot_qn <- ggplot(data=d)+
      geom_vline(xintercept = 0) +
      scale_fill_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      scale_colour_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +      
      geom_violin( data=d[d$focus=="WHAT_Qn",], aes( x=abs(diffB),y = focus, col=txtCorrectB,fill=txtCorrectB,alpha=0.3 )  ) +
      labs(title = 'Distribution of SC for WHAT_Qn', y = "" , x="" )
    
    gplot_where <- ggplot(data=d)+
      geom_vline(xintercept = 0) +
      scale_fill_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      scale_colour_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +      
      geom_violin( data=d[d$focus=="WHERE",], aes( x=abs(diffB), y = focus, col=txtCorrectB,fill=txtCorrectB, alpha=0.3 )  ) +
      labs(title = 'Distribution of SC for WHERE', y = "" , x="" )
    
      # labs(title = 'Distribution of SC', y = "" , x="" ) +
      # theme(
      #   strip.background = element_blank(), 
      #   # strip.text.x = element_blank(), 
      #   axis.ticks.y = element_blank(),  
      #   # axis.text.y = element_blank(), .
      #   legend.position="none"
      # )
    grid.arrange(grobs=list(gplot_ql,gplot_qn,gplot_where), ncol=1 ) # ,top=textGrob( "Distribution of SC" )
    
  }
}

# may be imperfect, but time is against us:
# - for focus: get worst performance cntrQ for each
get_best_worst_perfoms_cntrQ <- function (d){

  # 3 focus * 3 worst = 9 cntrQ to check
  arrCntrQ_focusWorst_A1 <- c();
  d <- d[order(d$focus), ]
  
  d_ql <- d[d$focus=="WHAT_Ql",]
  cat("\nd_ql$cntrQ: ",d_ql$cntrQ)
  cat("\nd_ql$log_diffA1: ",d_ql$log_diffA1)
  d_qn <- d[d$focus=="WHAT_Qn",]
  d_where <- d[d$focus=="WHERE",]
  # worst ql
  d_ql <- d_ql[order(as.numeric(as.character(d_ql$log_diffA1)) ), ];
  cat("\nJUST ORDERED\t as.numeric(as.character(d_ql$log_diffA1)): ",d_ql$log_diffA1)
  cat("\n\td_ql$cntrQ: ",d_ql$cntrQ)
  
  arr_ql_CntrQ_focusWorst_A1 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
  d_ql <- d_ql[order(as.numeric(as.character(d_ql$log_diffA2)) ), ];
  arr_ql_CntrQ_focusWorst_A2 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
  d_ql <- d_ql[order(as.numeric(as.character( d_ql$log_diffA3)) ), ];
  arr_ql_CntrQ_focusWorst_A3 <- c( d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 2], d_ql$cntrQ[ length( d_ql$log_diffA1 ) - 1], d_ql$cntrQ[ length( d_ql$log_diffA1 )] )
  cat("\narr_ql_CntrQ_focusWorst_A1: ",arr_ql_CntrQ_focusWorst_A1)
  
  # worst qn
  d_qn <- d_qn[order( as.numeric(as.character( d_qn$log_diffA1)) ), ];
  arr_qn_CntrQ_focusWorst_A1 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
  d_qn <- d_qn[order( as.numeric(as.character( d_qn$log_diffA2)) ), ];
  arr_qn_CntrQ_focusWorst_A2 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
  d_qn <- d_qn[order( as.numeric(as.character( d_qn$log_diffA3)) ), ];
  arr_qn_CntrQ_focusWorst_A3 <- c( d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 2], d_qn$cntrQ[ length( d_qn$log_diffA1 ) - 1], d_qn$cntrQ[ length( d_qn$log_diffA1 )] )
  
  # worst where
  d_where <- d_where[order( as.numeric(as.character( d_where$log_diffA1)) ), ];
  arr_where_CntrQ_focusWorst_A1 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )
  d_where <- d_where[order( as.numeric(as.character( d_where$log_diffA2)) ), ];
  arr_where_CntrQ_focusWorst_A2 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )
  d_where <- d_where[order( as.numeric(as.character( d_where$log_diffA3)) ), ];
  arr_where_CntrQ_focusWorst_A3 <- c( d_where$cntrQ[ length( d_where$log_diffA1 ) - 2], d_where$cntrQ[ length( d_where$log_diffA1 ) - 1], d_where$cntrQ[ length( d_where$log_diffA1 )] )
  cat("\n****arr_where_CntrQ_focusWorst_A3: ",arr_where_CntrQ_focusWorst_A3)
  # worst SC
  # 1 - filter neither for SC
  d_noneither <- filter_neitherLikert(d)
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
  
  
  arr_what_ql_cntrQ_focusWorst_B <- c( c( df_noneither_ql$arr_cntrQ_wrong [ length( df_noneither_ql$arr_cntrQ_wrong  ) - 2], 
                                          df_noneither_ql$arr_cntrQ_wrong[ length( df_noneither_ql$arr_cntrQ_wrong ) - 1], 
                                          df_noneither_ql$arr_cntrQ_wrong[ length( df_noneither_ql$arr_cntrQ_wrong )] ) )
  
  arr_what_qn_cntrQ_focusWorst_B <- c( c( df_noneither_qn$arr_cntrQ_wrong [ length( df_noneither_qn$arr_cntrQ_wrong  ) - 2], 
                                          df_noneither_qn$arr_cntrQ_wrong[ length( df_noneither_qn$arr_cntrQ_wrong ) - 1], 
                                          df_noneither_qn$arr_cntrQ_wrong[ length( df_noneither_qn$arr_cntrQ_wrong )] ) )

  arr_where_cntrQ_focusWorst_B <- c( c( df_noneither_where$arr_cntrQ_wrong [ length( df_noneither_where$arr_cntrQ_wrong  ) - 2], 
                                           df_noneither_where$arr_cntrQ_wrong[ length( df_noneither_where$arr_cntrQ_wrong ) - 1], 
                                           df_noneither_where$arr_cntrQ_wrong[ length( df_noneither_where$arr_cntrQ_wrong )] ) )

  cat("\narr_qn_CntrQ_focusWorst_A1: ",arr_qn_CntrQ_focusWorst_A1)
  cat("\narr_where_cntrQ_focusWorst_B: ",arr_where_cntrQ_focusWorst_B)
  
  # arr to return total: arr_ql_CntrQ_focusWorst_A1 / A2 / A3, arr_qn_CntrQ_focusWorst_A1 / A2 / A3, arr_where_CntrQ_focusWorst_A1 / A2 /A3,arr_what_ql_cntrQ_focusWorst_B, arr_what_qn_cntrQ_focusWorst_B, arr_where_cntrQ_focusWorst_B
  resArr <- c(arr_ql_CntrQ_focusWorst_A1,arr_ql_CntrQ_focusWorst_A2,arr_ql_CntrQ_focusWorst_A3,
              arr_qn_CntrQ_focusWorst_A1,arr_qn_CntrQ_focusWorst_A2,arr_qn_CntrQ_focusWorst_A3,
              arr_where_CntrQ_focusWorst_A1, arr_where_CntrQ_focusWorst_A2, arr_where_CntrQ_focusWorst_A3,
              arr_what_ql_cntrQ_focusWorst_B,
              arr_what_qn_cntrQ_focusWorst_B,
              arr_where_cntrQ_focusWorst_B
              )
  
  return(resArr)
}



performances_accordingToCntrQ <- function(d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorTrust=FALSE){
  cat("\n----\tperformances_accordingToCntrQ")
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,factorTrust=factorTrust);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  cat("\n}}}} factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  cat("\n\tfactor1: ",factor1,", factor2: ",factor2,", factor3: ",factor3,", factor4: ",factor4)
  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[dfCI_global$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
      d$scaling[d$scaling==0] <- "Scaling 0";d$scaling[d$scaling==1] <- "Scaling 1";d$scaling[d$scaling==2] <- "Scaling 2";
    }
  } 
  
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula)
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences)
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  } 
  cat("\n\tstrFormula: ",strFormula)
  
  d <- renameGroupedData(d)
  # we need to modify the structure of the table... something like: cases where correct are labeled "correct", incorrect as "incorrect" and "neither" as "neither"...
  d$txtCorrectB <- NA
  d$txtCorrectB[d$correctB==1] <- "correct"
  d$txtCorrectB[d$correctB==0 & d$answerB=="Neither agree nor disagree"] <- "neither"
  d$txtCorrectB[d$correctB==0 & d$answerB!="Neither agree nor disagree"] <- "incorrect"
  
  d <- renameGroupedData(d)
  
  
  if (numFactor>0){
    
    distribA1 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA1), alpha=0.3, fill=abs(diffA1), col=abs(diffA1) )  ) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) +
      labs(title = 'Distribution of MwM', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )
    
    distribA2 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA2), alpha=0.3, fill=abs(diffA2), col=abs(diffA2) )  ) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) +
      labs(title = 'Distribution of MO', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )

    distribA3 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA3), alpha=0.3, fill=abs(diffA3), col=abs(diffA3) )  ) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1) +
      labs(title = 'Distribution of MP', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )

    distribB <- ggplot(d) + 
      scale_fill_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      scale_colour_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      geom_point( data=d , aes( x= cntrQ, y=txtCorrectB, alpha=0.3, fill=txtCorrectB, col=txtCorrectB )  ) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1, strip.position = "right") +
      labs(title = 'Distribution of SC', y = "" , x="" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    
  } else {

    distribA1 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA1), alpha=0.3, fill=abs(diffA1), col=abs(diffA1) )  ) + 
      labs(title = 'Distribution of MwM', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )
    
    distribA2 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA2), alpha=0.3, fill=abs(diffA2), col=abs(diffA2) )  ) + 
      labs(title = 'Distribution of MO', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )
    
    distribA3 <- ggplot(d) + 
      geom_point( data=d , aes( x= cntrQ, y=abs(diffA3), alpha=0.3, fill=abs(diffA3), col=abs(diffA3) )  ) + 
      labs(title = 'Distribution of MP', y = "" , x="" ) +
      theme( strip.background = element_blank(), strip.text.x = element_blank(), axis.ticks.y = element_blank(),  axis.text.y = element_blank(), legend.position="none" )
    
    distribB <- ggplot(d) + 
      scale_fill_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      scale_colour_manual(values = c("correct" = "green", "incorrect" = "red", "neither" = "#888888")) +
      geom_point( data=d , aes( x= cntrQ, y=txtCorrectB, alpha=0.3, fill=txtCorrectB, col=txtCorrectB )  ) + 
      labs(title = 'Distribution of SC', y = "" , x="" ) +
      theme(axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none")
    
  }
  
  grid.arrange(grobs=list(distribA1,distribA2,distribA3,distribB), ncol=4 )
  # ggplotly(distribA1)
  
}



# disregarded... for now
stat_ggplot_data <- function(y, upper_limit = max(iris$Sepal.Length) * 1.15) {
  d
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}

returnFactorsCombination <- function(factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorTrust=FALSE){
  cat("\nfactorScaling: ",factorScaling,", factorDistractor: ",factorDistractor,", factorFocus: ",factorFocus,", factorDMask: ",factorDMask,", factorDComplex_focus: ",factorDComplex_focus)  
  res <- c(NULL,NULL,NULL,NULL)
  factor1 <- NULL; factor2 <- NULL; factor3 <- NULL; factor4 <- NULL;
  if (factorScaling & factorDistractor){
    cat("\nError, we don't expect factorScaling & factorDistractor")
  }
  else if(factorScaling){
    # numGraphs <- max(1,factorScaling*length(arrScalings))*max(1,factorFocus*length(arrFocus))*max(1,factorDMask*length(arrMask))*max(1,factorDComplex_focus*length(arrDComplex_focus))
    factor1 <- "scaling";
    if (factorFocus){
      factor2 = "focus";
      if (factorDMask){
        factor3 = "dMask";
        if (factorDComplex_focus){
          factor4 <- "dComplex_focus";
        }
      }
    } else if(factorDMask){
      factor2 <- "dMask";
      if (factorDComplex_focus){
        factor3 <- "dComplex_focus";
      }
    } else if (factorDComplex_focus){
      factor2 <- "dComplex_focus";
    }
  } 
  else if (factorDistractor){
    factor1 <- "distractor";
    if (factorFocus){
      factor2 = "focus";
      if (factorDMask){
        factor3 = "dMask";
        if (factorDComplex_focus){
          factor4 <- "dComplex_focus";
        }
      } else if (factorDComplex_focus){ # doubt here...
        factor3 <- "dComplex_focus"
      }
    } else if(factorDMask){
      factor2 <- "dMask";
      if (factorDComplex_focus){
        factor3 <- "dComplex_focus";
      }
    } else if (factorDComplex_focus){
      factor2 <- "dComplex_focus";
    }
  } 
  else if (factorTrust){ # make more variations...? what could we do for scaling and distractor? Might not work! It's not 1 column, it's 4!
    factor1 <- "trust"
    if (factorFocus){
      factor2 = "focus";
      if (factorDMask){
        factor3 = "dMask";
        if (factorDComplex_focus){
          factor4 <- "dComplex_focus";
        }
      }
    }
  }
  else {
    # numGraphs <- max(1,factorFocus*length(arrFocus))*max(1,factorDMask*length(arrMask))*max(1,factorDComplex_focus*length(arrDComplex_focus))    
    if (factorFocus){
      factor1 = "focus";
      if (factorDMask){
        factor2 = "dMask";
        if (factorDComplex_focus){
          factor3 <- "dComplex_focus";
        }
      } 
      else{
        if (factorDComplex_focus){
          factor2 <- "dComplex_focus";
        }
      }
    } 
    else if(factorDMask){
      factor1 <- "dMask";
      if (factorDComplex_focus){
        factor2 <- "dComplex_focus";
      }
    } 
    else if (factorDComplex_focus){
      factor1 <- "dComplex_focus";
    }
  }
  
  res <- c(factor1,factor2,factor3,factor4)
  return (res)
}

addReverseB <- function (d){
  d$reverseB <- abs(d$correctB -1);
  return(d);
}

generateGroupedData <- function (d){
  if (!is.null(d$scaling))
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask,scaling) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  } 
  else if (!is.null(d$distractor))
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask,distractor) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  } 
  else 
  {
    groupedData_all <- d %>%
      group_by(focus,info_focus_dComplex_dMask,dComplex_focus,dMask) %>%
      summarize(mean_diffA1 = mean(diffA1), sd_diffA1 = sd(diffA1, na.rm=TRUE), count=n(),se_diffA1=(sd_diffA1/(sqrt(count))),
                mean_diffA2 = mean(diffA2), sd_diffA2 = sd(diffA2, na.rm=TRUE), count=n(),se_diffA2=(sd_diffA2/(sqrt(count))),
                mean_diffA3 = mean(diffA3), sd_diffA3 = sd(diffA3, na.rm=TRUE), count=n(),se_diffA3=(sd_diffA3/(sqrt(count))),
                mean_correctB = mean(correctB), sd_correctB = sd(correctB, na.rm=TRUE), count=n(),se_correctB=(sd_correctB/(sqrt(count)))
      )
  }
  groupedData_all["low_ci_DiffA1"] <- NA; groupedData_all["high_ci_DiffA1"] <-NA; 
  groupedData_all["low_ci_DiffA2"] <-NA; groupedData_all["high_ci_DiffA2"] <-NA; 
  groupedData_all["low_ci_DiffA3"] <-NA; groupedData_all["high_ci_DiffA3"] <-NA;
  groupedData_all["mean_t0_DiffA1"]<-NA;groupedData_all["mean_t0_DiffA2"]<-NA;groupedData_all["mean_t0_DiffA3"]<-NA;
  return (groupedData_all)
}

filter_neitherLikert <- function (d){
  d_filtered <- d[d$answerB!="Neither agree nor disagree",]
  return (d_filtered)
}

# overall question: do we disregard the entire set of answers from a participant when they provide one problematic answer, or do we keep the rest? Let's start by being throrough in the data removal.
# about the reported trust: do we disregard the whole set of answers of a participant if they once answered 0 to all trust records for one stimuli, or only for that one stimuli...?
# impossibilities: we know of the cases for WHAT_Ql, but are there other impossibilities?
filter_someTrust0or5_impossibleQualAnswer <- function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[ (d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | 
                                                 (is.na(d$correctB)) |
                                                 (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d <- d[which(!(d$ResponseId %in% toFilter_ResponsesId)),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

filter_allTrust0or5_impossibleQualAnswer <- function (d){
  d <- enrichData_withTrustAll0or5(d)
  toFilter_ResponsesId <- unique(d$ResponseId[ (d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | 
                                                 (is.na(d$correctB)) |
                                                 (d$allSameTrust) ])
  d <- d[which(!(d$ResponseId %in% toFilter_ResponsesId)),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

filter_getWrongParticipants <- function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[(d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | 
                                                (is.na(d$correctB)) | (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d <- d[which(d$ResponseId %in% toFilter_ResponsesId),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

enrichData_withTrustSome0or5 <- function (d){
  d$someSameTrust <- NA;
  uniqueResponsesId <- unique(d$ResponseId);
  for (i in 1:length(uniqueResponsesId)){
    trustsA1 <- d$trustA1[d$ResponseId == uniqueResponsesId[i]];trustsA2 <- d$trustA2[d$ResponseId == uniqueResponsesId[i]];trustsA3 <- d$trustA3[d$ResponseId == uniqueResponsesId[i]];trustsB <- d$trustB[d$ResponseId == uniqueResponsesId[i]];
    # allTrustA1 <- allSame(d$trustA1[d$ResponseId==uniqueResponsesId[i]]);allTrustA2 <- allSame(d$trustA2[d$ResponseId==uniqueResponsesId[i]]);allTrustA3 <- allSame(d$trustA3[d$ResponseId==uniqueResponsesId[i]]);allTrustB  <- allSame(d$trustB [d$ResponseId==uniqueResponsesId[i]])
    anyCaseAll0or5 <- FALSE;
    for(j in 1:length(trustsA1)){
      if (trustsA1[j]==trustsA2[j]&trustsA2[j]==trustsA3[j]&trustsA3[j]==trustsB[j] & (trustsB[j]==0 | trustsB[j]==5) ){
        anyCaseAll0or5 <- TRUE
      }
    }
    if (anyCaseAll0or5){
      d$someSameTrust[d$ResponseId==uniqueResponsesId[i]] <- TRUE;
    }
    else {
      d$someSameTrust[d$ResponseId==uniqueResponsesId[i]] <- FALSE;
    }
  }
  return (d)
}

enrichData_withTrustAll0or5 <- function (d){
  d$allSameTrust <- NA;
  uniqueResponsesId <- unique(d$ResponseId);
  # cAllSame <- c(1:length(uniqueResponsesId));
  for (i in 1:length(uniqueResponsesId)){
    allTrustA1 <- allSame(d$trustA1[d$ResponseId==uniqueResponsesId[i]])
    allTrustA2 <- allSame(d$trustA2[d$ResponseId==uniqueResponsesId[i]])
    allTrustA3 <- allSame(d$trustA3[d$ResponseId==uniqueResponsesId[i]])
    allTrustB  <- allSame(d$trustB [d$ResponseId==uniqueResponsesId[i]])
    if (allTrustA1 & allTrustA2 & allTrustA3 & allTrustB & (d$trustA1[d$ResponseId==uniqueResponsesId[i]]==0 | d$trustA1[d$ResponseId==uniqueResponsesId[i]]==5)){
      # cAllSame[i] <- TRUE;
      d$allSameTrust[d$ResponseId==uniqueResponsesId[i]] <- TRUE;
    }
    else {
      # cAllSame[i] <- FALSE;
      d$allSameTrust[d$ResponseId==uniqueResponsesId[i]] <- FALSE;
    }
    # d$allSameTrust[d$ResponseId==uniqueResponsesId] <- cAllSame[i];
  }
  
  # toFilter_ResponsesId <- unique(d$ResponseId[ (d$fd_altocus=="WHAT_Ql" & d$answerA1 > d$answerA2) | 
  #                                                (is.na(d$correctB)) |
  #                                                (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  # d <- d[which(!(d$ResponseId %in% toFilter_ResponsesId)),] # remove the cases of impossible answer and trusts being all at 0 or all at 5 for a stimuli.
  return (d)
}

enrichData_impossibleQualAnswer <- function(d){
  d$impossibleQualAnswer <- NA;
  d$impossibleQualAnswer <- d$focus=="WHAT_Ql" & (d$answerA1>d$answerA2 | d$answerA1>d$answerA3)
  return (d)
}

enrich_absDiffs <- function (d){
  d$abs_diffA1 <- NA; d$abs_diffA2 <- NA; d$abs_diffA3 <- NA;
  d$abs_diffA1 <- abs(d$diffA1)
  d$abs_diffA2 <- abs(d$diffA2)
  d$abs_diffA3 <- abs(d$diffA3)
  return(d)
}

# function to enrich with number of errors... (function to work and without making differences for neither, i.e. call after filtering out neither)
getDF_WrongBCntrQ <- function(d){
  d$numErrorB <- NA;
  arr_cntrQ_wrong <- d$cntrQ[d$correctB==0]
  arr_amountWrong <- c();
  for (i in  arr_cntrQ_wrong){
    arr_amountWrong <- c( arr_amountWrong, sum(match(arr_cntrQ_wrong, i, nomatch=0))   )
  }
  cat("\narr_amountWrong: ",arr_amountWrong)
  dfRes <- data.frame(arr_cntrQ_wrong,arr_amountWrong)
  return (dfRes)
}


makeSummaryString <- function (d, factorScaling=FALSE, factorDistractor=FALSE, factorDMask= FALSE, factorFocus=FALSE, factorDComplex_focus=FALSE, 
                               factorDifference="",arrMixOrderFormula=c(),useLogDiff=TRUE){
  cat("\n--makeSummaryString--\n")
  
  df_summary <- data.frame();
  df_summary$RF1<-c();	df_summary$RF2<-c();	df_summary$RF3<-c();	df_summary$RF4<-c();	df_summary$RF5<-c();	df_summary$RF6<-c();	df_summary$RF7<-c();	df_summary$RF8<-c();	df_summary$RF9<-c();	
  df_summary$MwM1<-c();	df_summary$MwM2<-c(); df_summary$MwM3<-c();  df_summary$MwM4<-c();	df_summary$MwM5<-c();	df_summary$MwM6<-c(); df_summary$MwM7<-c();	df_summary$MwM8<-c();	df_summary$MwM9<-c();	
  df_summary$OM1<- c();	df_summary$OM2<-c();	df_summary$OM3<-c();	df_summary$OM4<-c();	df_summary$OM5<-c();	df_summary$OM6<-c();	df_summary$OM7<-c(); df_summary$OM8<-c();	df_summary$OM9;
  df_summary$MP1<-c();	df_summary$MP2<-c(); df_summary$MP3<-c();	df_summary$MP4<-c();	df_summary$MP5<-c(); df_summary$MP6<-c();	df_summary$MP7<-c(); df_summary$MP8<-c();	df_summary$MP9<-c();
  df_summary$SC1<-c();	df_summary$SC2<-c(); df_summary$SC3<-c();	df_summary$SC4<-c();	df_summary$SC5<-c(); df_summary$SC6<-c(); df_summary$SC7<-c();	df_summary$SC8<-c();	df_summary$SC9<-c();
  
  # d[sort(d$focus)] # https://stackoverflow.com/questions/51501989/how-to-sort-data-by-column-in-descending-order-in-r
  d <- d[order(d$focus), ]
  if (!is.null(d$distractor)){d <- d[order(d$distractor), ]}  # necessary? useful?
  if (!is.null(d$scaling)){d <- d[order(d$scaling), ]}  # necessary? useful?
  
  # d_noneither <- filter_neitherLikert(d) # not useful here...
  
  factorVariation <- factorDifference
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Ql","WHAT_Qn","WHERE"); arrMask <- c("easy","medium","hard"); 
  arrDComplex_focus <- c("E","M","H");  
  # arrTrust  <- c("trustA1","trustA2","trustA3","trustB")
  if (factorScaling | factorVariation=="scaling"){arrFocus <- c("WHAT_Ql","WHAT_Qn")}  
  
  # IMPORTANT NOTE ABOUT logFunction. If we use the log_diffA1 instead of diffA1, then isn't the usage again of the formula transforming it again?! To think about
  arrQuestions <- c();
  if (!useLogDiff){ arrQuestions <- c("diffA1","diffA2","diffA3"); } else { arrQuestions <- c("log_diffA1","log_diffA2","log_diffA3"); }
  
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus,
                                        # factorTrust=factorTrust
                                        );
  
  cat("\nfactorArr: ",factorArr);
  numFactor <- length(factorArr)
  if (length(arrMixOrderFormula) == 0){
    factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]  
  } 
  else {
    factor1 <- factorArr[arrMixOrderFormula[1]]; 
    factor2 <- factorArr[arrMixOrderFormula[2]]; 
    factor3 <- factorArr[arrMixOrderFormula[3]]; 
    factor4 <- factorArr[arrMixOrderFormula[4]];
  }
  
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else if (factor1=="trust"){
      arrFactor1 <- arrTrust
    }
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
      else if (factor2=="trust"){
        arrFactor2 <- arrTrust
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
      else if (factor3 == "trust"){
        arrFactor3 <- arrTrust
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  arrFactorVariations <- c()
  if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  arrFactorDifferences <- c()
  if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}  
  
  cat("\nok no crash")
  # TODO change
  if (numFactor== 1 ){
    factorArr[2] <- factorArr[1]; factorArr[3] <- factorArr[1];
    maxIndxF1 <- 3;
    maxIndxF2 <- 1;
    maxIndxF3 <- 1;
  } 
  else if (numFactor== 2){
    factorArr[3] <- factorArr[1];
    maxIndxF1 <- 3;
    maxIndxF2 <- 3;
    maxIndxF3 <- 1;
  } 
  else if (numFactor== 3){
    maxIndxF1 <- 3;
    maxIndxF2 <- 3;
    maxIndxF3 <- 3;
  }
  
  maxIndxF1 <- 3; maxIndxF2 <- 3; maxIndxF3 <- 3;
  
  arrFactorization1 <- c(); arrFactorization2 <- c(); arrFactorization3 <- c();
  if (!is.null(factorArr[1])){
    if (factorArr[1] =="focus" ){ arrFactorization1 <- arrFocus } else if (factorArr[1] == "dMask"){ arrFactorization1 <- arrMask;}
    else if (factorArr[1] =="dComplex_focus"){arrFactorization1 <- arrDComplex_focus } else if (factorArr[1] =="scaling"){arrFactorization1 <- arrScalings }
    else if (factorArr[1] =="distractor"){arrFactorization1 <- arrDistractor }
  }
  if (!is.null(factorArr[2])){
    if (factorArr[2] =="focus" ){ arrFactorization2 <- arrFocus } else if (factorArr[2] == "dMask"){ arrFactorization2 <- arrMask;}
    else if (factorArr[2] =="dComplex_focus"){arrFactorization2 <- arrDComplex_focus } else if (factorArr[2] =="scaling"){arrFactorization2 <- arrScalings }
    else if (factorArr[2] =="distractor"){arrFactorization2 <- arrDistractor }
  }
  if (!is.null(factorArr[3])){
    if (factorArr[3] =="focus" ){ arrFactorization3 <- arrFocus } else if (factorArr[3] == "dMask"){ arrFactorization3 <- arrMask;}
    else if (factorArr[3] =="dComplex_focus"){arrFactorization3 <- arrDComplex_focus } else if (factorArr[3] =="scaling"){arrFactorization3 <- arrScalings }
    else if (factorArr[3] =="distractor"){arrFactorization3 <- arrDistractor }
  }
  
  cat("\narrFactorization1: ",toString(arrFactorization1));
  cat("\narrFactorization2: ",toString(arrFactorization2));
  cat("\narrFactorization3: ",toString(arrFactorization3));
  
  if (numFactor>0){
    d_neither <- filter_neitherLikert(d);
    cat("\nfactor1,factor2,factor3,# responses", ",MwM ",",OM",",MP ",",mean SC",",error rate SC,", ",log MwM", ",log MO",",log MP",",#responses noNeither",",mean noNeither SC",",error rate noNeither SC,str\n")
    indxF1 <- 0; indxF2 <- 0; indxF3 <- 0;
    for (f1 in arrFactorization1){
      # cat("\n--f1: ",f1,", indxF1: ",indxF1,", maxIndxF1: ",maxIndxF1)
      # if (numFactor<2){indxF2 <- indxF1}
      indxF2 <- 0;
      if (indxF1 < maxIndxF1){
        for (f2 in arrFactorization2){
          # cat("\n----f2: ",f2,", indxF2: ",indxF2,", maxIndxF2: ",maxIndxF2)
          # if (numFactor<3){indxF3 <- indxF1}
          indxF3 <- 0
          if (indxF2 < maxIndxF2){
            for (f3 in arrFactorization3){
              # cat("\n------f3: ",f3,", indxF3: ",indxF3,", maxIndxF3: ",maxIndxF3)
              if (indxF3 < maxIndxF3){
                # cat("\n\t\t\tf1: ",f1,", f2:",f2,", f3:",f3)
                # cat("\nindxF1: ",indxF1,", indxF2: ",indxF2,", indxF3: ",indxF3)
                factor1 <- getFactorNameFromVal(f1);factor2 <- getFactorNameFromVal(f2);factor3 <- getFactorNameFromVal(f3);
                str <- paste0(f1)
                strF2 <- f2; strF3 <- f3
                if (factor2 == factor1){factor2<-""; strF2<-""} else {str <- paste0(str,'-',f2)}
                if (factor3 == factor1){factor3<-""; strF3<-""} else {str <- paste0(str,'-',f3)}
                
                if (length(d$diffA1[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3]) != 0)
                {
                  # cat("\nfactor1,factor2,factor3,# responses", ",MwM ",",OM",",MP ",",mean SC",",error rate SC,", ",log MwM", ",log MO",",log MP",",#responses noNeither",",mean noNeither SC",",error rate noNeither SC,str\n")
                  cat("\n", factor1,",",factor2,",",factor3,",",
                    length(d$diffA1[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3]),",",
                    mean(abs(d$diffA1[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),",",
                    mean(abs(d$diffA2[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),",",
                    mean(abs(d$diffA3[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),",",
                    mean(abs(d$correctB[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),",",
                    mean(abs(1-d$correctB[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),
                    ",",mean(abs(d$log_diffA1[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),
                    ",",mean(abs(d$log_diffA2[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),
                    ",",mean(abs(d$log_diffA3[d[[factorArr[1]]] == f1 & d[[factorArr[2]]] == f2 & d[[factorArr[3]]] == f3])),
                    ",",length(d_neither$diffA1[d_neither[[factorArr[1]]] == f1 & d_neither[[factorArr[2]]] == f2 & d_neither[[factorArr[3]]] == f3]),
                    ",", mean(abs(d_neither$correctB[d_neither[[factorArr[1]]] == f1 & d_neither[[factorArr[2]]] == f2 & d_neither[[factorArr[3]]] == f3])),
                    ",",mean(abs(1-d_neither$correctB[d_neither[[factorArr[1]]] == f1 & d_neither[[factorArr[2]]] == f2 & d_neither[[factorArr[3]]] == f3])),
                    ",",str                  
                  );
                }
                # df_summary
              }
              indxF3 <- indxF3 +1;
            }
          }
          indxF2 <- indxF2 +1;
        }
      }
      indxF1 <- indxF1 +1;
    }
  } else {
    d_neither <- filter_neitherLikert(d);
    
    cat("\nfactor1,factor2,factor3,# responses", ",MwM ",",OM",",MP",",mean SC",",error rate SC", ",log MwM", ",log MO",",log MP",",#responses noNeither",",mean noNeither SC",",error rate noNeither SC",",str\n")
    cat(" , , ,",
        length(d$diffA1),",",mean(abs(d$diffA1)),",",mean(abs(d$diffA2)),",",mean(abs(d$diffA3)),",", mean(abs(d$correctB)),",",mean(abs(1-d$correctB)),
        ",",mean(abs(d$log_diffA1)),",",mean(abs(d$log_diffA2)),",",mean(abs(d$log_diffA3)),
        ",",length(d_neither$diffA1),",", mean(abs(d_neither$correctB)),",",mean(abs(1-d_neither$correctB)),
        ","," "
        
    );
    
  }

}


getFactorNameFromVal <- function(val){
  if (val == "WHAT_Ql" | val== "WHAT_Qn" | val == "WHERE"){
    return ("focus")
  } else if (val == "easy" | val == "medium" | val == "hard"){
    return ("dMask")
  } else if (val == 'E' | val == "M" | val == "H"){
    return ("dComplex_focus")
  } else if (val == 0 | val ==1 | val == 2){
    return ("scaling")
  } else if (val == "h" | val == "n"){
    return ("distractor")
  } else {
    return ("")
  }
}

prettyEnrichOrderCategory <- function (d,dfCI){
  cat("\n&&&&&&prettyEnrichOrderCategory&&&&&& toString(dfCI$orderCategoryCombination[1]): ",toString(dfCI$orderCategoryCombination[1])) 
  d$orderCategoryCombination <- NA;
  # cat("\n~dfCI$orderCategoryCombination: ",dfCI$orderCategoryCombination,", dfCI$orderCategoryCombination[1]: ",dfCI$orderCategoryCombination[1], "\n\t toString(dfCI$orderCategoryCombination): ", toString(dfCI$orderCategoryCombination))
  is_focus_complexity <- str_detect(toString(dfCI$orderCategoryCombination[1]) , "focus complexity:" )
  is_focus <- str_detect(toString(dfCI$orderCategoryCombination[1]) , "focus:" )
  is_mask <- str_detect(toString(dfCI$orderCategoryCombination[1]) , "Mask:" )
  is_distractor <- str_detect(toString(dfCI$orderCategoryCombination[1]) , "distractor:" )
  is_scaling <- str_detect(toString(dfCI$orderCategoryCombination[1]) , "scaling:" )
  
  cat("\n--\tis_focus_complexity: ",is_focus_complexity,", is_focus: ",is_focus,", is_mask: ",is_mask,", is_distractor: ",is_distractor,", is_scaling: ",is_scaling)
  selecColName <- ""
  nameColPaste <- ""
  if (is_focus_complexity){
    selecColName <- "dComplex_focus"; nameColPaste <- "focus complexity"
  }  
  else if (is_focus){
    selecColName <- "focus"; nameColPaste<- "focus"
  } 
  else if (is_mask){
    selecColName <- "dMask"; nameColPaste <- "Mask"
  } 
  else if (is_distractor){
    selecColName <- "distractor"; nameColPaste <- "distractor"
  } 
  else if (is_scaling){
    selecColName <- "scaling"; nameColPaste <- "scaling"
  }
  # cat('\n##\t selecColName: ',selecColName,', nameColPaste: ',nameColPaste,"\nunique(dfCI$orderCategoryCombination): ",unique(dfCI$orderCategoryCombination), "\ntoString(unique(dfCI$orderCategoryCombination)): ",toString(unique(dfCI$orderCategoryCombination)))
  d$categoryCombination  <- NA;
  arrStrCategoryCombination <- c()
  for (i in 1:length(select(d,selecColName)[,1] ) ) {
    valVariant <- select(d,selecColName)[i,1]
    if ( identical(valVariant,"E") ){
      valVariant <- "Easy"
    } 
    else if ( identical(valVariant,"M") ){
      valVariant <- "Medium"
    } 
    else if ( identical(valVariant,"H") ){
      valVariant <- "Hard"
    } 
    else if ( identical(valVariant,"easy") ){
      valVariant <- "Easy"
    } 
    else if ( identical(valVariant,"medium") ){
      valVariant <- "Medium"
    } 
    else if ( identical(valVariant,"hard") ){
      valVariant <- "Hard"
    } 
    else if (identical(valVariant,"WHAT_Qn") ){
      # cat("_identical WHAT_Qn_")
      valVariant <- "WHAT_Qn"
    }
    else if (identical(valVariant,"WHAT_Ql") ){
      # cat("_identical WHAT_Ql_")
      valVariant <- "WHAT_Ql"
    }
    else if (identical(valVariant,"WHERE") ){
      # cat("_identical WHERE_")
      valVariant <- "WHERE"
    }
    # cat("\tTo assign valVariant: ",valVariant)
    # d$orderCategoryCombination <- paste( nameColPaste,": ",valVariant, sep="" )
    arrStrCategoryCombination <- c( arrStrCategoryCombination, paste( nameColPaste,": ",valVariant, sep="" ) )
  }
  cat("\nㄴㄴ\tunique(arrStrCategoryCombination): ",unique(arrStrCategoryCombination))
  
  # what if two factors...
  if (!is.null(dfCI$orderMaskComplex[1])){
    d$orderMaskComplex  <- NA;
    d$orderMaskComplex[d$dMask == "easy"] = "Mask Easy"
    d$orderMaskComplex[d$dMask == "medium"] = "Mask Medium"
    d$orderMaskComplex[d$dMask == "hard"] = "Mask Hard"
    # d$orderMaskComplex <- factor(d$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
    cat("\n[[[[\t같같d$orderMaskComplex[1]: ", d$orderMaskComplex[1])
  }
  if (!is.null(dfCI$orderFocusComplex[1])){
    d$orderFocusComplex  <- NA;
    d$orderFocusComplex[d$dComplex_focus == "E"] = "Focus Easy"
    d$orderFocusComplex[d$dComplex_focus == "M"] = "Focus Medium"
    d$orderFocusComplex[d$dComplex_focus == "H"] = "Focus Hard"
    # d$orderMaskComplex <- factor(d$dMask,c("Mask Easy","Mask Medium","Mask Hard"))
    cat("\n[[[[\t같같d$orderFocusComplex[1]: ", d$orderFocusComplex[1])
  }
  
  d$category_combination <- arrStrCategoryCombination
  # cat("\n\t\tunique(d$category_combination): ",unique(d$category_combination),"\n***")
  # I think we might have to factor to ensure the ordering goes as it should...?
  
  if (is_focus_complexity){
    d$orderCategoryCombination <- factor(d$category_combination,c("focus complexity: Hard","focus complexity: Medium","focus complexity: Easy"))
  } else if (is_mask){
    d$orderCategoryCombination <- factor(d$category_combination,c("Mask: Hard","Mask: Medium","Mask: Easy"))
    d$orderMaskComplex <- factor(d$category_combination,c("Mask: Hard","Mask: Medium","Mask: Easy"))
  } else if (is_focus){
    d$orderCategoryCombination <- factor(d$category_combination,c("focus: WHERE","focus: WHAT_Qn","focus: WHAT_Ql"))
  } else if (is_distractor) {
    d$orderCategoryCombination <- factor(d$category_combination,c("distractor: n","distractor: h"))
  } else if (is_scaling){
    d$orderCategoryCombination <- factor(d$category_combination,c("scaling: 2","scaling: 1","scaling: 0"))
  } 
  
  View(d)
  View(dfCI)
  
  cat("\n||\tunique(dfCI$orderCategoryCombination): ",unique(dfCI$orderCategoryCombination), "\n||\tunique(d$orderCategoryCombination): ",unique(d$orderCategoryCombination))
  cat("\n\t|||| class(dfCI$orderCategoryCombination): ",class(dfCI$orderCategoryCombination),", class(dfCI$orderCategoryCombination[1]): ",class(dfCI$orderCategoryCombination[1]))
  cat("\n\t|||| class(d$orderCategoryCombination): ",class(d$orderCategoryCombination),", class(d$orderCategoryCombination[1]): ",class(d$orderCategoryCombination[1]))
  cat("\n&&&&&&")
  return (d);
}

enrichData_wasCntrQFaulty <- function (d){
  arrCntrQFaultry <- c(425, 352, 439, 412, 403, 376, 377, 356, 464, 375, 366, 354, 357, 438, 362, 360, 372, 345, 454, 373, 359, 340, 421, 448, 482, 347, 374, 363, 446, 364, 445, 361, 442, 415, 343, 370, 451, 378, 351, 353, 355, 452, 371, 379, 396, 342, 369, 476, 385, 466, 404, 383, 402, 399, 381, 384, 389, 387, 481, 400, 386, 475, 394, 367, 428, 455, 401, 390, 473, 391, 472, 388, 469, 397, 478, 405, 380, 382, 398, 479, 109, 126, 99, 206, 179, 196, 115, 133, 106, 160, 134, 107, 167, 194, 113, 132, 105, 123, 96, 111, 114, 119, 117, 129, 102, 211, 130, 116, 97, 205, 124, 178, 212, 185, 131, 104, 203, 176, 202, 121, 118, 199, 172, 127, 100, 208, 135, 108, 110, 112, 128, 209, 236, 136, 153, 233, 142, 223, 161, 221, 140, 159, 150, 138, 141, 146, 144, 156, 238, 157, 143, 232, 151, 239, 158, 120, 230, 148, 229, 145, 226, 154, 235, 162, 137, 139, 155, 147)
  cntrQWasFaulty <- d$cntrQ %in% arrCntrQFaultry
  d$cntrQWasFaulty <- cntrQWasFaulty
  return(d)
}


modify_d_OkOrNot <-function (d){
  toFilter_ResponsesId <- unique(d$ResponseId[(d$focus=="WHAT_Ql" & d$answerA1 > d$answerA2) | (d$trustA1==d$trustA2 & d$trustA2==d$trustA3 & d$trustA3==d$trustB & (d$trustB==0 | d$trustB==5)) ])
  d$passedFilter <- NA
  d$passedFilter[which(d$ResponseId %in% toFilter_ResponsesId)] <- FALSE
  d$passedFilter[which(!(d$ResponseId %in% toFilter_ResponsesId))] <- TRUE
  return (d)
}

addInfoCiDifferenceSignificant<-function(d){
  d$significantDifference <- NA
  d$sizeSignificantDifference <- NA
  for(i in 1:length(d$low_CI)){
    if ((d[i,]$low_CI > 0 & d[i,]$high_CI > 0) | (d[i,]$low_CI < 0 & d[i,]$high_CI < 0)){
      d[i,]$significantDifference <- TRUE
      d[i,]$sizeSignificantDifference <- 1
    } else {
      d[i,]$significantDifference <- FALSE
      d[i,]$sizeSignificantDifference <- 0
    }
  }
  return (d)
}
# dfCI_test_differences<-addInfoCiDifferenceSignificant(dfCI_test_differences)

# d_alt_enrichedFilter <- modify_d_OkOrNot(d_alt)
# dim(d_alt)
# d_alt_Right <- filter_someTrust0or5_impossibleQualAnswer(d_alt)
# dim(d_alt_Right)
# d_alt_Wrong <- filter_getWrongParticipants(d_alt)
# dim(d_alt_Wrong)

# TODO do we need this anymore?
# TODO FIX!!! potentially buggy!!! 
# TODO take pieces of the code from genAndPlot_differences_factorBased to adapt the factoring dynamically but with the generation (and display?) of each category
setGroupDataCI <- function(groupedData_all,d,scaling=FALSE,distractor=FALSE,focus=FALSE,dMask=FALSE,dComplex_focus=FALSE){
  arrFocus_scaling <- c("WHAT_Qn","WHAT_Ql");
  arrFocus_measurement <- c("WHAT_Qn","WHAT_Ql","WHERE");
  arrDMask <- c("easy","medium","hard");
  arrDComplex_focus <- c("E","M","H");
  arrDistractor  <- c("h","n");
  for (k in 1:3){
    strDiff <- paste("diffA",k,sep="");
    strMean_t0 <- paste("mean_t0_DiffA",k,sep="");strlow_ci <- paste("low_ci_DiffA",k,sep=""); strhigh_ci <- paste("high_ci_DiffA",k,sep="");
    # ######## Scaling part
    if (scaling & !focus & !dMask & !dComplex_focus) {
      cat("\nscaling")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff)
      for (j in 1:3){
        groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1)] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1),])[1])
      }
    } 
    else if (scaling & !focus & dMask & !dComplex_focus) {
      cat("\nscaling X dMask");
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,"",arrDMask[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        }
      }
    } 
    else if (scaling & focus & !dMask & dComplex_focus) {
      cat("\nscaling X focus X dComplex_focus");
      for (f in 1:length(arrFocus_scaling)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,arrFocus_scaling[f],"",arrDComplex_focus[cf])
          for (j in 1:3){
            rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci)
            groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
            groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
            groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
          }
        }
      }
    }
    else if(scaling & focus & !dMask & !dComplex_focus) {
      cat("\nscaling X focus");
      for (i in 1:length(arrFocus_scaling)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,arrFocus_scaling[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$focus == arrFocus_scaling[i] ] <- rep_high_ci
        }
      }
    } 
    else if(scaling & !focus & !dMask & dComplex_focus){
      cat("\nscaling X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_sclDependent(d,strDiff,"","",arrDComplex_focus[i])
        for (j in 1:3){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$scaling == (j-1) & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
        }
      }
    } # ########## Distractor part
    else if (distractor & !focus & !dMask & !dComplex_focus) {
      cat("\ndistractor")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      for (j in 1:length(arrDistractor)){
        groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
        groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] ] <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j],])[1])
      }
    } 
    else if (distractor & !focus & dMask & !dComplex_focus) {
      cat("\ndistractor X dMask");
      # & groupedData_all$scaling == arrDMask[i]
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,"",arrDMask[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        }
      }
    } 
    else if (distractor & focus & !dMask & dComplex_focus) {
      cat("\ndistractor X focus X dComplex_focus");
      for (f in 1:length(arrFocus_measurement)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,focus=arrFocus_measurement[f],dMask="",dComplex_focus=arrDComplex_focus[cf])
          for (j in 1:length(arrDistractor)){
            rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
            cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci,"\n -- arrDistractor[j]: ",arrDistractor[j],", arrFocus_measurement[f]: ",arrFocus_measurement[f],", arrDComplex_focus[cf]: ",arrDComplex_focus[cf])
            groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
            groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
            groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
          }
        }
      }
    }
    else if(distractor & focus & !dMask & !dComplex_focus) {
      cat("\ndistractor X focus");
      for (i in 1:length(arrFocus_measurement)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,arrFocus_measurement[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$focus == arrFocus_measurement[i] ] <- rep_high_ci
        }
      }
    } 
    else if(distractor & !focus & !dMask & dComplex_focus){
      cat("\ndistractor X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff,"","",arrDComplex_focus[i])
        for (j in 1:length(arrDistractor)){
          rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
          groupedData_all[[strMean_t0]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
          groupedData_all[[strlow_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
          groupedData_all[[strhigh_ci]][groupedData_all$distractor == arrDistractor[j] & groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
        }
      }
    } # ######## Measurement part
    else if (!scaling & !distractor & !focus & !dMask & !dComplex_focus) {
      cat("\nmeasurment")
      summ_sclAll_diffAx <-make_gensMean_lowCI_highCI_distractorDependent(d,strDiff)
      groupedData_all[[strMean_t0]] <- rep(summ_sclAll_diffAx[1], dim(groupedData_all)[1])
      groupedData_all[[strlow_ci]] <- rep(summ_sclAll_diffAx[2], dim(groupedData_all)[1])
      groupedData_all[[strhigh_ci]] <- rep(summ_sclAll_diffAx[3], dim(groupedData_all)[1])
    } 
    else if (!scaling & !distractor & !focus & dMask & !dComplex_focus) {
      cat("\nmeasurement X dMask");
      for (i in 1:length(arrDMask)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,"",arrDMask[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$dMask == arrDMask[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$dMask == arrDMask[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$dMask == arrDMask[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$dMask == arrDMask[i] ] <- rep_high_ci
        
      }
    } 
    else if (!scaling & !distractor & focus & !dMask & dComplex_focus) {
      cat("\nmeasurement X focus X dComplex_focus");
      for (f in 1:length(arrFocus_measurement)){
        for(cf in 1:length(arrDComplex_focus)){
          summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,arrFocus_measurement[f],"",arrDComplex_focus[cf])
          rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf],])[1])
          cat("\n**** rep_t0: ",rep_t0,"\n*** rep_low_ci: ",rep_low_ci,"\n**** rep_high_ci: ",rep_high_ci)
          groupedData_all[[strMean_t0]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_t0
          groupedData_all[[strlow_ci]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_low_ci
          groupedData_all[[strhigh_ci]][ groupedData_all$focus == arrFocus_measurement[f] & groupedData_all$dComplex_focus == arrDComplex_focus[cf]] <- rep_high_ci
        }
      }
    }
    else if(!scaling & !distractor & focus & !dMask & !dComplex_focus) {
      cat("\nmeasurement X focus");
      for (i in 1:length(arrFocus_measurement)){        
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,arrFocus_measurement[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3], dim(groupedData_all[ groupedData_all$focus == arrFocus_measurement[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$focus == arrFocus_measurement[i] ] <- rep_high_ci
      }
    } 
    else if(!scaling & !distractor & !focus & !dMask & dComplex_focus){
      cat("\nmeasurement X dComplex_focus");
      for (i in 1:length(arrDComplex_focus)){
        summ_sclAll_diffAx <-make_gensMean_lowCI_highCI(d,strDiff,"","",arrDComplex_focus[i])
        rep_t0 <- rep(summ_sclAll_diffAx[1+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        rep_low_ci <- rep(summ_sclAll_diffAx[2+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        rep_high_ci <- rep(summ_sclAll_diffAx[3+(3*(j-1))], dim(groupedData_all[ groupedData_all$dComplex_focus == arrDComplex_focus[i],])[1])
        groupedData_all[[strMean_t0]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_t0
        groupedData_all[[strlow_ci]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_low_ci
        groupedData_all[[strhigh_ci]][ groupedData_all$dComplex_focus == arrDComplex_focus[i] ] <- rep_high_ci
      }
    }
    else {
      cat("\ncategory missing?")
    }
  }
  
  return (groupedData_all);
}

genDF_scaling_correctB_dMask <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_dMask <- d_scl0 %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_dMask["scaling"] <-0
  sumCorrectB_scl1_dMask <- d_scl1 %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_dMask["scaling"] <-1
  sumCorrectB_scl2_dMask <- d_scl2 %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_dMask["scaling"] <-2
  dfCorrectB_scaling_dMask <- rbind(sumCorrectB_scl0_dMask, sumCorrectB_scl1_dMask,sumCorrectB_scl2_dMask)
  return (dfCorrectB_scaling_dMask)
}

genDF_scaling_correctB_focus <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_focus <- d_scl0 %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_focus["scaling"] <-0
  sumCorrectB_scl1_focus <- d_scl1 %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_focus["scaling"] <-1
  sumCorrectB_scl2_focus <- d_scl2 %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_focus["scaling"] <-2
  dfCorrectB_scaling_focus <- rbind(sumCorrectB_scl0_focus, sumCorrectB_scl1_focus,sumCorrectB_scl2_focus)
  return (dfCorrectB_scaling_focus)
}

genDF_scaling_correctB_dComplex_focus <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_dComplex_focus <- d_scl0 %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_dComplex_focus["scaling"] <- 0
  sumCorrectB_scl1_dComplex_focus <- d_scl1 %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_dComplex_focus["scaling"] <- 1
  sumCorrectB_scl2_dComplex_focus <- d_scl2 %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_dComplex_focus["scaling"] <- 2
  dfCorrectB_scaling_dComplex_focus <- rbind(sumCorrectB_scl0_dComplex_focus, sumCorrectB_scl1_dComplex_focus,sumCorrectB_scl2_dComplex_focus)
  return (dfCorrectB_scaling_dComplex_focus)
}

genDF_scaling_correctB_overall <- function (d_scl0,d_scl1,d_scl2){
  sumCorrectB_scl0_overAll <- d_scl0 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl0_overAll["scaling"] <- 0
  sumCorrectB_scl1_overAll <- d_scl1 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl1_overAll["scaling"] <- 1
  sumCorrectB_scl2_overAll <- d_scl2 %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_scl2_overAll["scaling"] <- 2
  dfCorrectB_scaling_overAll <- rbind(sumCorrectB_scl0_overAll, sumCorrectB_scl1_overAll,sumCorrectB_scl2_overAll)
  return (dfCorrectB_scaling_overAll)
}

genDF_scaling_trust <- function (d_scl0,d_scl1,d_scl2){
  sumTrustA1_scl0_overAll <- d_scl0 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl0_overAll["scaling"] <- 0
  sumTrustA1_scl0_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl0_overAll["trustA2"] <- NA; sumTrustA1_scl0_overAll["trustA3"] <- NA; sumTrustA1_scl0_overAll["trustB"] <- NA
  sumTrustA2_scl0_overAll <- d_scl0 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl0_overAll["scaling"] <- 0
  sumTrustA2_scl0_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl0_overAll["trustA1"] <- NA; sumTrustA2_scl0_overAll["trustA3"] <- NA; sumTrustA2_scl0_overAll["trustB"] <- NA
  sumTrustA3_scl0_overAll <- d_scl0 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl0_overAll["scaling"] <- 0
  sumTrustA3_scl0_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl0_overAll["trustA1"] <- NA; sumTrustA3_scl0_overAll["trustA2"] <- NA; sumTrustA3_scl0_overAll["trustB"] <- NA
  sumTrustB_scl0_overAll <- d_scl0 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl0_overAll["scaling"] <- 0
  sumTrustB_scl0_overAll["trustType"] <- "trustB"
  sumTrustB_scl0_overAll["trustA1"] <- NA; sumTrustB_scl0_overAll["trustA2"] <- NA; sumTrustB_scl0_overAll["trustA3"] <- NA
  
  sumTrustA1_scl1_overAll <- d_scl1 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl1_overAll["scaling"] <- 1
  sumTrustA1_scl1_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl1_overAll["trustA2"] <- NA; sumTrustA1_scl1_overAll["trustA3"] <- NA; sumTrustA1_scl1_overAll["trustB"] <- NA
  sumTrustA2_scl1_overAll <- d_scl1 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl1_overAll["scaling"] <- 1
  sumTrustA2_scl1_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl1_overAll["trustA1"] <- NA; sumTrustA2_scl1_overAll["trustA3"] <- NA; sumTrustA2_scl1_overAll["trustB"] <- NA
  sumTrustA3_scl1_overAll <- d_scl1 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl1_overAll["scaling"] <- 1
  sumTrustA3_scl1_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl1_overAll["trustA1"] <- NA; sumTrustA3_scl1_overAll["trustA2"] <- NA; sumTrustA3_scl1_overAll["trustB"] <- NA
  sumTrustB_scl1_overAll <- d_scl1 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl1_overAll["scaling"] <- 1
  sumTrustB_scl1_overAll["trustType"] <- "trustB"
  sumTrustB_scl1_overAll["trustA1"] <- NA; sumTrustB_scl1_overAll["trustA2"] <- NA; sumTrustB_scl1_overAll["trustA3"] <- NA
  
  sumTrustA1_scl2_overAll <- d_scl2 %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_scl2_overAll["scaling"] <- 2
  sumTrustA1_scl2_overAll["trustType"] <- "trustA1"
  sumTrustA1_scl2_overAll["trustA2"] <- NA; sumTrustA1_scl2_overAll["trustA3"] <- NA; sumTrustA1_scl2_overAll["trustB"] <- NA
  sumTrustA2_scl2_overAll <- d_scl2 %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_scl2_overAll["scaling"] <- 2
  sumTrustA2_scl2_overAll["trustType"] <- "trustA2"
  sumTrustA2_scl2_overAll["trustA1"] <- NA; sumTrustA2_scl2_overAll["trustA3"] <- NA; sumTrustA2_scl2_overAll["trustB"] <- NA
  sumTrustA3_scl2_overAll <- d_scl2 %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_scl2_overAll["scaling"] <- 2
  sumTrustA3_scl2_overAll["trustType"] <- "trustA3"
  sumTrustA3_scl2_overAll["trustA1"] <- NA; sumTrustA3_scl2_overAll["trustA2"] <- NA; sumTrustA3_scl2_overAll["trustB"] <- NA
  sumTrustB_scl2_overAll <- d_scl2 %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_scl2_overAll["scaling"] <- 2
  sumTrustB_scl2_overAll["trustType"] <- "trustB"
  sumTrustB_scl2_overAll["trustA1"] <- NA; sumTrustB_scl2_overAll["trustA2"] <- NA; sumTrustB_scl2_overAll["trustA3"] <- NA
  
  dfTrust_scaling_overAll <- rbind(sumTrustA1_scl0_overAll,sumTrustA2_scl0_overAll,sumTrustA3_scl0_overAll,sumTrustB_scl0_overAll, sumTrustA1_scl1_overAll,sumTrustA2_scl1_overAll,sumTrustA3_scl1_overAll,sumTrustB_scl1_overAll, sumTrustA1_scl2_overAll,sumTrustA2_scl2_overAll,sumTrustA3_scl2_overAll,sumTrustB_scl2_overAll)
  return (dfTrust_scaling_overAll)
}

genDF_distractor_correctB_focus <- function (d_h,d_n){
  sumCorrectB_h_focus <- d_h %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_focus["distractor"] <-"h"
  sumCorrectB_n_focus <- d_n %>% 
    group_by(focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_focus["distractor"] <- "n"
  dfCorrectB_distractor_focus <- rbind(sumCorrectB_h_focus, sumCorrectB_n_focus)
  return (dfCorrectB_distractor_focus)
}

genDF_distractor_correctB_dMask <- function (d_h,d_n){
  sumCorrectB_h_dMask <- d_h %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_dMask["distractor"] <-"h"
  sumCorrectB_n_dMask <- d_n %>% 
    group_by(dMask, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_dMask["distractor"] <- "n"
  dfCorrectB_distractor_dMask <- rbind(sumCorrectB_h_dMask, sumCorrectB_n_dMask)
  return (dfCorrectB_distractor_dMask)
}

genDF_distractor_correctB_dComplex_focus <- function (d_h,d_n){
  sumCorrectB_h_dComplex_focus <- d_h %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  sumCorrectB_h_dComplex_focus["distractor"] <- "h"
  sumCorrectB_n_dComplex_focus <- d_n %>% 
    group_by(dComplex_focus, correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_dComplex_focus["distractor"] <- "n"
  dfCorrectB_distractor_dComplex_focus <- rbind(sumCorrectB_h_dComplex_focus, sumCorrectB_n_dComplex_focus)
  return (dfCorrectB_distractor_dComplex_focus)
}

genDF_distractor_correctB_overall <- function (d_h,d_n){
  sumCorrectB_h_overAll <- d_h %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_h_overAll["distractor"] <- "h"
  sumCorrectB_n_overAll <- d_n %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumCorrectB_n_overAll["distractor"] <- "n"
  dfCorrectB_distractor_overAll <- rbind(sumCorrectB_h_overAll, sumCorrectB_n_overAll)
  return (dfCorrectB_distractor_overAll)
}

genDF_distractor_trust <- function (d_h,d_n){
  sumTrustA1_h_overAll <- d_h %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_h_overAll["distractor"] <- "h"
  sumTrustA1_h_overAll["trustType"] <- "trustA1"
  sumTrustA1_h_overAll["trustA2"] <- NA; sumTrustA1_h_overAll["trustA3"] <- NA; sumTrustA1_h_overAll["trustB"] <- NA
  sumTrustA2_h_overAll <- d_h %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_h_overAll["distractor"] <- "h"
  sumTrustA2_h_overAll["trustType"] <- "trustA2"
  sumTrustA2_h_overAll["trustA1"] <- NA; sumTrustA2_h_overAll["trustA3"] <- NA; sumTrustA2_h_overAll["trustB"] <- NA
  sumTrustA3_h_overAll <- d_h %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_h_overAll["distractor"] <- "h"
  sumTrustA3_h_overAll["trustType"] <- "trustA3"
  sumTrustA3_h_overAll["trustA1"] <- NA; sumTrustA3_h_overAll["trustA2"] <- NA; sumTrustA3_h_overAll["trustB"] <- NA
  sumTrustB_h_overAll <- d_h %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_h_overAll["distractor"] <- "h"
  sumTrustB_h_overAll["trustType"] <- "trustB"
  sumTrustB_h_overAll["trustA1"] <- NA; sumTrustB_h_overAll["trustA2"] <- NA; sumTrustB_h_overAll["trustA3"] <- NA
  
  sumTrustA1_n_overAll <- d_n %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_n_overAll["distractor"] <- "n"
  sumTrustA1_n_overAll["trustType"] <- "trustA1"
  sumTrustA1_n_overAll["trustA2"] <- NA; sumTrustA1_n_overAll["trustA3"] <- NA; sumTrustA1_n_overAll["trustB"] <- NA
  sumTrustA2_n_overAll <- d_n %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_n_overAll["distractor"] <- "n"
  sumTrustA2_n_overAll["trustType"] <- "trustA2"
  sumTrustA2_n_overAll["trustA1"] <- NA; sumTrustA2_n_overAll["trustA3"] <- NA; sumTrustA2_n_overAll["trustB"] <- NA
  sumTrustA3_n_overAll <- d_n %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_n_overAll["distractor"] <- "n"
  sumTrustA3_n_overAll["trustType"] <- "trustA3"
  sumTrustA3_n_overAll["trustA1"] <- NA; sumTrustA3_n_overAll["trustA2"] <- NA; sumTrustA3_n_overAll["trustB"] <- NA
  sumTrustB_n_overAll <- d_n %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_n_overAll["distractor"] <- "n"
  sumTrustB_n_overAll["trustType"] <- "trustB"
  sumTrustB_n_overAll["trustA1"] <- NA; sumTrustB_n_overAll["trustA2"] <- NA; sumTrustB_n_overAll["trustA3"] <- NA
  
  dfTrust_distractor_overAll <- rbind(sumTrustA1_h_overAll,sumTrustA2_h_overAll,sumTrustA3_h_overAll,sumTrustB_h_overAll, sumTrustA1_n_overAll,sumTrustA2_n_overAll,sumTrustA3_n_overAll,sumTrustB_n_overAll)
  return (dfTrust_distractor_overAll)
}

genDF_measurement_correctB_dComplex_focus <- function (d){
  sumCorrectB_dComplex_focus <- d %>%
    group_by(dComplex_focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_dComplex_focus <- rbind(sumCorrectB_dComplex_focus)
  return (dfCorrectB_dComplex_focus)
}

genDF_measurement_correctB_dMask <- function (d){
  sumCorrectB_dMask <- d %>%
    group_by(dMask, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_dMask <- rbind(sumCorrectB_dMask)
  return (dfCorrectB_measurement_dMask)
}

genDF_measurement_correctB_focus <- function (d){
  sumCorrectB_focus <- d %>%
    group_by(focus, correctB) %>%
    summarise(count = n()) %>%
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_focus <- rbind(sumCorrectB_focus)
  return (dfCorrectB_measurement_focus)
}

genDF_measurement_correctB_overall <- function (d){
  sumCorrectB_overAll <- d %>% 
    group_by (correctB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  dfCorrectB_measurement_overAll <- rbind(sumCorrectB_overAll)
  return (dfCorrectB_measurement_overAll)
}

genDF_measurement_trust <- function (d){
  sumTrustA1_overAll <- d %>% 
    group_by (trustA1) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA1_overAll["trustType"] <- "trustA1"
  sumTrustA1_overAll["trustA2"] <- NA; sumTrustA1_overAll["trustA3"] <- NA; sumTrustA1_overAll["trustB"] <- NA
  sumTrustA2_overAll <- d %>% 
    group_by (trustA2) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA2_overAll["trustType"] <- "trustA2"
  sumTrustA2_overAll["trustA1"] <- NA; sumTrustA2_overAll["trustA3"] <- NA; sumTrustA2_overAll["trustB"] <- NA
  sumTrustA3_overAll <- d %>% 
    group_by (trustA3) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustA3_overAll["trustType"] <- "trustA3"
  sumTrustA3_overAll["trustA1"] <- NA; sumTrustA3_overAll["trustA2"] <- NA; sumTrustA3_overAll["trustB"] <- NA
  sumTrustB_overAll <- d %>% 
    group_by (trustB) %>% 
    summarise(count = n()) %>% 
    mutate(perc = count/sum(count))
  sumTrustB_overAll["trustType"] <- "trustB"
  sumTrustB_overAll["trustA1"] <- NA; sumTrustB_overAll["trustA2"] <- NA; sumTrustB_overAll["trustA3"] <- NA
  
  dfTrust_measurement_overAll <- rbind(sumTrustA1_overAll,sumTrustA2_overAll,sumTrustA3_overAll,sumTrustB_overAll)
  return (dfTrust_measurement_overAll)
}


pcentFun <- function(x) {
  res <- x > 0
  100 * (sum(res) / length(res))
}

# -------- gen and plot trust

genAndPlotTrust_scaling_overall <- function(d_scl0,d_scl1,d_scl2){
  dfTrust_scaling_overAll <- genDF_scaling_trust(d_scl0,d_scl1,d_scl2)
  plotTrustA1_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  plotTrustA1_scl0_overAll
  plotTrustA1_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA1_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())  
  plotTrustA3_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl0_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==0 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 0 Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl1_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==1 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 1 Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_scl2_overAll <- ggplot(dfTrust_scaling_overAll[dfTrust_scaling_overAll$scaling==2 & dfTrust_scaling_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + 
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Scale 2 Trust B", y = "", fill = "trust") +
    scale_fill_manual("trust", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA1_scl0_overAll + plotTrustA1_scl1_overAll + plotTrustA1_scl2_overAll + plotTrustA2_scl0_overAll + plotTrustA2_scl1_overAll + plotTrustA2_scl2_overAll +
    plotTrustA3_scl0_overAll + plotTrustA3_scl1_overAll + plotTrustA3_scl2_overAll + plotTrustB_scl0_overAll + plotTrustB_scl1_overAll + plotTrustB_scl2_overAll +  
    plot_layout(ncol = 12, widths = c(1, 1))
}

genAndPlot_differencesBoot_scaling <- function (d,d2,d3,question,focus="",dMask="",dComplex_focus="",R=10000, logFunction=FALSE){
  diffsOfBoots_1_2 <- bootQuestionsDifferences_directSubstract(d,d2,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R, logFunction=logFunction)
  diffsOfBoots_1_3 <- bootQuestionsDifferences_directSubstract(d,d3,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R, logFunction=logFunction)
  diffsOfBoots_2_3 <- bootQuestionsDifferences_directSubstract(d2,d3,question=question,focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R, logFunction=logFunction)
  diffsOfBoots_1_2 <- c(diffsOfBoots_1_2, "diff scale 0 and 1")
  diffsOfBoots_1_3 <- c(diffsOfBoots_1_3, "diff scale 0 and 2")
  diffsOfBoots_2_3 <- c(diffsOfBoots_2_3, "diff scale 1 and 2")
  df <- data.frame(diffsOfBoots_1_2,diffsOfBoots_1_3,diffsOfBoots_2_3);
  df <- data.frame(t(df))
  df <- rename(df,mean_CI=X1);df <- rename(df,low_CI=X2);df <- rename(df,high_CI=X3);df <- rename(df,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  df[,cols] <- lapply( df[,cols],as.numeric)
  cat("\ndf$low_CI: ",df$low_CI,", df$high_CI: ",df$high_CI)
  strSentence <- paste("The differences accordin to scaling for the questions: ",question)
  leftEdgeGraph <- min(-0.15, min(df$low_CI) -0.1 ); rightEdgeGraph <- max(0.15,max(df$high_CI)+0.1)
  cat("\nleftEdgeGraph: ",leftEdgeGraph,", rightEdgeGraph: ",rightEdgeGraph)
  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
  cat("\nabsGraphEdge: ",absGraphEdge)
  groupedPlotDiffB <- ggplot(df, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-absGraphEdge,absGraphEdge)) + 
    ggtitle(strSentence)
  
  grid.arrange(groupedPlotDiffB, ncol=1)
}

genAndPlot_errorRate_correctB_scaling <- function (d_scl0, d_scl1, d_scl2){
  boot_scl0 <- boot(d_scl0$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,0)
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_scl1$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,1)
  # 
  boot_scl2 <- boot(d_scl2$correctB,samplemean,10000)
  ci_scl2 <- boot.ci(boot.out = boot_scl2, type = c("norm", "basic", "perc", "bca"));
  mean_scl2 <- boot_scl2$t0;
  l_ci_scl2 <- ci_scl2$normal[2];
  h_ci_scl2 <- ci_scl2$normal[3];
  res_scl2 <- c(mean_scl2,l_ci_scl2,h_ci_scl2)
  res_scl2 <- c(res_scl2,2)
  
  df_errorRate <- data.frame(res_scl0,res_scl1,res_scl2)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,scaling=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}

genAndPlotCorrectB_scaling_mask <- function(d_scl0,d_scl1,d_scl2) {
  dfCorrectB_scaling_dMask <-genDF_scaling_correctB_dMask(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_dMask <- renameGroupedData(dfCorrectB_scaling_dMask)
  plotCorrectB_scl0_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==0,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==1,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dMask <- ggplot(dfCorrectB_scaling_dMask[dfCorrectB_scaling_dMask$scaling==2,], aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_maskDiff_correctB <- grid.arrange(plotCorrectB_scl0_dMask, plotCorrectB_scl1_dMask, plotCorrectB_scl2_dMask,ncol=3,
                                         top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_scaling_dcomplex_focus <- function (d_scl0,d_scl1,d_scl2){
  dfCorrectB_scaling_dComplex_focus <- genDF_scaling_correctB_dComplex_focus(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_dComplex_focus <- renameGroupedData(dfCorrectB_scaling_dComplex_focus)
  plotCorrectB_scl0_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==0,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==1,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dFocus <- ggplot(dfCorrectB_scaling_dComplex_focus[dfCorrectB_scaling_dComplex_focus$scaling==2,], aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_scl0_dFocus, plotCorrectB_scl1_dFocus, plotCorrectB_scl2_dFocus,ncol=3,
                                          top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))
}
genAndPlotTrust_scaling_focus <- function (d_scl0,d_scl1,d_scl2){
  dfCorrectB_scaling_focus <- genDF_scaling_correctB_focus(d_scl0,d_scl1,d_scl2)
  dfCorrectB_scaling_focus <- renameGroupedData(dfCorrectB_scaling_focus)
  plotCorrectB_scl0_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==0,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 0", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl1_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==1,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 1", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  plotCorrectB_scl2_dFocus <- ggplot(dfCorrectB_scaling_focus[dfCorrectB_scaling_focus$scaling==2,], aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Scale 2", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_scl0_dFocus, plotCorrectB_scl1_dFocus, plotCorrectB_scl2_dFocus,ncol=3,
                                          top = textGrob("focus",gp=gpar(fontsize=20,font=3)))
}

genAndPlotTrust_distractor_overall <- function(d_h,d_n){
  # d_h <- subset(d_h, is.numeric(d_h$trustA1) & is.numeric(d_h$trustA2) & is.numeric(d_h$trustA3) & is.numeric(d_h$trustB))
  # d_n <- subset(d_n, is.numeric(d_n$trustA1) & is.numeric(d_n$trustA2) & is.numeric(d_n$trustA3) & is.numeric(d_n$trustB))
  d_h$trustA1 <- as.numeric(d_h$trustA1); d_h$trustA2 <- as.numeric(d_h$trustA2); d_h$trustA3 <- as.numeric(d_h$trustA3); d_h$trustB <- as.numeric(d_h$trustB);
  d_n$trustA1 <- as.numeric(d_n$trustA1); d_n$trustA2 <- as.numeric(d_n$trustA2); d_n$trustA3 <- as.numeric(d_n$trustA3); d_n$trustB <- as.numeric(d_n$trustB);
  d_h <- subset(d_h,(trustA1==1 | trustA1==2 | trustA1==3 | trustA1==4 | trustA1==5) & (trustA2==1 | trustA2==2 | trustA2==3 | trustA2==4 | trustA2==5) & (trustA3==1 | trustA3==2 | trustA3==3 | trustA3==4 | trustA3==5) & (trustB==1 | trustB==2 | trustB==3 | trustB==4 | trustB==5))
  d_n <- subset(d_n,(trustA1==1 | trustA1==2 | trustA1==3 | trustA1==4 | trustA1==5) & (trustA2==1 | trustA2==2 | trustA2==3 | trustA2==4 | trustA2==5) & (trustA3==1 | trustA3==2 | trustA3==3 | trustA3==4 | trustA3==5) & (trustB==1 | trustB==2 | trustB==3 | trustB==4 | trustB==5))
  cat("\ndimensions of d_h: ",toString(dim(d_h)),", and d_n: ",toString(dim(d_n)))
  
  dfTrust_distractor_overAll <- genDF_distractor_trust(d_h,d_n)
  plotTrustA1_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  plotTrustA1_h_overAll
  plotTrustA1_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA2_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA3_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustB_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust B", y = "", fill = "Trust") +
    scale_fill_manual("Trust", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  
  plotTrustA1_h_overAll + plotTrustA1_n_overAll +  plotTrustA2_h_overAll + plotTrustA2_n_overAll + 
    plotTrustA3_h_overAll + plotTrustA3_n_overAll +  plotTrustB_h_overAll + plotTrustB_n_overAll + 
    plot_layout(ncol = 8, widths = c(1, 1))
}
genAndPlot_correctB_measurement_focus <- function(d) {
  dfCorrectB_measurement_focus <-genDF_measurement_correctB_focus(d)
  dfCorrectB_measurement_focus <- renameGroupedData(dfCorrectB_measurement_focus)
  plotCorrectB_focus <- ggplot(dfCorrectB_measurement_focus, aes(x = factor(focus), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectB_focus,  
                                          top = textGrob("focusDiff",gp=gpar(fontsize=20,font=3)))
}

genAndPlot_correctB_measurement_dcomplex_focus <- function (d){
  dfCorrectB_measurement_dComplex_focus <- genDF_measurement_correctB_dComplex_focus(d)
  dfCorrectB_measurement_dComplex_focus <- renameGroupedData(dfCorrectB_measurement_dComplex_focus)
  plotCorrectBa_dFocus <- ggplot(dfCorrectB_measurement_dComplex_focus, aes(x = factor(orderFocusComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  
  grid_focusDiff_correctB <- grid.arrange(plotCorrectBa_dFocus, ncol=1,
                                          top = textGrob("dComplexFocusDiff",gp=gpar(fontsize=20,font=3)))
}


genAndPlotTrust_measurement_dMask <- function(d){
  # d_h <- subset(d_h, is.numeric(d_h$trustA1) & is.numeric(d_h$trustA2) & is.numeric(d_h$trustA3) & is.numeric(d_h$trustB))
  # d_n <- subset(d_n, is.numeric(d_n$trustA1) & is.numeric(d_n$trustA2) & is.numeric(d_n$trustA3) & is.numeric(d_n$trustB))
  d$trustA1 <- as.numeric(d$trustA1); d$trustA2 <- as.numeric(d$trustA2); d$trustA3 <- as.numeric(d$trustA3); d$trustB <- as.numeric(d$trustB);

  d <- subset(d,(trustA1==1 | trustA1==2 | trustA1==3 | trustA1==4 | trustA1==5) & (trustA2==1 | trustA2==2 | trustA2==3 | trustA2==4 | trustA2==5) & (trustA3==1 | trustA3==2 | trustA3==3 | trustA3==4 | trustA3==5) & (trustB==1 | trustB==2 | trustB==3 | trustB==4 | trustB==5))

    cat("\ndimensions of d_h: ",toString(dim(d_h)),", and d_n: ",toString(dim(d_n)))
  
  dfTrust_measurement_dMask <- genDF_measurement_trust_dMask(d)
  plotTrustA1_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A1", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  plotTrustA1_h_overAll
  plotTrustA1_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) + theme(legend.position="none") +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A1", y = "", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA2_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA2_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A2", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA3_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustA3_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust A3", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustB_h_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="h" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Distractor Trust B", y = "", fill = "trustB") +
    scale_fill_manual("trustB", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  plotTrustB_n_overAll <- ggplot(dfTrust_distractor_overAll[dfTrust_distractor_overAll$distractor=="n" & dfTrust_distractor_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB))) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Normal Trust B", y = "", fill = "Trust") +
    scale_fill_manual("Trust", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  
  plotTrustA1_h_overAll + plotTrustA1_n_overAll +  plotTrustA2_h_overAll + plotTrustA2_n_overAll + 
    plotTrustA3_h_overAll + plotTrustA3_n_overAll +  plotTrustB_h_overAll + plotTrustB_n_overAll + 
    plot_layout(ncol = 8, widths = c(1, 1))
}


# -------- gen and plot error rate
# Error rate: computed as the number of incorrect answers per task multiplied by the total number of repetitions. # reverify
genAndPlot_errorRate_correctB_measurement <-function(d, factorScaling=FALSE, factorDistractor=FALSE,factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorVariation="dMask") {
  d <- filter_allTrust0or5_impossibleQualAnswer(d);
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
  if (factorScaling | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  d$reverseB <- abs(d$correctB -1);
  arrQuestions <- c("reverseB");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor: ",numFactor)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  cat("\narrFactor1: ",arrFactor1,", arrFactor2: ",arrFactor2,", arrFactor3: ",arrFactor3,", arrFactor4: ",arrFactor4,"\n\n");
  arrFactorVariations <- c()
  if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  
  dfCI_global <- data.frame();
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
  }
  
  for (i in arrQuestions){
    cat("\nloop questions. i: ",i,", numFactor: ",numFactor,"\n\n")
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("\nHey 5 length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)                  
                } 
                else {
                  # numFactor == 3 # should be fine, a) but testing necessary b) adaptation in cases where there 
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  # TODO consider that there could potentially be only 2 selec, for a factor like distractor!
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("\nHEY 6 length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  }
                  is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI));
                  dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
                  dfTest_CI <- rename(dfTest_CI,low_CI=X2);
                  dfTest_CI <- rename(dfTest_CI,high_CI=X3);
                  dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI[factor1] <- curFactor1; 
                  dfTest_CI[factor2] <- curFactor2; 
                  dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI$question <- i
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              cat("\nPLEASE TELLE ME WE GET HERE")
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              if (length(arrFactorVariations)== 2){
                # cat("length(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI);
              } 
              else {
                # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              }
              is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI));
              dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
              dfTest_CI <- rename(dfTest_CI,low_CI=X2);
              dfTest_CI <- rename(dfTest_CI,high_CI=X3);
              dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI$question <- i
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              # cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            if (length(arrFactorVariations)== 2){
              # cat("length(arrFactorVariations)== 2")
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              dfTest_CI <- data.frame(group1_CI,group2_CI);
            } 
            else {
              # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
              # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
              # cat("\nand added the strings. Might be a typo in all the cases of this code...")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
            }
            is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI));
            dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
            dfTest_CI <- rename(dfTest_CI,low_CI=X2);
            dfTest_CI <- rename(dfTest_CI,high_CI=X3);
            dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
            dfTest_CI[factor1] <- curFactor1; 
            dfTest_CI$question <- i
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
    }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorVariation can be distractor
      dfTest_CI <- NULL
      if (length(arrFactorVariations)== 2){
        cat("\nHEY 7 length(arrFactorVariations)== 2")
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI);
      } 
      else {
        cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
      }
      is.numeric(dfTest_CI$mean_CI[2])
      dfTest_CI <- data.frame(t(dfTest_CI));
      dfTest_CI <- rename(dfTest_CI,mean_CI=X1);
      dfTest_CI <- rename(dfTest_CI,low_CI=X2);
      dfTest_CI <- rename(dfTest_CI,high_CI=X3);
      dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
      # dfTest_CI[factor1] <- curFactor1; # Regular Show... No idea why we had this!
      dfTest_CI$question <- i
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      cat("\ngenerated the data to display, factorVariation-",factorVariation)
    }
  }
  
  cat("\nPASSED MASSIVE LOOP")
  # boot without factoring
  # res_reverse <- NULL; boot_reverse <- boot(d$reverseB,samplemean,10000); ci_reverse <- boot.ci(boot.out = boot_reverse, type = c("norm", "basic", "perc", "bca")); mean_reverse <- boot_reverse$t0; l_ci_reverse <- ci_reverse$normal[2]; h_ci_reverse <- ci_reverse$normal[3]; res_reverse <- c(mean_reverse,l_ci_reverse,h_ci_reverse);
  # df_errorRate <- data.frame(res_reverse); df_errorRate <- data.frame(t(df_errorRate)); df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3); cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
    
  dfCI_global <- renameGroupedData(dfCI_global); cat("\nrenaming done...")
  
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
  } 
  else {
    # no wrapping.
  } 
  cat("\nAbout to try to generate the graphs. what are the category_combination: ", dfCI_global$orderCategoryCombination,", strFormula: ",strFormula)
  groupedErrorRateB <- NULL;
  if (numFactor!=0){  
    groupedErrorRateB <- ggplot(dfCI_global, aes(x=mean_CI,y=orderCategoryCombination)) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(0,1)) + 
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+
      labs(title = 'Error rate question B', y = "" ) 
  groupedErrorRateB # orderedCategoryCombination # category_combination
  grid.arrange(groupedErrorRateB, ncol=1,top=textGrob(paste("Error rate for ",factorVariation,", factored by ",factorArr,sep="")))  
  } 
  else {
    groupedErrorRateB <- ggplot(dfCI_global, aes(x=mean_CI,y=orderCategoryCombination)) +
      geom_vline(xintercept = 0) +
      geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(0,1)) + 
      ggtitle("Error rate question B") + 
      labs(title = 'Error rate question B', y = "" ) 
    groupedErrorRateB # orderedCategoryCombination # category_combination
    grid.arrange(groupedErrorRateB, ncol=1,top=textGrob(paste("Error rate for ",factorVariation,sep="")))  
  }
  
  cat("\nDrawing done")
  return(dfCI_global)
}
# dfCI_errorRate_B_test1 <- genAndPlot_errorRate_correctB_measurement(d_measurement_filtered, factorDMask = TRUE, factorVariation = "focus")
# dfCI_errorRate_B_test2 <- genAndPlot_errorRate_correctB_measurement(d_measurement_filtered, factorVariation = "focus")
# dfCI_errorRate_B_test3 <- genAndPlot_errorRate_correctB_measurement(d_sclFiltered, factorVariation = "scaling")

df_Distribution_correctB_per_idc <- function (d){
  arrFocus <- c("WHAT_Ql","WHAT_Qn","WHERE")
  arrDmask <- c("easy","medium","hard")
  arrDcomplex_focus <- c("E","M","H")
  idcList <- sort(unique(d$idc))
  toFillIdc <- c()
  correctBNumIdc <- c()
  wrongBNumIdc <- c()
  dMaskNumIdc <- c()
  focusNumIdc <- c()
  dComplex_focusNumIdc <- c()
  counterIdcArr <- 1
  for (idc in idcList){
    for (i in 1:length(arrDmask)){
      for (fcs in arrFocus){
        for (dComplex in arrDcomplex_focus){
          curMask <- arrDmask[i]
          numCorrectB <- length(d$correctB[d$correctB==1 & d$idc==idc & 
                                             d$dMask== curMask  & d$focus==fcs & d$dComplex_focus==dComplex] );
          numWrongB <- length(d$correctB[d$correctB==0 & d$idc==idc & 
                                           d$dMask== curMask & d$focus==fcs & d$dComplex_focus==dComplex] );
          toFillIdc[counterIdcArr] <- idc
          correctBNumIdc[counterIdcArr] <- numCorrectB
          wrongBNumIdc[counterIdcArr] <- numWrongB
          dMaskNumIdc[counterIdcArr] <- curMask
          focusNumIdc[counterIdcArr] <- fcs
          dComplex_focusNumIdc[counterIdcArr] <- dComplex
          counterIdcArr <- counterIdcArr + 1
        }
      }
    }
  }
  # cat("\ncounterIdcArr: ",counterIdcArr)
  resDf <- data.frame(idcs = toFillIdc, correctB = correctBNumIdc, wrongB = wrongBNumIdc, dMask= dMaskNumIdc 
                      , focus <- focusNumIdc, dComplex_focus <- dComplex_focusNumIdc )
  return(resDf)
}


# error rate. 
combine_genPlot_ErrorRate_CIandDifferences <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorDifference="dMask", logFunction=FALSE,
                                                        filterNeither=FALSE){

    if (filterNeither){ d <- filter_neitherLikert(d); }
    d$reverseB <- abs(d$correctB -1);
    
    factorVariation <- factorDifference
    arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
    if (factorScaling | factorDifference =="scaling" | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
    arrQuestions <- c("reverseB");
    numGraphs <- length(arrQuestions); 
    groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
    # call the function to get the factors
    factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
    numFactor <- length(factorArr)
    factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
    numFactor <- length(factorArr)
    cat("\n}}}}factorArr: ",toString(factorArr)); cat("\nnumFactor:length(arrFactorVariations)== ",numFactor); cat('\nfactorDifference: ',factorDifference)
    
    arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
    if(numFactor>0){
      if (factor1 == "scaling"){
        arrFactor1 <- arrScalings
      } 
      else if (factor1 == "distractor"){
        arrFactor1 <- arrDistractor
      } 
      else if (factor1 == "focus"){
        arrFactor1 <- arrFocus
      } 
      else if (factor1 == "dMask"){
        arrFactor1 <- arrMask 
      } 
      else if (factor1 == "dComplex_focus"){
        arrFactor1 <- arrDComplex_focus
      } 
      else {
        return ("Error? We have no factor for the display")
      }
      if (numFactor>1){
        if (factor2 == "focus"){
          arrFactor2 <- arrFocus
        } 
        else if (factor2 == "dMask"){
          arrFactor2 <- arrMask 
        } 
        else if (factor2 == "dComplex_focus"){
          arrFactor2 <- arrDComplex_focus
        }
      }
      if (numFactor>2){
        if (factor3 == "focus"){
          arrFactor3 <- arrFocus
        } 
        else if (factor3 == "dMask"){
          arrFactor3 <- arrMask 
        } 
        else if (factor3 == "dComplex_focus"){
          arrFactor3 <- arrDComplex_focus
        }
      }
      if (numFactor>3){
        if (factor4 == "focus"){
          arrFactor4 <- arrFocus
        } 
        else if (factor4 == "dMask"){
          arrFactor4 <- arrMask 
        } 
        else if (factor4 == "dComplex_focus"){
          arrFactor4 <- arrDComplex_focus
        }
      }
    }
    arrFactorVariations <- c()
    if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
    arrFactorDifferences <- c()
    if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}  
    cat("\narrFactorDifferences: ",arrFactorDifferences)
    dfCI_global <- data.frame()
    dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
    dfCI_global_differences <- data.frame()
    dfCI_global_differences$mean_CI[0] <- 0; dfCI_global_differences$low_CI[0] <- 0;dfCI_global_differences$high_CI[0] <- 0;dfCI_global_differences$category_combination[0] <- 0; dfCI_global_differences$question[0] <- 0;
    
    if (numFactor>=1){ 
      if(factor1=="focus"){dfCI_global$focus[0] <- 0}
      if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
      if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
      # 
      if(factor1=="focus"){dfCI_global_differences$focus[0] <- 0}
      if(factor1=="scaling"){dfCI_global_differences$scaling[0] <- 0}
      if(factor1=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
    }
    if (numFactor>=2){ 
      if(factor2=="focus"){dfCI_global$focus[0] <- 0}
      if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
      if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
      # 
      if(factor2=="focus"){dfCI_global_differences$focus[0] <- 0}
      if(factor2=="scaling"){dfCI_global_differences$scaling[0] <- 0}
      if(factor2=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
    }
    if (numFactor>=3){ 
      if(factor3=="focus"){dfCI_global$focus[0] <- 0}
      if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
      if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
      # 
      if(factor3=="focus"){dfCI_global_differences$focus[0] <- 0}
      if(factor3=="scaling"){dfCI_global_differences$scaling[0] <- 0}
      if(factor3=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
    }
    if (numFactor>=4){ 
      if(factor4=="focus"){dfCI_global$focus[0] <- 0}
      if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
      if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
      # 
      if(factor4=="focus"){dfCI_global_differences$focus[0] <- 0}
      if(factor4=="scaling"){dfCI_global_differences$scaling[0] <- 0}
      if(factor4=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
    }
    
    cat("\n about to loop arrQuestions. arrFactor1: ",arrFactor1,", arrFactor2: ",arrFactor2)
    
    # generations of boot according to the number of factors for each question
    for (i in arrQuestions){
      cat("\nloop questions. i: ",i)
      curQuestion <- i;
      if (numFactor>0){
        for (j in arrFactor1){
          curFactor1 <- j
          if (numFactor > 1 ){
            for (k in arrFactor2){
              curFactor2 <- k
              if (numFactor>2){
                for (l in arrFactor3){
                  curFactor3 <- l
                  if (numFactor>3){
                    # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                    dfTest_CI <- NULL;
                    dfTest_CI_differences <- NULL;
                    if (length(arrFactorVariations)== 2){
                      cat("length(arrFactorVariations)== 2")
                      selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                      selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                      selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                      selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]                    
                      
                      group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                      group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                      group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                      group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                      group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                      group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                      
                      dfTest_CI <- data.frame(group1_CI,group2_CI);
                      dfTest_CI_differences <- data.frame(group_differences1_CI);
                    } 
                    else {
                      selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                      selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                      selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                      selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                      selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]
                      selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3],]
                      
                      group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                      group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                      group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                      group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                      group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                      group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                      
                      group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                      group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                      group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                      group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="") 
                      group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                      group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                      
                      dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                      dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                    }
                    # is.numeric(dfTest_CI$mean_CI[2])
                    dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                    dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                    
                    dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                    dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                    
                    dfTest_CI$question <- i
                    dfTest_CI_differences$question <- i
                    
                    cols <- c("mean_CI","low_CI","high_CI");
                    dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                    leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                    rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                    absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                    strSentence <- paste("Confidence intervals, ",curQuestion)
                    dfCI_global <- rbind(dfCI_global, dfTest_CI)
                    
                    dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                    leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                    rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                    absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                    strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                    dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                    cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                  } 
                  else {
                    # numFactor == 3 # should be fine, a) but testing necessary b) adaptation in cases where there 
                    # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                    # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                    # TODO consider that there could potentially be only 2 selec, for a factor like distractor!
                    dfTest_CI <- NULL
                    if (length(arrFactorVariations)== 2){
                      cat("length(arrFactorVariations)== 2")
                      selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                      selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                      selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                      selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                      
                      group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                      group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                      group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                      
                      group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                      group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                      group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                      
                      dfTest_CI <- data.frame(group1_CI,group2_CI);
                      dfTest_CI_differences <- data.frame(group_differences1_CI);
                    } 
                    else {
                      selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                      selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                      selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                      selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                      selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                      selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                      
                      #   THIS IS THE PART THAT DIFFERS!
                      group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                      group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                      group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                      group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                      group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                      group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                      
                      group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="");
                      group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="");
                      group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="");
                      group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                      group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                      group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                      
                      dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                      dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                    }
                    # is.numeric(dfTest_CI$mean_CI[2])
                    dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                    dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                    
                    dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                    dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                    
                    dfTest_CI$question <- i
                    dfTest_CI_differences$question <- i
                    
                    cols <- c("mean_CI","low_CI","high_CI");
                    dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                    leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                    rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                    absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                    strSentence <- paste("Confidence intervals, ",curQuestion)
                    dfCI_global <- rbind(dfCI_global, dfTest_CI)
                    dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                    leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                    rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                    absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                    strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                    dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                    # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                  }
                }
              }
              else {
                # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
                cat("\n !-!-! numFactor==2 ... ");
                # warning: remember that factorVariation can be distractor
                dfTest_CI <- NULL
                dfTest_CI_differences <- NULL
                if (length(arrFactorVariations)== 2){
                  # cat("\n !!! length(arrFactorVariations)== 2")
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]; selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                  # cat("\n~~\tmade selec_differences for selec1 and selec2. dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", curQuestion: ",curQuestion,"\t~~")
                  dataRandom <- c(0.0001, 0.00012, 0.00013, 0.00014, 0.00015, 0.00016, 0.00017, 0.00018)
                  randV <- runif(1, 0.00005, 0.00010)
                  if (length (selec1[curQuestion] <=1)){
                    # cat("\nnhave to add things. length of selec1[curQuestion]: ",length(selec1[curQuestion]),", selec1[curQuestion]: ",toString(selec1[curQuestion]) )
                    newRow <- selec1
                    newRow[curQuestion] <- newRow[curQuestion] +randV
                    d <- rbind(d, newRow)
                    # selec1[curQuestion] <- c(selec1[curQuestion],selec1[curQuestion] +0.00017, selec1[curQuestion] +0.00018 )
                  }
                  if (length (selec2[curQuestion] <=1)){
                    # cat("\nnhave to add things. length of selec2[curQuestion]: ",length(selec2[curQuestion]),", selec2[curQuestion]: ",toString(selec2[curQuestion]) )
                    newRow <- selec2
                    newRow[curQuestion] <- newRow[curQuestion] +randV
                    # cat("\tdim(newRow): ",dim(newRow))
                    d <- rbind(d, newRow)
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    # selec2[curQuestion] <- c(selec2[curQuestion],selec2[curQuestion] +0.00017, selec2[curQuestion] +0.00018 )
                  }
                  # cat("\n++++Post addiition: dim(selec1): ",dim(selec1), ", dim(selec2): ",dim(selec2) )
                  selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]; selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                  # cat("\nselec1[1,1]: ", selec1[1,1], ", selec2[1,1]: ", selec2[1,1])
                  # cat("\n\n-- length selec1: ", length(selec1[curQuestion]), "\n\n-- length selec2: ", length(selec2[curQuestion]))
                  # View(selec1); View(selec2);
                  
                  selec1All0 = TRUE; selec1All1 = TRUE;
                  for (v in selec1[curQuestion]){
                    if (v != 0){
                      selec1All0 <- FALSE
                    } else {
                      selec1All1 <- FALSE
                    }
                  }
                  selec2All0 = TRUE; selec2All1 = TRUE;
                  for (v in selec2[curQuestion]){
                    if (v != 0){
                      selec2All0 <- FALSE
                    } else {
                      selec2All1 <- FALSE
                    }
                  }
                  # cat("\nselec1All0: ",selec1All0,", selec1All1: ",selec1All1,", selec2All0: ",selec2All0,", selec2All1: ",selec2All1)

                  randS1 <- sample(x=dataRandom, size=length(selec1[curQuestion]))
                  randS2 <- sample(x=dataRandom, size=length(selec2[curQuestion]))
                  if (selec1All0){selec1[curQuestion] <- selec1[curQuestion] + randS1  }
                  if (selec1All1){selec1[curQuestion] <- selec1[curQuestion] - randS1}
                  if (selec2All0){selec2[curQuestion] <- selec2[curQuestion] + randS2}
                  if (selec2All1){selec2[curQuestion] <- selec2[curQuestion] - randS2}
                  # cat("\n**selec1[curQuestion]: ",toString(selec1[curQuestion])); cat("\n**selec2[curQuestion]: ",toString(selec2[curQuestion]))
                  
                  group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion); group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                  if (toString(group1_CI) == "1"){ group1_CI <- c(0.999,0.998,1) }
                  if (toString(group2_CI) == "1"){ group2_CI <- c(0.999,0.998,1) }
                  if (toString(group1_CI) == "0"){ group1_CI <- c(0.001,0.002,000) }
                  if (toString(group2_CI) == "0"){ group2_CI <- c(0.001,0.002,000) }
                                    
                  group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                  group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                  # cat("\n{{{ booted group_differences1_CI")
                  if ( is.na( as.numeric(group1_CI[2] ) ) ){ 
                      # cat("\n\n\n\t need to modify group1_CI, because it returned a single value. We will simply reproduce it")
                      group1_CI <- c(group1_CI[1],as.numeric(group1_CI[1]),as.numeric(group1_CI[1]),group1_CI[2],group1_CI[3])
                    }
                  group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                  group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                  dfTest_CI <- data.frame(group1_CI,group2_CI);
                  dfTest_CI_differences <- data.frame(group_differences1_CI);
                } 
                else {
                  cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                  selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                  selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                  selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                  
                  all0 <- TRUE;
                  for (a in selec1$reverseB){
                    if (a != 0){
                      all0 <- FALSE;
                    }
                  }
                  if (all0){
                    selec1$reverseB[1] <- 0.000000001
                  }
                  all0 <- TRUE;
                  for (a in selec2$reverseB){
                    if (a != 0){
                      all0 <- FALSE;
                    }
                  }
                  if (all0){
                    selec2$reverseB[1] <- 0.000000001
                  }
                  all0 <- TRUE;
                  for (a in selec3$reverseB){
                    if (a != 0){
                      all0 <- FALSE;
                    }
                  }
                  if (all0){
                    selec3$reverseB[1] <- 0.000000001
                  }

                  
                  all1 <- TRUE;
                  for (a in selec1$reverseB){
                    if (a != 1){
                      all1 <- FALSE;
                    }
                  }
                  if (all1){
                    selec1$reverseB[1] <- 0.99999999
                  }
                  all1 <- TRUE;
                  for (a in selec2$reverseB){
                    if (a != 1){
                      all1 <- FALSE;
                    }
                  }
                  if (all1){
                    selec2$reverseB[1] <- 0.99999999
                  }
                  all1 <- TRUE;
                  for (a in selec3$reverseB){
                    if (a != 1){
                      all1 <- FALSE;
                    }
                  }
                  if (all1){
                    selec3$reverseB[1] <- 0.99999999
                  }
                  
                  selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                  selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                  selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                  # cat("\ndim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3),", curQuestion: ",curQuestion )
                  # View(selec3)
                  group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                  group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                  group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                  group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                  group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                  group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);

                  if (is.na(group_differences1_CI[2])){
                    fVal<- group_differences1_CI[1]; leftFVal <- fVal - 0.00001; rightFVal <- fVal + 0.00001; 
                    txtV <- group_differences1_CI[4]; txtL <- group_differences1_CI[5]
                    group_differences1_CI <- c(fVal,leftFVal,rightFVal, txtV, txtL)
                  }
                  if (is.na(group_differences2_CI[2])){
                    fVal<- group_differences2_CI[1]; leftFVal <- fVal - 0.00001; rightFVal <- fVal + 0.00001; 
                    txtV <- group_differences2_CI[4]; txtL <- group_differences2_CI[5]
                    group_differences2_CI <- c(fVal,leftFVal,rightFVal, txtV, txtL)
                  }
                  if (is.na(group_differences3_CI[2])){
                    fVal<- group_differences3_CI[1]; leftFVal <- fVal - 0.00001; rightFVal <- fVal + 0.00001; 
                    txtV <- group_differences3_CI[4]; txtL <- group_differences3_CI[5]
                    group_differences3_CI <- c(fVal,leftFVal,rightFVal, txtV, txtL)
                  }
                  # cat("\nheeeeeeeeeeeeeeeeeeeeeeeeeeeee)
                                    
                  group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                  group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                  group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")

                  group_differences1_CI <- group_differences1_CI[!is.na(group_differences1_CI)]
                  group_differences2_CI <- group_differences2_CI[!is.na(group_differences2_CI)]
                  group_differences3_CI <- group_differences3_CI[!is.na(group_differences3_CI)]
                  
                  
                  group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                  # cat("\n##group_differences1_CI: ",toString(group_differences1_CI) )
                  group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                  # cat("\n##group_differences2_CI: ",toString(group_differences2_CI) )
                  group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                  # cat("\n##group_differences3_CI: ",toString(group_differences3_CI) )
                  # cat("\n\t\t\t\t\t\t changed the group_differences_CI")
                  
                  dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                  dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                }
                # cat("\nout...")
                # is.numeric(dfTest_CI$mean_CI[2])
                dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                
                dfTest_CI[factor1] <- curFactor1; 
                dfTest_CI[factor2] <- curFactor2;
                dfTest_CI_differences[factor1] <- curFactor1; 
                dfTest_CI_differences[factor2] <- curFactor2;
                
                dfTest_CI$question <- i
                dfTest_CI_differences$question <- i
                
                cols <- c("mean_CI","low_CI","high_CI");
                dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                strSentence <- paste("Confidence intervals, ",curQuestion)
                dfCI_global <- rbind(dfCI_global, dfTest_CI)
                dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                
                cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
              }
            }
          }
          else {
            if(numFactor==1){
              cat("\ncase with numFactor == 1")
              # numFactor==1
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              dfTest_CI_differences <- NULL
              if (length(arrFactorVariations)== 2){
                cat("\n!!!length(arrFactorVariations)== 2, factor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", factorDifference: ",factorDifference)
                selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
                cat("\ndim selec1: ", toString(dim(selec1)),", dim selec2: ", toString(dim(selec2)))
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
                
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
                dfTest_CI <- data.frame(group1_CI,group2_CI);
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                dfTest_CI_differences <- data.frame(group_differences1_CI); 
              } 
              else {
                cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec_differences3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
                # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
                cat("\n{{{{group1_CI: ",group1_CI,", group2_CI: ",group2_CI,", group3_CI: ",group3_CI,"\t\n")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                cat("\n{{{{group_differences1_CI: ",group_differences1_CI,", group_differences2_CI: ",group_differences2_CI,", group_differences3_CI: ",group_differences3_CI,"\t\n")
                
                # cat("\nand added the strings. Might be a typo in all the cases of this code...")
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
              }
              # is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4); dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
              dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
              cat("\n||||||||",toString(head(dfTest_CI_differences)))
              if (any(grepl('X4', colnames(dfTest_CI_differences)))){
                dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
              }
              dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences$question <- i;
              
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
              
              cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
            }
          }
        }
        
      }
      else {
        # no factoring... so which differences do we display?!
        cat("\ncase with numFactor == 0")
        # numFactor==0
        # warning: remember that factorVariation can be distractor
        dfTest_CI <- NULL
        dfTest_CI_differences <- NULL
        
        if (length(arrFactorVariations)== 2){
          cat("length(arrFactorVariations)== 2, factorVariation: ",factorVariation,", factorDifference: ",factorDifference,", arrFactorVariations[1]: ",arrFactorVariations[1],", arrFactorDifferences[1]: ",arrFactorDifferences[1])
          cat("\nlength selec1: ",length(d[factorDifference]==arrFactorDifferences[1])," length selec2: ",length(d[factorDifference]==arrFactorDifferences[2]))
          selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
          selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
          selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
          selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
          cat("\n__selec1, selec2, selec_differences1 and selec_differences2 made. Also, curQuestion is: ",curQuestion);
          group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
          group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
          cat("\n__group1_CI and group2_CI");
          group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
          cat("\n__bootQuestionsDifferences_conservative passed");
          group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
          group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
          group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
          
          dfTest_CI <- data.frame(group1_CI,group2_CI);
          dfTest_CI_differences <- data.frame(group_differences1_CI);
        } 
        else {
          cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", curQuestion: ",curQuestion)
          selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
          selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
          selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
          selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
          selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
          selec_differences3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
          
          group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
          group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
          group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
          group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
          group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
          group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
          
          group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
          group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
          group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
          group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
          group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
          group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
          
          dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
          dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
        }
        dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);dfTest_CI$question <- i
        # cat("\n****length(dfTest_CI$question): ",length(dfTest_CI$question))
        # cat("\n****length(dfTest_CI_differences$question): ",length(dfTest_CI_differences$question))
        # dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
        dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
        cat("\n||||||||",toString(head(dfTest_CI_differences)))
        if (any(grepl('X4', colnames(dfTest_CI_differences)))){
          dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
        }
        dfTest_CI_differences$question <- i
        
        cols <- c("mean_CI","low_CI","high_CI");
        dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
        leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
        rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
        absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
        strSentence <- paste("Confidence intervals, ",curQuestion)
        dfCI_global <- rbind(dfCI_global, dfTest_CI)
        dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
        leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
        rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
        absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
        strSentence <- paste("Differences of confidence intervals, ",curQuestion)
        dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
        
        cat("\ngenerated the data to display, factorVariation-",factorVariation)
      }
    }
    
    # we should have the dfCI_global loaded now, but still need to display it.
    cat("\n####about to draw")
    cat("\nwhat of the global structure variations... ",dim(dfCI_global),", and their questions: ",length(dfCI_global$question));
    cat("\nwhat of the global structure differences... ",dim(dfCI_global_differences),", and their questions: ",length(dfCI_global_differences$question));
    class(dfCI_global$category_combination)
    class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
    class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
    class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
    class(dfCI_global_differences$category_combination);
    class(dfCI_global_differences$mean_CI); dfCI_global_differences$mean_CI <- as.numeric(dfCI_global_differences$mean_CI); class(dfCI_global_differences$mean_CI);
    class(dfCI_global_differences$low_CI); dfCI_global_differences$low_CI <- as.numeric(dfCI_global_differences$low_CI); class(dfCI_global_differences$low_CI);
    class(dfCI_global_differences$high_CI); dfCI_global_differences$high_CI <- as.numeric(dfCI_global_differences$high_CI); class(dfCI_global_differences$high_CI);
    
    d <- renameGroupedData(d)
    dfCI_global <- renameGroupedData(dfCI_global);
    dfCI_global_differences <- renameGroupedData(dfCI_global_differences);

    cat("\ndid the renaming mess up dimensions of dfCI_global: ",dim(dfCI_global),", and how many items in question? ",length(dfCI_global$question));
    cat("\nand what about dfCI_global_differences: ",dim(dfCI_global_differences),", and how many items in question? ",length(dfCI_global_differences$question));

    if (numFactor ==3 ){
      if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
        dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
        dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
      }
    }
    else if(numFactor ==2) {
      if (factor1=="scaling" | factor2=="scaling"){
        dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
        dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
      }
    }
    else if(numFactor ==1){
      if (factor1=="scaling"){
        dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
        dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
      }
    }
    minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
    minLow_cI_differences <- max(abs(dfCI_global_differences$low_CI));maxHigh_CI_differences <- max(abs(dfCI_global_differences$high_CI)); edgeSize_differences <- max(0.1+abs(minLow_cI_differences),0.1+abs(maxHigh_CI_differences)); # very odd. but should be fine...
    cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize,",minLow_cI_differences: ",minLow_cI_differences,", maxHigh_CI_differences: ",maxHigh_CI_differences,",edgeSize_differences: ",edgeSize_differences)
    minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
    cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
    cat("\n^^^^what's the length of question for dfCI_global now? ",length(dfCI_global$question))
    strFormula <- ""
    if (numFactor==2){
      strFormula<-paste("~",factor1,"+",factor2)
      cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
      strFormula <- str_replace(strFormula,"scaling","orderedScaling"); strFormula <- str_replace(strFormula,"dMask","orderMaskComplex"); strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
      cat("\npost modif strFormula: ",strFormula)
      strFormula_differences<-paste("~",factor1,"+",factor2)
      cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
      strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling"); strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex"); strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
      cat("\npost modif strFormula_differences: ",strFormula_differences)
    } 
    else if (numFactor==1){
      strFormula<-paste("~",factor1)
      cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
      strFormula <- str_replace(strFormula,"scaling","orderedScaling"); strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
      cat("\npost modif strFormula: ",strFormula)
      strFormula_differences<-paste("~",factor1)
      cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
      strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling"); strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
      cat("\npost modif strFormula_differences: ",strFormula_differences)
    }
    else {
      # no wrapping.
      cat("\nno wrapping according to formula")
    } 
    # TODO fix the question missing in dfCI_global when numFactor == 0
    groupedPlotCI_1 <- NULL; groupedPlotCI_2 <- NULL;groupedPlotCI_3<- NULL;
    group_differencesedPlotCI_1<-NULL;group_differencesedPlotCI_2<-NULL;group_differencesedPlotCI_3<-NULL;
    dfCI_global_differences <- addInfoCiDifferenceSignificant(dfCI_global_differences)
    cat("\n:::: dfCI_global colNames: ",colnames(dfCI_global))
    cat("\n:::: dfCI_global_differences colNames: ",colnames(dfCI_global_differences))
    
    if ( ! "orderCategoryCombination" %in% colnames(d)  ){
      cat("\nㄴ--ㄴNeed to fill orderCategoryCombination for d.")
      cat("\n\tcolnames(d): ",colnames(d))
      cat("\n\tcolnames(dfCI_global): ",colnames(dfCI_global))
      d$orderCategoryCombination <- c()
      order_dComplex_focus <- grepl( "focus complexity:" , dfCI_global$orderCategoryCombination[1] )
      order_focus <- grepl( "focus:" , dfCI_global$orderCategoryCombination[1] )
      order_dMask <- grepl( "Mask:" , dfCI_global$orderCategoryCombination[1] )
      order_distractor <- grepl( "distractor:" , dfCI_global$orderCategoryCombination[1] )
      order_scaling <- grepl( "scaling:" , dfCI_global$orderCategoryCombination[1] )
      
      cat("\norder_dComplex_focus: ",order_dComplex_focus,", order_focus: ",order_focus,", order_dMask: ",order_dMask,", order_distractor: ",order_distractor,", order_scaling: ",order_scaling)
      if (order_dComplex_focus){ d$orderCategoryCombination[d$orderFocusComplex=="Focus Easy"] <- "focus complexity: Easy";
                                 d$orderCategoryCombination[d$orderFocusComplex=="Focus Medium"] <- "focus complexity: Medium";
                                 d$orderCategoryCombination[d$orderFocusComplex=="Focus Hard"] <- "focus complexity: Hard"; }
      if (order_distractor){ d$orderCategoryCombination[d$distractor=="h"] <- "distractor: h";
                                 d$orderCategoryCombination[d$distractor=="n"] <- "distractor: n"; }
      if (order_scaling){ d$orderCategoryCombination[d$scaling==0] <- "scaling: 0";
                          d$orderCategoryCombination[d$scaling==1] <- "scaling: 1";
                          d$orderCategoryCombination[d$scaling==2] <- "scaling: 2"; }
      if (order_focus){ d$orderCategoryCombination[d$focus=="WHAT_Ql"] <- "focus: WHAT_Ql";
                        d$orderCategoryCombination[d$focus=="WHAT_Qn"] <- "focus: WHAT_Qn";
                        d$orderCategoryCombination[d$focus=="WHERE"] <- "focus: WHERE"; }
      if (order_dMask){ d$orderCategoryCombination[d$dMask=="easy"] <- "Mask: Easy";
                        d$orderCategoryCombination[d$dMask=="medium"] <- "Mask: Medium";
                        d$orderCategoryCombination[d$dMask=="hard"] <- "Mask: Hard"; } 
    }
    
    d <- orderData(d)
    # we end up with d not having orderCategoryCombination...
    
    strTitleTotal <- NULL;
    if (numFactor!=0){ 
      strTitleTotal <- paste("Error rates and differences for ",factorDifference,", factored by ",toString(factorArr),sep="")
      groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
        geom_vline(xintercept = 0) +
        geom_violin( data = d,  aes (x=  abs(1 - correctB)  , y = orderCategoryCombination), show.legend = FALSE, alpha = 0.3 ) +
        geom_errorbarh(aes(xmin=low_CI, xmax= high_CI, height= .5 )) +
        geom_point(size=3,col="black",fill="white", shape=1) +
        xlim(c(  min(0, min(dfCI_global$low_CI) ) , max(1, max(dfCI_global$high_CI)  ) ) ) + 
        ggtitle("Error Rate") +
        facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+ 
        labs(title = 'Error Rate', y = "" ) +
        theme(
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        )
      # 
      groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
        geom_vline(xintercept = 0) +
        geom_errorbarh(aes(xmin=low_CI, xmax= high_CI, height= .5 )) +
        geom_point(size=2,col="black",fill="white", shape=1) +
        geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
        scale_size_manual(values=c(0.1,5)) +
        xlim(c(-edgeSize,edgeSize)) +
        ggtitle("Differences for Error Rate") +
        labs(title = 'Differences for Error Rate', y = "" ) +
        theme(
          # strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          legend.position="none"
        ) +
        facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") 
        
    } 
    else {
      cat("\n))))numFactor==0. dim(dfCI_global):  ",dim(dfCI_global) )
      strTitleTotal <- paste("Error rates and differences for ",factorDifference,sep="")
      groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
        geom_vline(xintercept = 0) +
        geom_violin( data = d,  aes (x=  abs(1 - correctB)  , y = orderCategoryCombination), show.legend = FALSE, alpha = 0.3 ) +
        geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
        geom_point(size=3,col="black",fill="white", shape=1) +
        xlim(c(0,1)) + 
        ggtitle("Error Rate")+
        theme(
          strip.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          legend.position="none"
        ) +
        labs(title = "Error Rate",y="")
      # 
      groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
        geom_vline(xintercept = 0) +
        geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
        geom_point(size=2,col="black",fill="white", shape=1) +
        geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
        scale_size_manual(values=c(0.1,5)) +
        xlim(c(-edgeSize,edgeSize)) +
        ggtitle("Differences for Error Rate") +
        labs(title = 'Differences for Error Rate', y = "" ) +
        theme(
          strip.text.x = element_blank(),
          axis.ticks.y = element_blank(), axis.text.y = element_blank(),
          legend.position="none"
        )
    }
    cat("\nThe plots are generated. But are they fine?\n")
    grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_differences1), ncol=2,top=textGrob( strTitleTotal ) )
    # cat("not sure what to return")
    return (dfCI_global)
  }

# error rate. 
combine_genPlot_ErrorRate_CIandDifferences_old <- function (d,factorScaling=FALSE,factorDistractor=FALSE, factorFocus=FALSE, factorDMask= FALSE, factorDComplex_focus=FALSE, factorDifference="dMask", logFunction=FALSE){
  # d <- filter_allTrust0or5_impossibleQualAnswer(d)
  d$reverseB <- abs(d$correctB -1);
  
  factorVariation <- factorDifference
  arrScalings <- c(0,1,2); arrDistractor <- c("h","n"); arrFocus <- c("WHAT_Qn","WHAT_Ql","WHERE"); arrMask <- c("easy","medium","hard"); arrDComplex_focus <- c("E","M","H");  
  if (factorScaling | factorDifference =="scaling" | factorVariation=="scaling"){arrFocus <- c("WHAT_Qn","WHAT_Ql")}  
  arrQuestions <- c("reverseB");
  numGraphs <- length(arrQuestions); 
  groupedPlotCI_1 <- NULL;groupedPlotCI_2 <- NULL;groupedPlotCI_3 <- NULL;
  # call the function to get the factors
  factorArr <- returnFactorsCombination(factorScaling=factorScaling,factorDistractor=factorDistractor,factorFocus=factorFocus,factorDMask=factorDMask,factorDComplex_focus=factorDComplex_focus);
  numFactor <- length(factorArr)
  factor1 <- factorArr[1]; factor2 <- factorArr[2]; factor3 <- factorArr[3]; factor4 <- factorArr[4]
  numFactor <- length(factorArr)
  cat("\n}}}}factorArr: ",toString(factorArr))
  cat("\nnumFactor:length(arrFactorVariations)== ",numFactor)
  cat('\nfactorDifference: ',factorDifference)
  
  arrFactor1 <- NULL; arrFactor2 <- NULL; arrFactor3 <- NULL; arrFactor4 <- NULL;
  if(numFactor>0){
    if (factor1 == "scaling"){
      arrFactor1 <- arrScalings
    } 
    else if (factor1 == "distractor"){
      arrFactor1 <- arrDistractor
    } 
    else if (factor1 == "focus"){
      arrFactor1 <- arrFocus
    } 
    else if (factor1 == "dMask"){
      arrFactor1 <- arrMask 
    } 
    else if (factor1 == "dComplex_focus"){
      arrFactor1 <- arrDComplex_focus
    } 
    else {
      return ("Error? We have no factor for the display")
    }
    if (numFactor>1){
      if (factor2 == "focus"){
        arrFactor2 <- arrFocus
      } 
      else if (factor2 == "dMask"){
        arrFactor2 <- arrMask 
      } 
      else if (factor2 == "dComplex_focus"){
        arrFactor2 <- arrDComplex_focus
      }
    }
    if (numFactor>2){
      if (factor3 == "focus"){
        arrFactor3 <- arrFocus
      } 
      else if (factor3 == "dMask"){
        arrFactor3 <- arrMask 
      } 
      else if (factor3 == "dComplex_focus"){
        arrFactor3 <- arrDComplex_focus
      }
    }
    if (numFactor>3){
      if (factor4 == "focus"){
        arrFactor4 <- arrFocus
      } 
      else if (factor4 == "dMask"){
        arrFactor4 <- arrMask 
      } 
      else if (factor4 == "dComplex_focus"){
        arrFactor4 <- arrDComplex_focus
      }
    }
  }
  arrFactorVariations <- c()
  if(factorVariation == "focus"){arrFactorVariations <- arrFocus} else if (factorVariation=="dMask"){arrFactorVariations <- arrMask} else if (factorVariation=="dComplex_focus"){arrFactorVariations <- arrDComplex_focus} else if (factorVariation=="scaling"){arrFactorVariations <- arrScalings} else if (factorVariation=="distractor"){arrFactorVariations <- arrDistractor}
  arrFactorDifferences <- c()
  if(factorDifference == "focus"){arrFactorDifferences <- arrFocus} else if (factorDifference=="dMask"){arrFactorDifferences <- arrMask} else if (factorDifference=="dComplex_focus"){arrFactorDifferences <- arrDComplex_focus} else if (factorDifference=="scaling"){arrFactorDifferences <- arrScalings} else if (factorDifference=="distractor"){arrFactorDifferences <- arrDistractor}  
  cat("\narrFactorDifferences: ",arrFactorDifferences)
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;
  dfCI_global_differences <- data.frame()
  dfCI_global_differences$mean_CI[0] <- 0; dfCI_global_differences$low_CI[0] <- 0;dfCI_global_differences$high_CI[0] <- 0;dfCI_global_differences$category_combination[0] <- 0; dfCI_global_differences$question[0] <- 0;
  
  if (numFactor>=1){ 
    if(factor1=="focus"){dfCI_global$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor1=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor1=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor1=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=2){ 
    if(factor2=="focus"){dfCI_global$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor2=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor2=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor2=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=3){ 
    if(factor3=="focus"){dfCI_global$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor3=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor3=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor3=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  if (numFactor>=4){ 
    if(factor4=="focus"){dfCI_global$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global$dComplex_focus[0] <- 0}
    # 
    if(factor4=="focus"){dfCI_global_differences$focus[0] <- 0}
    if(factor4=="scaling"){dfCI_global_differences$scaling[0] <- 0}
    if(factor4=="dComplex_focus"){dfCI_global_differences$dComplex_focus[0] <- 0}
  }
  
  cat("\n about to loop arrQuestions. arrFactor1: ",arrFactor1,", arrFactor2: ",arrFactor2)
  # generations of boot according to the number of factors for each question
  for (i in arrQuestions){
    cat("\nloop questions. i: ",i)
    curQuestion <- i;
    if (numFactor>0){
      for (j in arrFactor1){
        curFactor1 <- j
        if (numFactor > 1 ){
          for (k in arrFactor2){
            curFactor2 <- k
            if (numFactor>2){
              for (l in arrFactor3){
                curFactor3 <- l
                if (numFactor>3){
                  # numFactor == 4 This case is unlikely to be displayed due to lack of data with surprisingly poor quality in the answers from Prolific's participants.
                  dfTest_CI <- NULL;
                  dfTest_CI_differences <- NULL;
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]                    
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1],]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2],]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3],]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="") 
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                  cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                } 
                else {
                  # numFactor == 3 # should be fine, a) but testing necessary b) adaptation in cases where there 
                  # ... Consider that this means that the actual factor that varies would be the 4th factor?! # But there are empty cases...?! Need to sleep on it
                  # factor4 <- "dComplex_focus"; arrFactor4 <- c("E","M","H")
                  # TODO consider that there could potentially be only 2 selec, for a factor like distractor!
                  dfTest_CI <- NULL
                  if (length(arrFactorVariations)== 2){
                    cat("length(arrFactorVariations)== 2")
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI);
                  } 
                  else {
                    selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[1] ,]
                    selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[2] ,]
                    selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorVariation]==arrFactorVariations[3] ,]
                    selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[1] ,]
                    selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[2] ,]
                    selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factor3]==curFactor3 & d[factorDifference]==arrFactorDifferences[3] ,]
                    
                    #   THIS IS THE PART THAT DIFFERS!
                    group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                    group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                    group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                    group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                    group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                    
                    group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="");
                    group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="");
                    group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="");
                    group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                    group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                    group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                    
                    dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                    dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
                  }
                  # is.numeric(dfTest_CI$mean_CI[2])
                  dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
                  dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
                  
                  dfTest_CI[factor1] <- curFactor1; dfTest_CI[factor2] <- curFactor2; dfTest_CI[factor3] <- curFactor3;
                  dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences[factor2] <- curFactor2; dfTest_CI_differences[factor3] <- curFactor3;
                  
                  dfTest_CI$question <- i
                  dfTest_CI_differences$question <- i
                  
                  cols <- c("mean_CI","low_CI","high_CI");
                  dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Confidence intervals, ",curQuestion)
                  dfCI_global <- rbind(dfCI_global, dfTest_CI)
                  dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
                  leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
                  rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
                  absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
                  strSentence <- paste("Differences of confidence intervals, ",curQuestion)
                  dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
                  # cat("\ngenerated the data to display, factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2,", factor3-",factor3,": ",curFactor3)
                }
              }
            }
            else {
              # Most likely the case that will happen the most, since we don't have all cases of dComplex_focus medium... 
              cat("\n !!! numFactor==2 ");
              # warning: remember that factorVariation can be distractor
              dfTest_CI <- NULL
              dfTest_CI_differences <- NULL
              if (length(arrFactorVariations)== 2){
                cat("\n !!! length(arrFactorVariations)== 2")
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                cat("\nmade selec_differences for selec1 and selec2")
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                cat("\ngroup1_CI: ",toString(group1_CI),", group2_CI: ",toString(group2_CI))
                
                if (toString(group1_CI) == "1" ){
                  cat("\n very wrong group1_CI but we try to set a small variation that won't be visible")
                  group1_CI <- c(0.999,0.998,1)
                }
                if (toString(group2_CI) == "1") {
                  cat("\n very wrong group2_CI but we try to set a small variation that won't be visible")
                  group2_CI <- c(0.999,0.998,1)
                }
                
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                cat("\n{{{ booted group_differences1_CI")
                if ( is.na( as.numeric(group1_CI[2] ) ) ){ 
                  cat("\n\n\n\t need to modify group1_CI, because it returned a single value. We will simply reproduce it")
                  group1_CI <- c(group1_CI[1],as.numeric(group1_CI[1]),as.numeric(group1_CI[1]),group1_CI[2],group1_CI[3])
                }
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                dfTest_CI <- data.frame(group1_CI,group2_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI);
              } 
              else {
                # cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1," factor2: ",factor2,", curFactor2: ",curFactor2,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
                selec1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[1] ,]
                selec2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[2] ,]
                selec3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorVariation]==arrFactorVariations[3] ,]
                selec_differences1 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[1] ,]
                selec_differences2 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[2] ,]
                selec_differences3 <- d[d[factor1]==curFactor1 & d[factor2]==curFactor2 & d[factorDifference]==arrFactorDifferences[3] ,]
                
                group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
                group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
                group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
                group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
                group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
                
                group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
                group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
                group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
                group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
                group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
                group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
                
                dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
                dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
              }
              # is.numeric(dfTest_CI$mean_CI[2])
              dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);
              dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4);
              
              dfTest_CI[factor1] <- curFactor1; 
              dfTest_CI[factor2] <- curFactor2;
              dfTest_CI_differences[factor1] <- curFactor1; 
              dfTest_CI_differences[factor2] <- curFactor2;
              
              dfTest_CI$question <- i
              dfTest_CI_differences$question <- i
              
              cols <- c("mean_CI","low_CI","high_CI");
              dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Confidence intervals, ",curQuestion)
              dfCI_global <- rbind(dfCI_global, dfTest_CI)
              dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
              leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
              rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
              absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
              strSentence <- paste("Differences of confidence intervals, ",curQuestion)
              dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
              
              # cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1,", factor2-",factor2,": ",curFactor2)
            }
          }
        }
        else {
          if(numFactor==1){
            cat("\ncase with numFactor == 1")
            # numFactor==1
            # warning: remember that factorVariation can be distractor
            dfTest_CI <- NULL
            dfTest_CI_differences <- NULL
            if (length(arrFactorVariations)== 2){
              cat("\n!!!length(arrFactorVariations)== 2, factor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", factorDifference: ",factorDifference)
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              cat("\ndim selec1: ", toString(dim(selec1)),", dim selec2: ", toString(dim(selec2)))
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
              
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              dfTest_CI <- data.frame(group1_CI,group2_CI);
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              dfTest_CI_differences <- data.frame(group_differences1_CI); 
            } 
            else {
              cat("\nfactor1: ",factor1,", curFactor1: ",curFactor1,", factorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations))
              selec1 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[1] ,]
              selec2 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[2] ,]
              selec3 <- d[d[factor1]==curFactor1 & d[factorVariation]==arrFactorVariations[3] ,]
              selec_differences1 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[1] ,]
              selec_differences2 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[2] ,]
              selec_differences3 <- d[d[factor1]==curFactor1 & d[factorDifference]==arrFactorDifferences[3] ,]
              # cat("\n dim(selec1): ",dim(selec1),", dim(selec2): ",dim(selec2),", dim(selec3): ",dim(selec3))
              group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
              group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
              group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
              group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
              group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
              group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
              # cat("\n dim(group1_CI): ",dim(group1_CI),", dim(group2_CI): ",dim(group2_CI),", dim(group3_CI): ",dim(group3_CI))
              group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1] ,sep="") )
              group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2] ,sep="") )
              group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3] ,sep="") )
              cat("\n{{{{group1_CI: ",group1_CI,", group2_CI: ",group2_CI,", group3_CI: ",group3_CI,"\t\n")
              group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
              group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
              group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
              cat("\n{{{{group_differences1_CI: ",group_differences1_CI,", group_differences2_CI: ",group_differences2_CI,", group_differences3_CI: ",group_differences3_CI,"\t\n")
              
              # cat("\nand added the strings. Might be a typo in all the cases of this code...")
              dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
              dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
            }
            # is.numeric(dfTest_CI$mean_CI[2])
            dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4); dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
            dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
            cat("\n||||||||",toString(head(dfTest_CI_differences)))
            if (any(grepl('X4', colnames(dfTest_CI_differences)))){
              dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
            }
            dfTest_CI_differences[factor1] <- curFactor1; dfTest_CI_differences$question <- i;
            
            cols <- c("mean_CI","low_CI","high_CI");
            dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Confidence intervals, ",curQuestion)
            dfCI_global <- rbind(dfCI_global, dfTest_CI)
            dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
            leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
            rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
            absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
            strSentence <- paste("Differences of confidence intervals, ",curQuestion)
            dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
            
            cat("\ngenerated the data to display, factorVariation-",factorVariation,", factor1-",factor1,": ",curFactor1)
          }
        }
      }
      
    }
    else {
      # no factoring... so which differences do we display?!
      cat("\ncase with numFactor == 0")
      # numFactor==0
      # warning: remember that factorVariation can be distractor
      dfTest_CI <- NULL
      dfTest_CI_differences <- NULL
      
      if (length(arrFactorVariations)== 2){
        cat("length(arrFactorVariations)== 2, factorVariation: ",factorVariation,", factorDifference: ",factorDifference,", arrFactorVariations[1]: ",arrFactorVariations[1],", arrFactorDifferences[1]: ",arrFactorDifferences[1])
        cat("\nlength selec1: ",length(d[factorDifference]==arrFactorDifferences[1])," length selec2: ",length(d[factorDifference]==arrFactorDifferences[2]))
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        cat("\n__selec1, selec2, selec_differences1 and selec_differences2 made. Also, curQuestion is: ",curQuestion);
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        cat("\n__group1_CI and group2_CI");
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
        cat("\n__bootQuestionsDifferences_conservative passed");
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI);
      } 
      else {
        cat("\nfactorVariation: ",factorVariation,", arrFactorVariations: ",toString(arrFactorVariations),", curQuestion: ",curQuestion)
        selec1 <- d[d[factorVariation]==arrFactorVariations[1] ,]
        selec2 <- d[d[factorVariation]==arrFactorVariations[2] ,]
        selec3 <- d[d[factorVariation]==arrFactorVariations[3] ,]
        selec_differences1 <- d[d[factorDifference]==arrFactorDifferences[1] ,]
        selec_differences2 <- d[d[factorDifference]==arrFactorDifferences[2] ,]
        selec_differences3 <- d[d[factorDifference]==arrFactorDifferences[3] ,]
        
        group1_CI<- make_gensMean_lowCI_highCI(d=selec1,question=curQuestion);
        group2_CI<- make_gensMean_lowCI_highCI(d=selec2,question=curQuestion);
        group3_CI<- make_gensMean_lowCI_highCI(d=selec3,question=curQuestion);
        group_differences1_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2= selec_differences2,question=curQuestion, logFunction=logFunction);
        group_differences2_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences1, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
        group_differences3_CI<- bootQuestionsDifferences_directSubstract(d=selec_differences2, d2=selec_differences3,question=curQuestion, logFunction=logFunction);
        
        group1_CI <- c(group1_CI, paste(factorVariation,",",arrFactorVariations[1]),sep="")
        group2_CI <- c(group2_CI, paste(factorVariation,",",arrFactorVariations[2]),sep="")
        group3_CI <- c(group3_CI, paste(factorVariation,",",arrFactorVariations[3]),sep="")
        group_differences1_CI <- c(group_differences1_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[2]),sep="")
        group_differences2_CI <- c(group_differences2_CI, paste(factorDifference,",",arrFactorDifferences[1],"_",arrFactorDifferences[3]),sep="")
        group_differences3_CI <- c(group_differences3_CI, paste(factorDifference,",",arrFactorDifferences[2],"_",arrFactorDifferences[3]),sep="")
        
        dfTest_CI <- data.frame(group1_CI,group2_CI,group3_CI);
        dfTest_CI_differences <- data.frame(group_differences1_CI,group_differences2_CI,group_differences3_CI);
      }
      dfTest_CI <- data.frame(t(dfTest_CI)); dfTest_CI <- rename(dfTest_CI,mean_CI=X1); dfTest_CI <- rename(dfTest_CI,low_CI=X2); dfTest_CI <- rename(dfTest_CI,high_CI=X3); dfTest_CI <- rename(dfTest_CI,"category_combination"=X4);dfTest_CI$question <- i
      # cat("\n****length(dfTest_CI$question): ",length(dfTest_CI$question))
      # cat("\n****length(dfTest_CI_differences$question): ",length(dfTest_CI_differences$question))
      # dfTest_CI[factor1] <- curFactor1; dfTest_CI$question <- i
      dfTest_CI_differences <- data.frame(t(dfTest_CI_differences)); dfTest_CI_differences <- rename(dfTest_CI_differences,mean_CI=X1); dfTest_CI_differences <- rename(dfTest_CI_differences,low_CI=X2); dfTest_CI_differences <- rename(dfTest_CI_differences,high_CI=X3); 
      cat("\n||||||||",toString(head(dfTest_CI_differences)))
      if (any(grepl('X4', colnames(dfTest_CI_differences)))){
        dfTest_CI_differences <- rename(dfTest_CI_differences,"category_combination"=X4); # should this one exist... if so check it?!
      }
      dfTest_CI_differences$question <- i
      
      cols <- c("mean_CI","low_CI","high_CI");
      dfTest_CI[,cols] <- lapply( dfTest_CI[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Confidence intervals, ",curQuestion)
      dfCI_global <- rbind(dfCI_global, dfTest_CI)
      dfTest_CI_differences[,cols] <- lapply( dfTest_CI_differences[,cols],as.numeric)
      leftEdgeGraph <- min(-0.15, min(dfTest_CI_differences$low_CI) -0.1 ); 
      rightEdgeGraph <- max(0.15,max(dfTest_CI_differences$high_CI)+0.1)
      absGraphEdge <- max( abs(leftEdgeGraph),abs(rightEdgeGraph) )
      strSentence <- paste("Differences of confidence intervals, ",curQuestion)
      dfCI_global_differences <- rbind(dfCI_global_differences, dfTest_CI_differences)
      
      cat("\ngenerated the data to display, factorVariation-",factorVariation)
    }
  }
  
  # we should have the dfCI_global loaded now, but still need to display it.
  cat("\n####about to draw")
  cat("\nwhat of the global structure variations... ",dim(dfCI_global),", and their questions: ",length(dfCI_global$question));
  cat("\nwhat of the global structure differences... ",dim(dfCI_global_differences),", and their questions: ",length(dfCI_global_differences$question));
  
  class(dfCI_global$category_combination)
  class(dfCI_global$mean_CI); dfCI_global$mean_CI <- as.numeric(dfCI_global$mean_CI); class(dfCI_global$mean_CI);
  class(dfCI_global$low_CI); dfCI_global$low_CI <- as.numeric(dfCI_global$low_CI); class(dfCI_global$low_CI);
  class(dfCI_global$high_CI); dfCI_global$high_CI <- as.numeric(dfCI_global$high_CI); class(dfCI_global$high_CI);
  class(dfCI_global_differences$category_combination);
  class(dfCI_global_differences$mean_CI); dfCI_global_differences$mean_CI <- as.numeric(dfCI_global_differences$mean_CI); class(dfCI_global_differences$mean_CI);
  class(dfCI_global_differences$low_CI); dfCI_global_differences$low_CI <- as.numeric(dfCI_global_differences$low_CI); class(dfCI_global_differences$low_CI);
  class(dfCI_global_differences$high_CI); dfCI_global_differences$high_CI <- as.numeric(dfCI_global_differences$high_CI); class(dfCI_global_differences$high_CI);
  
  dfCI_global <- renameGroupedData(dfCI_global);
  dfCI_global_differences <- renameGroupedData(dfCI_global_differences);
  
  cat("\ndid the renaming mess up dimensions of dfCI_global: ",dim(dfCI_global),", and how many items in question? ",length(dfCI_global$question));
  cat("\nand what about dfCI_global_differences: ",dim(dfCI_global_differences),", and how many items in question? ",length(dfCI_global_differences$question));
  
  if (numFactor ==3 ){
    if (factor1=="scaling" | factor2=="scaling" | factor3=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==2) {
    if (factor1=="scaling" | factor2=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  else if(numFactor ==1){
    if (factor1=="scaling"){
      dfCI_global$scaling[dfCI_global$scaling==0] <- "Scaling 0";dfCI_global$scaling[dfCI_global$scaling==1] <- "Scaling 1";dfCI_global$scaling[dfCI_global$scaling==2] <- "Scaling 2";
      dfCI_global_differences$scaling[dfCI_global_differences$scaling==0] <- "Scaling 0";dfCI_global_differences$scaling[dfCI_global_differences$scaling==1] <- "Scaling 1";dfCI_global_differences$scaling[dfCI_global_differences$scaling==2] <- "Scaling 2";
    }
  } 
  
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  minLow_cI_differences <- max(abs(dfCI_global_differences$low_CI));maxHigh_CI_differences <- max(abs(dfCI_global_differences$high_CI)); edgeSize_differences <- max(0.1+abs(minLow_cI_differences),0.1+abs(maxHigh_CI_differences)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize,",minLow_cI_differences: ",minLow_cI_differences,", maxHigh_CI_differences: ",maxHigh_CI_differences,",edgeSize_differences: ",edgeSize_differences)
  
  # cat("\n no complaints about scaling as a factor?")
  minLow_cI <- max(abs(dfCI_global$low_CI));maxHigh_CI <- max(abs(dfCI_global$high_CI)); edgeSize <- max(0.1+abs(minLow_cI),0.1+abs(maxHigh_CI)); # very odd. but should be fine...
  cat("\n====The vals of minLow_cI: ",minLow_cI,", maxHigh_CI: ",maxHigh_CI,", edgeSize: ",edgeSize)
  cat("\n^^^^what's the length of question for dfCI_global now? ",length(dfCI_global$question))
  strFormula <- ""
  if (numFactor==2){
    strFormula<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    strFormula <- str_replace(strFormula,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1,"+",factor2)
    cat("\nnumFactor==2. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    strFormula_differences <- str_replace(strFormula_differences,"dComplex_focus","orderFocusComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
  } 
  else if (numFactor==1){
    strFormula<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula: ",strFormula,"... what about dfCI_global: ",toString(dfCI_global[1,]))
    strFormula <- str_replace(strFormula,"scaling","orderedScaling")
    strFormula <- str_replace(strFormula,"dMask","orderMaskComplex")
    cat("\npost modif strFormula: ",strFormula)
    strFormula_differences<-paste("~",factor1)
    cat("\nnumFactor==1. strFormula_differences: ",strFormula_differences,"... what about dfCI_global_differences: ",toString(dfCI_global_differences[1,]))
    strFormula_differences <- str_replace(strFormula_differences,"scaling","orderedScaling")
    strFormula_differences <- str_replace(strFormula_differences,"dMask","orderMaskComplex")
    cat("\npost modif strFormula_differences: ",strFormula_differences)
    
  }
  else {
    # no wrapping.
    cat("\nno wrapping according to formula")
  } 
  # TODO fix the question missing in dfCI_global when numFactor == 0
  groupedPlotCI_1 <- NULL; groupedPlotCI_2 <- NULL;groupedPlotCI_3<- NULL;
  group_differencesedPlotCI_1<-NULL;group_differencesedPlotCI_2<-NULL;group_differencesedPlotCI_3<-NULL;
  dfCI_global_differences <- addInfoCiDifferenceSignificant(dfCI_global_differences)
  cat("\n:::: dfCI_global colNames: ",colnames(dfCI_global))
  cat("\n:::: dfCI_global_differences colNames: ",colnames(dfCI_global_differences))
  
  strTitleTotal <- NULL;
  if (numFactor!=0){ 
    strTitleTotal <- paste("Error rates and differences for ",factorDifference,", factored by ",toString(factorArr),sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax= high_CI )) + #seriously concerned with this
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(  min(0, min(dfCI_global$low_CI) ) , max(1, max(dfCI_global$high_CI)  ) ) ) + 
      ggtitle("Error Rate") +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1)+ 
      labs(title = 'Error Rate', y = "" ) +
      theme(
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
      )
    # 
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax= high_CI )) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Error Rate") +
      labs(title = 'Differences for Error Rate', y = "" ) +
      theme(
        # strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      ) +
      facet_wrap( as.formula(strFormula) , dir="v", ncol=1 , strip.position = "right") 
    
  } 
  else {
    cat("\n))))numFactor==0. dim(dfCI_global):  ",dim(dfCI_global) )
    strTitleTotal <- paste("Error rates and differences for ",factorDifference,sep="")
    groupedPlotCI_1 <- ggplot(dfCI_global[dfCI_global$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=3,col="black",fill="white", shape=1) +
      xlim(c(0,1)) + 
      ggtitle("Error Rate")+
      theme(
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      ) +
      labs(title = "Error Rate",y="")
    # 
    groupedPlotCI_differences1 <- ggplot(dfCI_global_differences[dfCI_global_differences$question=="reverseB",], aes(x=mean_CI,y=orderCategoryCombination )) +
      geom_vline(xintercept = 0) +
      geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
      geom_point(size=2,col="black",fill="white", shape=1) +
      geom_point( aes(x=-edgeSize,fill=significantDifference, col="#FF0000", alpha = 0.5 *significantDifference,size=significantDifference), alpha = 0.5)+
      scale_size_manual(values=c(0.1,5)) +
      xlim(c(-edgeSize,edgeSize)) +
      ggtitle("Differences for Error Rate") +
      labs(title = 'Differences for Error Rate', y = "" ) +
      theme(
        strip.text.x = element_blank(),
        axis.ticks.y = element_blank(), axis.text.y = element_blank(),
        legend.position="none"
      )
    
  }
  
  cat("\nThe plots are generated. But are they fine?\n")
  grid.arrange(grobs=list(groupedPlotCI_1,groupedPlotCI_differences1), ncol=2,top=textGrob( strTitleTotal ) )
  # cat("not sure what to return")
  return (dfCI_global)
}


# dfCI_errorRate_B_differences_focus_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorDifference = "focus")
# dfCI_errorRate_B_differences_dMask_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_all, factorDifference = "dMask")

# dfCI_errorRate_B_differences_dMask_factoredby_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dMask")
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDComplex_focus = TRUE, factorDifference = "dMask")
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus

# # this combination is buggy!
# dfCI_errorRate_B_differences_focus_factoredby_scaling <- combine_genPlot_ErrorRate_CIandDifferences(d_sclFiltered, factorScaling =TRUE, factorDifference = "focus")
# # d_sclFiltered[d_sclFiltered$focus=="WHAT_Ql" & d_sclFiltered$scaling==2,]
# dfCI_errorRate_B_differences_scaling_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_sclFiltered, factorScaling =FALSE, factorDifference = "scaling")
# dfCI_errorRate_B_differences_distractor_noFactor <- combine_genPlot_ErrorRate_CIandDifferences(d_distrFiltered, factorScaling =FALSE, factorDifference = "distractor")

# dfCI_differences_distractor_noFactor <- combine_genPlot_CIandDifferences(d_distrFiltered, factorScaling =FALSE, factorDifference = "distractor")

# bug with this combination (probably an issue with rename function)
# dfCI_differences_dComplex_focus_factoredby_focus <- combine_genPlot_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dComplex_focus")
# dfCI_differences_dMask_factoredby_focus <- combine_genPlot_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDifference = "dMask")
# 
# # TODO!!! function that makes those graphs without making factorDifference and factorVariations
# dfCI_errorRate_B_differences_dMask_factoredby_focus_dComplex_focus <- combine_genPlot_ErrorRate_CIandDifferences(d_measurement_filtered, factorFocus = TRUE, factorDComplex_focus = TRUE, factorDifference = "")
# # TODO potentially look at reordering?!


# strFormulaTest <- "~ orderMaskComplex"
# ggplot(dfCI_errorRate_B_test2, aes(x=mean_CI,y=orderCategoryCombination)) +
#   geom_vline(xintercept = 0) +
#   geom_errorbar(aes(xmin=low_CI, xmax=high_CI)) +
#   geom_point(size=3,col="black",fill="white", shape=1) # +
#   # facet_wrap( as.formula(strFormulaTest) , dir="v", ncol=1)


genAndPlot_errorRate_correctB_scaling <- function (d_scl0, d_scl1, d_scl2) {
  boot_scl0 <- boot(d_scl0$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,0)
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_scl1$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,1)
  # 
  boot_scl2 <- boot(d_scl2$correctB,samplemean,10000)
  ci_scl2 <- boot.ci(boot.out = boot_scl2, type = c("norm", "basic", "perc", "bca"));
  mean_scl2 <- boot_scl2$t0;
  l_ci_scl2 <- ci_scl2$normal[2];
  h_ci_scl2 <- ci_scl2$normal[3];
  res_scl2 <- c(mean_scl2,l_ci_scl2,h_ci_scl2)
  res_scl2 <- c(res_scl2,2)
  
  df_errorRate <- data.frame(res_scl0,res_scl1,res_scl2)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,scaling=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=scaling)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}

genAndPlot_errorRate_correctB_distractor <- function (d_h, d_n){
  boot_scl0 <- boot(d_h$correctB,samplemean,10000)
  ci_scl0 <- boot.ci(boot.out = boot_scl0, type = c("norm", "basic", "perc", "bca"));
  mean_scl0 <- boot_scl0$t0;
  l_ci_scl0 <- ci_scl0$normal[2];
  h_ci_scl0 <- ci_scl0$normal[3];
  res_scl0 <- c(mean_scl0,l_ci_scl0,h_ci_scl0)
  res_scl0 <- c(res_scl0,"h")
  # cat("\n bootTest scale0 mean: ",mean,", l_ci:",l_ci,", h_ci: ",h_ci);
  boot_scl1 <- boot(d_n$correctB,samplemean,10000)
  ci_scl1 <- boot.ci(boot.out = boot_scl1, type = c("norm", "basic", "perc", "bca"));
  mean_scl1 <- boot_scl1$t0;
  l_ci_scl1 <- ci_scl1$normal[2];
  h_ci_scl1 <- ci_scl1$normal[3];
  res_scl1 <- c(mean_scl1,l_ci_scl1,h_ci_scl1)
  res_scl1 <- c(res_scl1,"n")
  
  df_errorRate <- data.frame(res_scl0,res_scl1)
  df_errorRate <- data.frame(t(df_errorRate))
  df_errorRate <- rename(df_errorRate,mean_CI=X1);df_errorRate <- rename(df_errorRate,low_CI=X2);df_errorRate <- rename(df_errorRate,high_CI=X3);df_errorRate <- rename(df_errorRate,distractor=X4)
  cols <- c("mean_CI","low_CI","high_CI");df_errorRate[,cols] <- lapply( df_errorRate[,cols],as.numeric)
  
  groupedErrorRateB <- ggplot(df_errorRate, aes(x=mean_CI,y=distractor)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(0,1)) + 
    ggtitle("Error rate question B")
  
  groupedErrorRateB
}

genAndPlot_differences_errorRateB_scaling <- function (d_scl0, d_scl1, d_scl2, logFunction=FALSE){
  questions <- c("correctB")
  diffsOfBoots_0_1_B <- bootQuestionsDifferences_directSubstract(d_scl0,d_scl1,"correctB", logFunction=logFunction)
  diffsOfBoots_0_2_B <- bootQuestionsDifferences_directSubstract(d_scl0,d_scl2,"correctB", logFunction=logFunction)
  diffsOfBoots_1_2_B <- bootQuestionsDifferences_directSubstract(d_scl1,d_scl2,"correctB", logFunction=logFunction)
  diffsOfBoots_0_1_B <- c(diffsOfBoots_0_1_B, "Differences Scaling 0 and 1")
  diffsOfBoots_0_2_B <- c(diffsOfBoots_0_2_B, "Differences Scaling 0 and 2")
  diffsOfBoots_1_2_B <- c(diffsOfBoots_1_2_B, "Differences Scaling 1 and 2")
  
  cat("\ndiffsOfBoots_0_1_B: ",toString(diffsOfBoots_0_1_B))
  
  diffsOfBoots_0_1_B <- data.frame(diffsOfBoots_0_1_B);
  diffsOfBoots_0_1_B <- data.frame(t(diffsOfBoots_0_1_B));
  diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,mean_CI=X1);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,low_CI=X2);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,high_CI=X3);diffsOfBoots_0_1_B <- rename(diffsOfBoots_0_1_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_0_1_B[,cols] <- lapply( diffsOfBoots_0_1_B[,cols],as.numeric)
  
  diffsOfBoots_0_2_B <- data.frame(diffsOfBoots_0_2_B);
  diffsOfBoots_0_2_B <- data.frame(t(diffsOfBoots_0_2_B))
  diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,mean_CI=X1);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,low_CI=X2);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,high_CI=X3);diffsOfBoots_0_2_B <- rename(diffsOfBoots_0_2_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_0_2_B[,cols] <- lapply( diffsOfBoots_0_2_B[,cols],as.numeric)
  
  diffsOfBoots_1_2_B <- data.frame(diffsOfBoots_1_2_B);
  diffsOfBoots_1_2_B <- data.frame(t(diffsOfBoots_1_2_B))
  diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,mean_CI=X1);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,low_CI=X2);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,high_CI=X3);diffsOfBoots_1_2_B <- rename(diffsOfBoots_1_2_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  diffsOfBoots_1_2_B[,cols] <- lapply( diffsOfBoots_1_2_B[,cols],as.numeric)
  
  cat("\ngenerated the data frames")
  
  dfCI_global <- data.frame()
  dfCI_global$mean_CI[0] <- 0; dfCI_global$low_CI[0] <- 0;dfCI_global$high_CI[0] <- 0;dfCI_global$category_combination[0] <- 0; dfCI_global$question[0] <- 0;

  dfCI_global <- rbind(dfCI_global, diffsOfBoots_0_1_B); dfCI_global <- rbind(dfCI_global, diffsOfBoots_0_2_B); dfCI_global <- rbind(dfCI_global, diffsOfBoots_1_2_B);
  
  groupedPlotDiffB <- ggplot(dfCI_global, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-1,1)) + 
    ggtitle("Differences of Error Rate: Scaling")
  
  grid.arrange(groupedPlotDiffB, ncol=1)
  
}
# genAndPlot_differences_errorRateB_scaling(d_scl0, d_scl1, d_scl2)

genAndPlot_differencesBoot_distractor <- function (d,d2,focus="",dMask="",dComplex_focus="",R=10000, logFunction=FALSE){
  questions <- c("correctB")
  diffsOfBoots_1_2_B <- bootQuestionsDifferences_directSubstract(d,d2,question=questions[1],focus=focus,dMask=dMask,dComplex_focus=dComplex_focus,R=R, logFunction=logFunction)
  diffsOfBoots_1_2_B <- c(diffsOfBoots_1_2_B, "diff distractor and normal")
  df_B <- data.frame(diffsOfBoots_1_2_B);
  df_B <- data.frame(t(df_B))
  df_B <- rename(df_B,mean_CI=X1);df_B <- rename(df_B,low_CI=X2);df_B <- rename(df_B,high_CI=X3);df_B <- rename(df_B,difference_name=X4)
  cols <- c("mean_CI","low_CI","high_CI");
  df_B[,cols] <- lapply( df_B[,cols],as.numeric)
  
  groupedPlotDiffB <- ggplot(df_B, aes(x=mean_CI,y=difference_name)) +
    geom_vline(xintercept = 0) +
    geom_errorbarh(aes(xmin=low_CI, xmax=high_CI, height= .5)) +
    geom_point(size=3,col="black",fill="white", shape=1) +
    xlim(c(-1,1)) + 
    ggtitle("diffB")
  
  grid.arrange(groupedPlotDiffB, ncol=1)
}

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

genAndPlotTrust_measurement_overall <- function(d){
  d <- subset(d, is.numeric(d$trustA1) & is.numeric(d$trustA2) & is.numeric(d$trustA3) & is.numeric(d$trustB))
  
  dfTrust_measurement_overAll <- genDF_measurement_trust(d)
  plotTrustA1_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA1",], aes(x=0,y = perc*100, fill = factor(trustA1))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust MwM", y = "percent", fill = "trustA1") +
    scale_fill_manual("trustA1", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 5)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank()) 
  
  plotTrustA2_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA2",], aes(x=0,y = perc*100, fill = factor(trustA2))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust OM", y = "", fill = "trustA2") +
    scale_fill_manual("trustA2", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA3_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustA3",], aes(x=0,y = perc*100, fill = factor(trustA3))) + guides(fill=FALSE) +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust MP", y = "", fill = "trustA3") +
    scale_fill_manual("trustA3", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustB_overAll <- ggplot(dfTrust_measurement_overAll[ dfTrust_measurement_overAll$trustType == "trustB",], aes(x=0,y = perc*100, fill = factor(trustB)))  +
    geom_bar(stat="identity", width = 0.4) +
    labs(x = "Trust SC", y = "", fill = "trustB") +
    scale_fill_manual("Trust", values = c("0" = "#ffffb2", "1" = "#fed976", "2" = "#feb24c","3"="#fd8d3c","4"="#f03b20","5"="#bd0026")) +
    theme_minimal(base_size = 10)+ 
    theme(text = element_text(size = 10), axis.text.x = element_blank())
  
  plotTrustA1_overAll +  plotTrustA2_overAll +  plotTrustA3_overAll +  plotTrustB_overAll + plot_layout(ncol = 4, widths = c(1, 1))
}
# genAndPlotTrust_measurement_overall(d_measurement_filtered)

genAndPlot_correctB_measurement_mask <- function(d) {
  dfCorrectB_measurement_dMask <-genDF_measurement_correctB_dMask(d)
  dfCorrectB_measurement_dMask <- renameGroupedData(dfCorrectB_measurement_dMask)
  plotCorrectB_dMask <- ggplot(dfCorrectB_measurement_dMask, aes(x = factor(orderMaskComplex), y = perc*100, fill = factor(correctB))) +
    geom_bar(stat="identity", width = 0.7) +
    labs(x = "Measurement", y = "percent", fill = "correctB") +
    theme_minimal(base_size = 14)+ 
    theme(text = element_text(size = 10)) +
    theme(text = element_text(size = 10),axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))  
  
  grid_maskDiff_correctB <- grid.arrange(plotCorrectB_dMask,  
                                         top = textGrob("maskDiff",gp=gpar(fontsize=20,font=3)))
}
# genAndPlot_correctB_measurement_mask(d_measurement_filtered)
