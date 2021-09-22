# ---- Libraries loading
library(gridExtra)
library(grid)
library(boot) 
library(ggplot2) 
library(dplyr)
library(lattice)
library(scales)
setwd("C:/Users/Kevin/Dropbox/Courses/PhD documents/R_studyResultsAnalysis")

# ---- Data loading
d_alt <- read.table(file="data/transformed/survey_complete_measurement_all_2021_09_18_headerAdapted.csv",TRUE, ",")

# ---- Functions

# ---- Calls