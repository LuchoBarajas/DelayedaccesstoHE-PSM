#------------------------------------------------------------#
# Art 1 - Exploring Delayed Access to Higher Education:      #
# A Comprehensive Analysis from Student Dropout Perspective  #
#------------------------------------------------------------#

# Erase everything

rm(list = ls())

# Libraries and Working Directory

library(dplyr)
library(magrittr)
library(tidyr)
library(haven)
library(readxl)

setwd("/Users/luchobarajas/Documents/Investigación/Artículos de Investigación/Paper 1_Exploring Delayed Access to Higher Education/")


# Data base load

da_database = read_dta("Da_Database.dta")

# Data Cleaning

da_database %<>% select(-z_1, -z_2,-transito_1)

# Exploratory data Análisis

# Propensity Score Matching

# Dealing with ommited variable Bias
