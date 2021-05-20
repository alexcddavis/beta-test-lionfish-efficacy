########## 
##########
# This main file calls and runs all subsequent R files in this project analysis
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2021-05-20
##########
##########

# set-up =======================================================================
library(here)

# data wrangling================================================================

#this script reads in iris data and performs some simple data wrangling
#writes out files needed for model fitting
source(here('./code/data-wrangling.R'))

# model fitting ================================================================

#this script read in appropes csvs and fits  a glm and makes a model prediction
#on in and out of sample data

rm(list = ls()) #clears model objects that you dont need
source(here('./code/model-fitting.R'))

# vizualization ================================================================

#this script reads in two dataframs creates some simple plots

rm(list = ls())
source(here('./code/vizualization.R'))