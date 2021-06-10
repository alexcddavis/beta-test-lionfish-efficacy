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


# data wrangling================================================================

#this script reads in iris data and performs some simple data wrangling
#writes out files needed for model fitting
source('./code/data-wrangling.R')

# model fitting ================================================================

#this script read in appropes csvs and fits  a glm and makes a model prediction
#on in and out of sample data

rm(list = ls()) #clears model objects that you dont need
source('./code/model-fitting.R')

# vizualization ================================================================

#this script reads in two dataframs creates some simple plots

rm(list = ls())
source('./code/vizualization.R')


####
#sample script on how to stop your code running with an error
x=3

if(x == 2) {
  stop("you dummo")
}
print(x)