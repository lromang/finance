##################################################
## This code generates the variables needed for
## the project.
##################################################


## ------------------------------
## Libraries
## ------------------------------
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)

## ------------------------------
## Read in datasets:
## ------------------------------
data.comp <- fread("../data/compustatFA.csv")
data.crsp <- fread("../data/CRSP.csv")

## ------------------------------
## Calculate variables:
## ------------------------------
