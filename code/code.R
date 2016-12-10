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
library(lubridate)
library(stringr)

## ------------------------------
## Read in datasets
## ------------------------------
data.comp <- fread("../data/compustatFA.csv")
names(data.comp) <- tolower(names(data.comp))
data.crsp <- fread("../data/CRSP.csv")
names(data.crsp) <- tolower(names(data.crsp))

## ------------------------------
## Process datasets
## ------------------------------

## Dates crsp
year <-


## ------------------------------
## Calculate variables
## ------------------------------

## Enterprise number
enterprises   <- data.crsp$comnam
n_enterprises <- length(unique(data.crsp$comnam))
##
n_enterprise <- data.crsp[,.N, by = comnam]
