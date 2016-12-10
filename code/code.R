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

## Date crsp
year  <- str_sub(data.crsp$date, 1, 4)
month <- str_sub(data.crsp$date, 5, 6)
day   <- str_sub(data.crsp$date, 7, 8)
date  <- as.Date(paste(year, month, day, sep = "-"))
data.crsp$date <- date

## Date comp
year  <- str_sub(data.comp$datadate, 1, 4)
month <- str_sub(data.comp$datadate, 5, 6)
day   <- str_sub(data.comp$datadate, 7, 8)
date  <- as.Date(paste(year, month, day, sep = "-"))
data.comp$datadate <- date



## ------------------------------
## Calculate variables
## ------------------------------

## Enterprise number
enterprises   <- data.crsp$comnam
n_enterprises <- length(unique(data.crsp$comnam))
##
n_enterprise <- data.crsp[,.N, by = comnam]
