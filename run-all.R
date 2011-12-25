########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Sat Dec 24 17:09:48 2011
# Email: diegovalle at gmail.com
# Purpose: Clean a bunch of homicide files 
# Copyright (c) Diego Valle-Jones. All rights reserved

library(compiler)
library(ggplot2)
library(stringr)
library(gdata)
library(classInt)
library(ggplot2)
library(stringr)
library(car)
library(Hmisc)
library(geosphere)
library(Cairo)
library(plyr)
library(reshape)
library(rgdal)
library(maptools)
library(memoise)
library(data.table)
library(doMC)
library(directlabels)

registerDoMC(2) 
gpclibPermit()
theme_set(theme_bw())
options(stringAsFactors = FALSE)

kUrbanThreshold <- 2500


#Clean the population data
if(!file.exists("cache/population.RData")) {
  source("src/clean_population.R")
} else {
  load("cache/population.RData")
}


#Clean the homicide files with age groups
if(!file.exists("clean-data/homicides-agegroup-sex1990-2010.csv.bz2") |
   !file.exists("cache/agerates.RData")) {
  source("src/clean_age_adjusted.R")
} else {
  load("cache/agerates.RData")
}

#Clean the homicide files without age groups
source("src/clean_homicides.R")

#Check that the data makes sense
ddply(subset(agerates, Sex == "Total"), .(Year), summarise, sum(Homicides))

ddply(hompop, .(Year), summarise, hom = sum(TotalHomicides))
ddply(hompop, .(Year), summarise, hom = sum(FemaleHomicides))
ddply(hompop, .(Year), summarise, hom = sum(FemalePopulation))
ddply(hompop, .(Year), summarise, hom = sum(MalePopulation))

