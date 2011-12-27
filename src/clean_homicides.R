########################################################
# Author: Diego Valle-Jones
# Website: www.diegovalle.net
# Date Created: Sat Dec 24 17:09:24 2011
# Email: diegovalle at gmail.com
# Purpose: Clean homicides from 1990 to 2010 
# Copyright (c) Diego Valle-Jones. All rights reserved

hompop <- local({

hom <- read.csv("data/1994.csv.bz2", skip = 4, fileEncoding = "windows-1252")
head(hom)
  #hom <- hom[grep("=CONCATENAR", hom[[2]]),]
hom <- hom[-grep("Extranjero", hom[[3]]),]

hom <- subset(hom, X != "No especificado")
hom$X <-  as.numeric(as.character(hom$X))
hom <- subset(hom, X >= 1994)
col2cvt <- 4:ncol(hom)
hom[ ,col2cvt] <- lapply(hom[ ,col2cvt],
                         function(x){as.numeric(gsub(",", "", x))})
hom$Tot <- apply(hom[ , col2cvt], 1, sum, na.rm = TRUE)
hom <- hom[ , c(1:3, ncol(hom))]
names(hom) <- c("Year", "Code", "MunName", "Homicides")
head(hom)



#Read the data by date of occurance after 1994
CleanHom1994 <- function(filename, skip, n) {
  hom <- read.csv(filename, skip = skip, fileEncoding = "windows-1252")
  head(hom)
  names(hom)[1:3] <- c("Code", "MunName", "Year")
  hom <- hom[-grep("Extranjero", hom[[n]]),]
  
  hom <- subset(hom, Year != "No especificado")
  hom$Year <-  as.numeric(as.character(hom$Year))
  hom <- subset(hom, Year >= 1994)
  col2cvt <- 4:ncol(hom)
  hom[ ,col2cvt] <- lapply(hom[ ,col2cvt],
                           function(x){as.numeric(gsub(",", "", x))})
  hom$Tot <- apply(hom[ , col2cvt], 1, sum, na.rm = TRUE)
  hom <- hom[ , c(1:3, ncol(hom))]
  names(hom) <- c("Code", "MunName", "Year", "Homicides")
  hom
}

#hom <- CleanHom1994("1994.csv", 4, 3)
hom.male <- CleanHom1994("data/1994male.csv.bz2", 5, 2)
hom.female <- CleanHom1994("data/1994female.csv.bz2", 5, 2)

#Read the data from 1990-1994 by date of registry
hom90 <- read.csv("data/1990male-female.csv.bz2", skip = 4, fileEncoding = "windows-1252")
head(hom90)
hom90 <- melt(hom90, id = c("X", "X.1", "X.2"))
hom90$variable <- as.numeric(gsub("X", "", hom90$variable))
names(hom90) <- c("Code", "MunName", "Sex", "Year", "Homicides")
hom90$Homicides <- as.numeric(gsub(",", "", hom90$Homicides))
hom90 <- cast(subset(hom90, Sex != "No especificado"), Code + MunName + Year ~ Sex)
hom90$V1 <- NULL
names(hom90) <- c("Code", "MunName", "Year", "Male", "Female", "Homicides")



head(hom)
head(hom.male)
hom <- merge(hom, hom.male, by = c("Code", "Year"), all.x = TRUE)
hom <- merge(hom, hom.female, by = c("Code", "Year"), all.x = TRUE)
hom$MunName.y <- NULL
hom$MunName <- NULL
names(hom) <- c("Code", "Year", "MunName", "Homicides", "Male", "Female")
hom <- rbind(hom, hom90)

hom$Code <- gsub("=CONCATENAR\\(|[0],|[. ]*,|) ", "", hom$Code)
hom$Code <- gsub("([0-9]+)$", " \\1", hom$Code)
hom$CVE_ENT <- str_sub(hom$Code, 2, 3)
hom$CVE_MUN <- str_sub(hom$Code, 5, 7)




hom[is.na(hom$Homicides),]$Homicides <- 0
hom[is.na(hom$Female),]$Female <- 0
hom[is.na(hom$Male),]$Male <- 0
names(hom) <- c("Code", "Year", "MunName", "TotalHom", "MaleHom", "FemaleHom",
                "CVE_ENT", "CVE_MUN")

head(hom)
hom$Code <- NULL
hom$MunName <- NULL
hom$id <- as.numeric(str_c(hom$CVE_ENT, hom$CVE_MUN))
hom$CVE_ENT <- NULL
hom$CVE_MUN <- NULL

hom <- na.omit(hom)




##Merge Tulum with Solidaridad
hom[which(hom$id == 23009),]
hom[which(hom$id == 14125),]

hom[which(hom$id == 14125),]$id <- 14008
hom[which(hom$id == 23009),]$id <- 23008

newmuns <- subset(hom, id %in% c(14008, 23008))
newmuns <- ddply(newmuns, .(id, Year),
                 summarise, TotalHom = sum(TotalHom),
                 MaleHom = sum(MaleHom),
                 FemaleHom = sum(FemaleHom))
hom <- subset(hom, !id %in% c(14008, 23008))
hom <- rbind(hom, newmuns)

hom[which(hom$id == 23009),]
hom[which(hom$id == 14125),]

hom <- subset(hom, id > 33)

mh <- read.csv("data/municipality-heads.csv")

hom2 <- expand.grid(id = mh$id, Year = 1990:2010)
hom2 <- merge(hom2, mh[,c("id", "MunName")])
hom <- merge(hom, hom2, by = c("id", "Year"),
             all = TRUE)

hom <- subset(hom, id %% 1000 != 999)
hom[is.na(hom$MunName),]


hom <- hom[order(hom$id, hom$Year),]
hom[is.na(hom$TotalHom),]$TotalHom <- 0
hom[is.na(hom$FemaleHom),]$FemaleHom <- 0
hom[is.na(hom$MaleHom),]$MaleHom <- 0

hom[which(hom$id == 23009),]
hom[which(hom$id == 14125),]



pop <- read.csv("data/pop-census.csv.bz2", fileEncoding = "UTF8")
hompop <- merge(hom, pop[,c(1,3,ncol(pop))], by.x = c("id", "Year"),
      by.y = c("id", "Year"), all.x = TRUE)
names(hompop) <- c("id", "Year", "TotalHomicides",
                   "MaleHomicides", "FemaleHomicides", "MunName", 
                   "TotalPopulation")

pop <- read.csv("data/popmale-census.csv.bz2", fileEncoding = "UTF8")
hompop <- merge(hompop, pop[,c(1,3,ncol(pop))], by.x = c("id", "Year"),
      by.y = c("id", "Year"), all.x = TRUE)
names(hompop) <- c("id", "Year", "TotalHomicides",
                   "MaleHomicides", "FemaleHomicides", "MunName", 
                   "TotalPopulation", "TotalMalePopulation")

pop <- read.csv("data/popfemale-census.csv.bz2", fileEncoding = "UTF8")
hompop <- merge(hompop, pop[,c(1,3,ncol(pop))], by.x = c("id", "Year"),
      by.y = c("id", "Year"), all.x = TRUE)
names(hompop) <- c("id", "Year", "TotalHomicides",
                   "MaleHomicides", "FemaleHomicides", "MunName", 
                   "TotalPopulation", "MalePopulation",
                   "FemalePopulation")



state.codes <- read.csv("data/state-codes.csv")
hompop$StateCode <- round(as.numeric(hompop$id) / 1000)
hompop <- merge(hompop, state.codes[,c("StateCode", "StateAbbrev")])
hompop <- hompop[,c("id", "StateCode", "MunName", "StateAbbrev",
                    "Year", "TotalHomicides",
                   "MaleHomicides", "FemaleHomicides",  
                   "TotalPopulation", "MalePopulation",
                   "FemalePopulation")]
hompop <- hompop[order(hompop$StateCode, hompop$MunName, hompop$Year),]


write.csv(hompop, file = bzfile("clean-data/homicides1990-2010.csv.bz2"),
          row.names = FALSE)

return(hompop)
})


