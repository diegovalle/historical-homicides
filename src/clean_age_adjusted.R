



ma <- read.csv("data/municipality-heads.csv")

CleanHomicides <- function(year) {
  ages <- read.csv(str_c("data/inegi-occurrence/", year, ".csv.bz2"),
                   skip = 5,
                   fileEncoding = "windows-1252")
  head(ages)
  ages <- subset(ages, X.1 != "No especificado" &
                 X.1 != "Total" &
                 ##X.2 != "No especificado" &
                 X.2 != "Total" &
                 X.2 != "" &
                 X.3 != "No especificado")
  ##X.3 != "Total")
  ages <- ages[-grep("=CONCATENAR", ages$X),]
  ages$id <- as.numeric(str_c(str_sub(ages$X, 2,3), str_sub(ages$X, 5,7)))
  ages$X <- NULL
  names(ages)[1:3] <- c("MunName", "AgeGroup", "Sex")

  ages[4:ncol(ages)] <- apply(ages[4:ncol(ages)], 2,
                              function(x) as.numeric(gsub(",", "", x)))
  ages[is.na(ages)] <- 0
  
  
  ages$AgeGroup <- as.character(car::recode(ages$AgeGroup, "'Menores de 1 año' = '.0';
                                      '1-4 años' = '1.4';
                                      '5-9 años' = '5.9';
                                      '10-14 años' = '10.14';
                                      '15-19 años' = '15.19';
                                      '20-24 años' = '20.24';
                                      '25-29 años' = '25.29';
                                      '30-34 años' = '30.34';
                                      '35-39 años' = '35.39';
                                      '40-44 años' = '40.44';
                                      '45-49 años' = '45.49';
                                      '50-54 años' = '50.54';
                                      '55-59 años' = '55.59';
                                      '60-64 años' = '60.64';
                                      '65-69 años' = '65.69';
                                      '70-74 años' = '70.74';
                                      '75-79 años' = '75.79';
                                      '80-84 años' = '80.84';
                                      '85 años y más' = '85plus';
                                      'No especificado' = 'NA'"))
#Replace the string NA with the real NA
  ##ages[ages$AgeGroup == "NA",]$AgeGroup <- NA
  ages$Sex <- as.character(car::recode(ages$Sex, "'Total' = 'Total';
                                      'Hombre' = 'Male';
                                      'Mujer' = 'Female'"))
  ages$Homicides <- apply(ages[ ,4:(ncol(ages)-1)], 1, sum)
  ages <- ages[,c(1:3, ncol(ages)-1, ncol(ages))]
  ages$Year <- year
  ##ages <- melt(ages, id = c("MunName", "AgeGroup", "Sex", "id"))
  names(ages) <-  c("MunName", "AgeGroup", "Sex", "id", "Homicides", "Year")
  ages <- ages[,names(ages) <-  c("MunName", "AgeGroup", "Sex",
                                  "id", "Year", "Homicides")]
  ages
  ##ages$Year <- as.numeric(gsub("X", "", ages$Year))
}

agerates <- local({
  ages94 <- data.frame()
  for(year in 1994:2010) {
    t <- CleanHomicides(year)
    ages94 <- rbind(ages94, t)
  }
  rm(t)

  ddply(subset(ages94, Sex == "Total"), .(Year), summarise, sum = sum(Homicides))

  ma <- read.csv("data/municipality-heads.csv")

  ages <- read.csv("data/ages90.csv.bz2", skip = 4, fileEncoding = "windows-1252")
  head(ages)
  ages <- subset(ages, X.1 != "No especificado" &
                 X.1 != "Total" &
                 ##X.2 != "No especificado" &
                 X.2 != "Total" &
                 X.3 != "No especificado")
  ##X.3 != "Total")
  ages <- ages[-grep("=CONCATENAR", ages$X),]
  ages$id <- as.numeric(str_c(str_sub(ages$X, 2,3), str_sub(ages$X, 5,7)))
  ages$X <- NULL
  names(ages)[1:3] <- c("MunName", "AgeGroup", "Sex")
  
  ages[4:ncol(ages)] <- apply(ages[4:ncol(ages)], 2,
                            function(x) as.numeric(gsub(",", "", x)))
  ages[is.na(ages)] <- 0
  

  ages$AgeGroup <- as.character(car::recode(ages$AgeGroup, "'Menores de 1 año' = '.0';
                                      '1-4 años' = '1.4';
                                      '5-9 años' = '5.9';
                                      '10-14 años' = '10.14';
                                      '15-19 años' = '15.19';
                                      '20-24 años' = '20.24';
                                      '25-29 años' = '25.29';
                                      '30-34 años' = '30.34';
                                      '35-39 años' = '35.39';
                                      '40-44 años' = '40.44';
                                      '45-49 años' = '45.49';
                                      '50-54 años' = '50.54';
                                      '55-59 años' = '55.59';
                                      '60-64 años' = '60.64';
                                      '65-69 años' = '65.69';
                                      '70-74 años' = '70.74';
                                      '75-79 años' = '75.79';
                                      '80-84 años' = '80.84';
                                      '85 años y más' = '85plus';
                                      'No especificado' = 'NA'"))

  ages$Sex <- as.character(car::recode(ages$Sex, "'Total' = 'Total';
                                      'Hombre' = 'Male';
                                      'Mujer' = 'Female'"))
  ages <- melt(ages, id = c("MunName", "AgeGroup", "Sex", "id"))
  names(ages) <-  c("MunName", "AgeGroup", "Sex", "id", "Year", "Homicides")
  ages$Year <- as.numeric(gsub("X", "", ages$Year))
  
  ages <- subset(ages, Year < 1994)
  ages <- rbind(ages, ages94)
  
  ##Check that the sums make sense
  ddply(subset(ages, Sex == "Total"), .(Year), summarise, sum = sum(Homicides))

  sum(subset(ages, is.na(AgeGroup))$Homicides) /
    sum(ages$Homicides)
  ##Merge Tulum with Solidaridad
  ages[which(ages$id == 23009 & ages$Homicides > 0),]
  ages[which(ages$id == 14125 & ages$Homicides > 0),]

  ages[which(ages$id == 14125 & ages$Homicides > 0),]$MunName <- "Solidaridad"
  ages[which(ages$id == 23009 & ages$Homicides > 0),]$MunName <- "Arandas"
  ages[which(ages$id == 14125 & ages$Homicides > 0),]$id <- 14008
  ages[which(ages$id == 23009 & ages$Homicides > 0),]$id <- 23008
  
  newmuns <- subset(ages, id %in% c(14008, 23008))
  newmuns <- ddply(newmuns, .(MunName, AgeGroup, Sex, id, Year),
                summarise, Homicides = sum(Homicides))
  ages <- subset(ages, !id %in% c(14008, 23008))
  ages <- rbind(ages, newmuns)
  
  ages[which(ages$id == 23009 & ages$Homicides > 0),]
  ages[which(ages$id == 14125 & ages$Homicides > 0),]

  
  ## ages[which(ages$id == 23008 &
  ##            ages$AgeGroup == "20.24" &
  ##            ages$Sex %in% c("Male", "Total") &
  ##            ages$Year == 2008),]$Homicides <-  ages[which(ages$id == 23008 &
  ##              ages$AgeGroup == "20.24" &
  ##              ages$Sex %in% c("Male", "Total") &
  ##              ages$Year == 2008),]$Homicides + 1
  ## ages <- subset(ages, id != 23009)
  
  ages <- ages[,c(4,1:3,5,6)]



  ##Convert AgeGroups 0 & 1.4 -> 0.4
  ages$AgeGroup2 <- as.character(ages$AgeGroup)
  
  ages[ages$AgeGroup2 == ".0" & !is.na(ages$AgeGroup2),]$AgeGroup2 <-"0.4"
  ages[ages$AgeGroup2 == "1.4" & !is.na(ages$AgeGroup2),]$AgeGroup2 <-"0.4"
  agestop <- ddply(subset(ages, AgeGroup2 %in% c("0.4")),
                   .(id, MunName, AgeGroup2, Sex, Year),
                   function(df) sum(df$Homicides),
                   .progress = "text",
                   .parallel = TRUE)
  names(agestop) <-  c("id", "MunName", "AgeGroup", "Sex", "Year", "Homicides")
  agesbottom <- subset(ages, !AgeGroup2 %in% c("0.4"))
  agesbottom$AgeGroup2 <- NULL
  ages <- rbind(agestop, agesbottom)
  rm(agesbottom); rm(agestop)
  ages$MunName <- NULL
  ages$Sex <- factor(ages$Sex)
  ages$AgeGroup <- factor(ages$AgeGroup)
  
  ids <- as.numeric(levels(factor(ma$id)))
  agegroups <- factor(levels(factor(ages$AgeGroup)))
  sexes <- factor(levels(factor(ages$Sex)))
  years <- as.numeric(levels(factor(ages$Year)))
  
  len <- length(agegroups) * length(sexes)  * length(years)
  ages2 <- data.frame(id = rep(ids, each = len),
                      AgeGroup = rep(agegroups, each = length(sexes) *length(years)),
                      Sex = rep(sexes, each = length(years)),
                      Year = years
                      )
  nrow(ages2) == len * length(ids)
  head(ages, 30)
  message("Making sure the data includes all municipalities...\n")
  ages <- merge(ages2, ages, by = c("id", "AgeGroup", "Sex", "Year"),
                all.x = TRUE)
  rm(ages2)
  ages$Homicides[is.na(ages$Homicides)] <- 0

  ##Replace the string NA with the real NA
  ages[ages$AgeGroup == "NA",]$AgeGroup <- NA
  write.csv(ages,
            file = bzfile("clean-data/homicides-agegroup-sex1990-2010.csv.bz"),
            row.names = FALSE)

  ##Check that the sums make sense
  ddply(subset(ages, Sex == "Total"), .(Year), summarise, sum = sum(Homicides))


########################################################
#Adjust data for NA ages
########################################################

  agescast <- cast(id + Sex + Year ~ AgeGroup, data = ages,
                   value = "Homicides", fun.aggregate = sum)
  tot <- apply(agescast[4:ncol(agescast)], 1, sum)
  agescast[4:(ncol(agescast) -1)] <- apply(agescast[4:(ncol(agescast) -1)], 2,
                          function(x) ifelse(tot > 0, x + ((x / tot ) * (agescast$"NA")),
                              0))

  
  agescast$"NA" <- NULL
  head(agescast, 11)
  
  ages <- melt(agescast, id = c("id", "Sex", "Year"))
  names(ages) <- c("id", "Sex", "Year", "Homicides", "AgeGroup")
  ages <- ages[, c("id", "AgeGroup", "Sex", "Year", "Homicides")]
  rm(agescast)
  nrow(ages) == nrow(na.omit(ages))
  head(ages)

  message("Merging homicide data with population data...\n")
  agerate <- merge(ages, pop, by = c("id", "Year", "AgeGroup", "Sex"),
                   all.x = TRUE)
  nrow(agerate[which(is.na(agerate$Population)),]) == 0
  head(agerate)
  agerate$MunName <- NULL
  

  agerate$rate <- with(agerate, Homicides / Population * 10^5)
  return(agerate)
})

save(agerates, file = "cache/agerates.RData")
