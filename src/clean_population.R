########################################################
#Section
########################################################
pop <- local({
  pop <- read.csv("data/pop-census.csv.bz2")
  ##Save the data for when we calculate the Rural vs Urban homicide rates
  totalpop <<- pop[,c("id", "Year", "Population")]
  pop$Population <- NULL
  pop <- melt(pop, id = c("id", "MunName", "Year"))
  pop$Sex <- "Total"
  
  popmen <- read.csv("data/popmale-census.csv.bz2")
  popmen$Population <- NULL
  popmen <- melt(popmen, id = c("id", "MunName", "Year"))
  popmen$Sex <- "Male"

  popwomen <- read.csv("data/popfemale-census.csv.bz2")
  popwomen$Population <- NULL
  popwomen <- melt(popwomen, id = c("id", "MunName", "Year"))
  popwomen$Sex <- "Female"

  pop <- rbind(pop, popmen, popwomen)
  pop$variable <- gsub("m|f|a", "", pop$variable)
  head(pop)
  names(pop) <- c("id", "MunName", "Year", "AgeGroup", "Population", "Sex")
  return (pop)
})

save(pop, file = "cache/population.RData")


