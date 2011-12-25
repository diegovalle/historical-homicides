head(agerate)

ratesbyage <- ddply(agerates, .(Year, AgeGroup, Sex),
                    function(df) sum(df$Homicides) / sum(df$Population) * 10^5)

plotbyAgeGroup <- function(df, sex = "Total") {
  ratesbyage.sex <- subset(df, Sex == sex)
  ifelse(sex != "Total", title <- str_c( sex, " homicide rate, by age group"),
         title <- "Homicide rate by age group")
  p <- ggplot(ratesbyage.sex, aes(Year, V1, group = AgeGroup, color = AgeGroup)) +
    geom_line() +
    opts(title = title) +
    ylab("rate per 100,000") +
    xlab("year")
  direct.label(p, "last.bumpup")
}

plotbyAgeGroup(ratesbyage, "Male")
plotbyAgeGroup(ratesbyage, "Female")
plotbyAgeGroup(ratesbyage, "Total")
