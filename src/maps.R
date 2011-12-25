map.mun <- readOGR("maps/MUNICIPIOS-60.shp", "MUNICIPIOS-60")
map.mun <- spTransform(map.mun, CRS("+proj=longlat +ellps=WGS84"))
map.st <- readOGR("maps/ESTADOS-90.shp", "ESTADOS-90")
map.st <- spTransform(map.st, CRS("+proj=longlat +ellps=WGS84"))


map.mun$id <- as.numeric(str_c(map.mun$CVE_ENT, map.mun$CVE_MUN))
#centroids <- data.frame(coordinates(map.mun))
#centroids$id <- as.numeric(str_c(map.mun$CVE_ENT, map.mun$CVE_MUN))
#names(centroids) <- c("long.cen", "lat.cen", "id")
#dput(centroids[which(centroids$id %in% c(12077, 12078, 12079, 12080,
 #                                   12081, 14125, 15123, 15124, 15125,
  #                                  30211, 30212, 32058)),])
#

map.mun <- fortify(map.mun, region = "id")
map.st <- fortify(map.st)



whorates <- ddply(agerate, .(id, Year, Sex),
                  function(df) c(wtd.mean(df$rate, df$WHO), sum(df$Homicides)),
                  .progress = "text",
                  .parallel = TRUE)
ma <- read.csv("data/municipality-heads.csv")
whorates2 <- merge(whorates, ma, all.x = TRUE)
##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title 
##' @param df 
##' @param centroids 
##' @param startrange 
##' @param endrange 
##' @return 
##' @author diego
PlotMap <- function(df, startrange = c(1990,1990),
                    endrange = c(2007,2007), dist = NULL, Sex = "Total") {

  r <- range(c(startrange, endrange))
  
  map.df <- MapDF(df, startrange, endrange)
  ggplot(subset(map.df, Sex == Sex), aes(Long, Lat)) +
    geom_point(aes(size = (V2 + 1)*1.15), color = "black", alpha = .1, legend = FALSE) +
    geom_point(aes(color = V1, size = V2), alpha = 1) +
    scale_color_gradient2("log change in\nage-adjusted\nhomicide rate",
                          low = "#2166AC", mid = "gray90", high = "red") +
    scale_area(str_c("number of\nhomicides\n", r[1], "-", r[2]),
               to = c(2, 8), breaks = c(1,max(df$V2))) +
    geom_path(data = map.st, aes(long, lat, group = group), size = .3) +
    opts(title = str_c("Change in Homicide Rates from ",
           startrange[1],"-", startrange[2], " to ",
           endrange[1], "-", endrange[2])) +
    coord_map() 
}

MapDF <- function(df, startrange = c(1990,1990),
                    endrange = c(2007,2007)){
  whorates2 <- ddply(df, .(id, Sex),
            function(df)  c(mean(df$V1[(endrange[1]-1990+1):(endrange[2]-1990+1)]) -
                            mean(df$V1[(startrange[1]-1990+1):(startrange[2]-1990+1)]),
                            sum(df$V2[(startrange[1]-1990+1):(endrange[2]-1990+1)])),
                     .parallel = TRUE)
  whorates2$V1 <- ifelse(whorates2$V1 >= 0, log1p(whorates2$V1), -log1p(-whorates2$V1))
  #whorates2$V2 <- log1p(whorates2$V2)
  merge(whorates2, ma)
}

PlotMap(whorates)
PlotMap(whorates, 1990:1992, 2003:2004)
ggsave("graphs/map-diff1992-0304.png", type = "cairo", dpi = 100, w = 10, h = 7)
PlotMap(whorates, 1992:1993, 1995:1996)
PlotMap(whorates, 2002:2003, 2005:2006)




PlotBorderMap <- function(df, ma, Sex = "Total", startrange = c(1990,1990),
                    endrange = c(2007,2007), dist = 200){
  df <- MapDF(subset(merge(df, ma), DistUSPort <= dist), startrange, endrange)
  r <- range(c(startrange, endrange))
  
  ggplot(subset(df, Sex == Sex), aes(Long, Lat)) +
    geom_point(aes(size = (V2 + 1)*1.15), color = "grey", alpha = .9, legend = FALSE) +
    geom_point(aes(color = V1, size = V2), alpha = .9) +
    scale_color_gradient2("log change in\nage-adjusted\nhomicide rate",
                          low = "#2166AC", mid = "gray90", high = "red") +
    scale_area(str_c("number of\nhomicides\n", r[1], "-", r[2]),
               to = c(2, 15), breaks = c(1,max(df$V2))) +
    geom_path(data = map.st, aes(long, lat, group = group), size = .3) +
    opts(title = str_c("Change in Homicide Rates from ",
           startrange[1],"-", startrange[2], " to ",
           endrange[1], "-", endrange[2])) +
    coord_map(ylim = c(min(df$Lat)-1, max(df$Lat) + 1),
              xlim = c(min(df$Long)-1, max(df$Long) + 1) ) +
    opts(title = str_c("Change in Homicide Rates from ",
           startrange[1],"-", startrange[2], " to ",
           endrange[1], "-", endrange[2]))
}

PlotBorderMap(whorates, ma,
              Sex = "Total",
              1992:1993,
              1995:1996)
ggsave("graphs/ban1994.png", type = "cairo", dpi = 100, w = 10, h = 7)



PlotBorderMap(whorates, ma,
              Sex = "Total",
              2002:2003, 2005:2006)
ggsave("graphs/ban2004.png", type = "cairo", dpi = 100, w = 10, h = 7)







borderrates.adj <- local({
  ageratedist <- merge(agerate, ma[ ,c("id", "DistUSBorder")], by = "id")
  ageratedist$isBorder <- ageratedist$DistUSBorder <= 200


  borderrates <- ddply(t, .(isBorder, AgeGroup, Year, Sex),
                     function(df) {
                       sum(df$Homicides) / sum(df$Population) * 10^5
                     },
                     .progress = "text",
                     .parallel = TRUE)

  borderrates <- merge(borderrates, who)
  borderrates.adj <- ddply(borderrates, .(isBorder, Year, Sex),
                           function(df) {
                             wtd.mean(df$V1, df$WHO)
                           })
  return(borderrates)
})

ggplot(subset(borderrates.adj, Sex == "Total"),
       aes(Year, V1, group = isBorder, color = isBorder)) +
  geom_line()
ggsave("graphs/border90-09adj.png", type = "cairo", dpi = 100, w = 10, h = 7)


df <- MapDF(subset(merge(whorates, ma), DistUSPort <= 200), 2002:2003, 2005:2006)
border <- merge(df, agerate, by =c("id", "Sex"), all.x = TRUE)
border$group <- cut(border$distance, c(-Inf, 200000, Inf))
borderage <- merge(ddply(subset(border, Sex == "Total"), .(Year, group, AgeGroup),
                  function(df) sum(df$Homicides) / sum(df$Population) * 10^5),
      who)
borderage$group <- factor(borderage$group)

ggplot(ddply(, .(Year, group), function(df) wtd.mean(df$V1, df$WHO)),
       aes(Year, V1, group = group, color = group)) +
  geom_line() +
  scale_color_brewer()
  scale_colour_grey(start = 0, end = .8)

head(map.df)
ggplot(subset(map.df, Sex == "Total"), aes(distance, V1)) +
  geom_point(aes(size = V2)) +
  geom_smooth()


usa2 <- data.frame(map("usa", plot=FALSE)[c("x", "y")])
head(usa2)
plot(usa2, xlim = c(-120, -90), type = "l")
which(usa2$y == min(usa2[which(usa2$x < -115 & usa2$x > -120),]$y))
which(usa2$y == min(usa2[which(usa2$x < -95 & usa2$x > -100),]$y))

usa2 <- usa2[-c(6713:178),]
usa2 <- rbind(usa2[178:nrow(usa2),], usa2[1:177,])
#usa2 <- usa2[order(-usa2$x),]
usa2 <- subset(usa2, usa2$x > -120 & usa2$x < -90)
plot(usa2, type = "l")
tail(usa2)
head(usa2)
