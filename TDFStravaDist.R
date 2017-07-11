library(readxl)
library(ggplot2)
library(dplyr)

tdf <- read_excel("~/Desktop/MakeoverMonday/TDF/TourdeFrance.xlsx")
colnames(tdf)

plot(tdf$Entrants)

ggplot(tdf, aes(x=Entrants)) + geom_histogram()
ggplot(tdf, aes(x=Finishers)) + geom_histogram()


ggplot(tdf, aes(x=Entrants, y=Finishers)) + geom_point() + coord_fixed() + geom_smooth()

ggplot(tdf, aes(x=Year, y=`Total distance (km)`)) + geom_point()

ggplot(tdf, aes(x=Year, y=`Number of stages`)) + geom_point()




athleteStatsUrl <- "https://www.strava.com/api/v3/athletes/1034/stats"

athleteStatsResponse <- httr::GET(athleteStatsUrl, httr::add_headers(Authorization = "Bearer bcdf6adf4d6ebaef6"))
stop_for_status(athleteStatsResponse)
athleteStats <- content(athleteStatsResponse)

athleteStats$all_ride_totals$distance
athleteStats$recent_ride_totals$distance

tdfDistanceCum <- tdf %>% arrange(-Year) %>% mutate(cum = cumsum(`Total distance (km)`)) %>% select(Year, `Total distance (km)`, cum)
tdfDistanceCum
calcAthleteTDFDist <- function(athleteDistKm, tdfDistanceCum) {
  fullTdf <- sum(tdfDistanceCum$cum < athleteDistKm)

  remainingKm <- tdfDistanceCum[fullTdf+1,'cum'] - athleteDistKm
  nextTourDist <- tdfDistanceCum[fullTdf+1,'Total distance (km)']
  partTdf <- round((nextTourDist - remainingKm)/ nextTourDist , digits = 2)
  
  toReturn <- list(tdfs = fullTdf+partTdf[1,], distToNextTour = ceiling(remainingKm)[1,])
  return(toReturn)
}

calcAthleteTDFDist(athleteStats$all_ride_totals$distance / 1000, tdfDistanceCum)
calcAthleteTDFDist(athleteStats$ytd_ride_totals$distance / 1000, tdfDistanceCum)
calcAthleteTDFDist(athleteStats$recent_ride_totals$distance / 1000, tdfDistanceCum)

paste0("You have cycled the equivalent distance of ", fullTdf+partTdf, " Tours de France. You just need to cycle another ",ceiling(nextTourDist),"km to complete your next!")


ggplot(tdf, aes(x=Year, color=(Year>2006), y=`Total distance (km)`)) + geom_point()

tdfDots <- tdf %>% 
  filter(Year %in% c(2015, 2014, 1903, 1926)) %>%
  mutate(
    TDF=T,
    label=as.character(Year),
    dist = round(`Total distance (km)`)
  ) %>% 
  select(dist, TDF, label)

tdfDots <- rbind(tdfDots, data.frame("dist"=c(1000, 2000, 11000), "TDF"=F, "label"=c("You\n30Days", "You\nYTD", "You\nAll")))

tdfDots["TDF" == TRUE,]
ggplot(tdfDots, aes(x=1, y=dist, label = label, color=TDF)) +
  #geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
  geom_point(size = 15) + ylab("Total Distance (km)") + scale_color_manual(values=c("TRUE"="#ff0000", "FALSE"="#004400"), guide=F) +
  geom_text(color = "white", size = 3) + coord_flip()  + scale_y_continuous(minor_breaks=NULL, breaks = tdfDots$dist, labels = tdfDots$dist, limits = c(0, max(tdf$`Total distance (km)`)+150)) +
  theme_linedraw() +
  theme(axis.text.x = element_text(angle = 60, hjust=1)
        ,axis.title.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank()
  #, plot.margin = unit(c(0, 0, 0, 0), "lines")
  , rect = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()
)
