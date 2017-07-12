library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(shiny)
library(httr)

tdf <- read_excel("./TourdeFrance.xlsx")
secrets <- read.csv("./secrets")
# secrets is a csv of the form:
# strava_client_id, strava_client_secret
# 111111,           "skgljsegkjlkjgljsegj080198n"

strava_client_id <- secrets$strava_client_id
strava_client_secret <- as.character(secrets$strava_client_secret)

#ggplot(tdf, aes(x=Year, y=`Total distance (km)`)) + geom_point()


tdfDistanceCum <- tdf %>% arrange(-Year) %>% mutate(cum = cumsum(`Total distance (km)`)) %>% select(Year, `Total distance (km)`, cum)

calcAthleteTDFDist <- function(athleteDistKm, tdfDistanceCum) {
  fullTdf <- sum(tdfDistanceCum$cum < athleteDistKm)
  
  remainingKm <- tdfDistanceCum[fullTdf+1,'cum'] - athleteDistKm
  nextTourDist <- tdfDistanceCum[fullTdf+1,'Total distance (km)']
  partTdf <- round((nextTourDist - remainingKm)/ nextTourDist , digits = 2)
  
  toReturn <- list(tdfs = fullTdf+partTdf[1,], fullTdf = fullTdf, partTdf=partTdf[1,], distToNextTour = ceiling(remainingKm)[1,])
  return(toReturn)
}

#calcAthleteTDFDist(athleteStats$all_ride_totals$distance / 1000, tdfDistanceCum)
#calcAthleteTDFDist(athleteStats$ytd_ride_totals$distance / 1000, tdfDistanceCum)
#calcAthleteTDFDist(athleteStats$recent_ride_totals$distance / 1000, tdfDistanceCum)

#paste0("You have cycled the equivalent distance of ", fullTdf+partTdf, " Tours de France. You just need to cycle another ",ceiling(nextTourDist),"to complete your next!")

tdfDistSummary <- tdf %>% filter(Year > 2006) %>% summarise(minDist = round(min(`Total distance (km)`)), maxDist = round(max(`Total distance (km)`)))

tdfDistMax <- tdf %>% arrange(-`Total distance (km)`) %>% head(1)


ui <- fluidPage(theme="bootstrap.css",
  
  conditionalPanel("!output.stravaData", 
    h1("You & ", img(src="stravaLogo.png"), " vs ", img(src="tdfLogo.png"))
  ),
  conditionalPanel("output.stravaData", 
     h1(htmlOutput("imagePath", inline=T), " & ", img(src="stravaLogo.png"), " vs ", img(src="tdfLogo.png"))
  ), 
  mainPanel(width=12,
      fluidRow(
        p(paste0("The Tour de France is a long event! In the last 10 years, the race distance is between: ", tdfDistSummary$minDist , "km and ", tdfDistSummary$maxDist ,"km. "), br(), paste0(" But that is nothing compared to ", tdfDistMax$Year, " when they cycled ", tdfDistMax$`Total distance (km)`, "km!")), 
        plotOutput("distanceGraph", height = 120)
        
      ),
      conditionalPanel("!output.stravaData", 
                       p("Sign in to strava below to see how you compare."),
                       #https://mryeti1.shinyapps.io/StravaTDFMakeover/
        fluidRow(a(href=paste0("https://www.strava.com/oauth/authorize?client_id=",strava_client_id,"&response_type=code&state=shinyButton&redirect_uri=https://mryeti1.shinyapps.io/StravaTDFMakeover/"), "Log In to Strava"))
      ),
      conditionalPanel("output.stravaData", 
        #wellPanel(textOutput("stravaData") ),
        fluidRow( 
          div(class="tdfNumDesc col-xs-12 col-sm-4 col-md-3 col-lg-4", textOutput("stravaData", inline=T), "In your strava lifetime, you have cycled: ", textOutput("stravaDist", inline=T), HTML("A&nbsp;distance equivalent to this many of the most recent Tours de France:")),
          div(class="tdfNum", textOutput("fullTdf", inline=T), textOutput("partTdf", inline=T))
        ),
        fluidRow( 
          div(class="tdfRemDesc", "You just need to cycle another ",  textOutput("remainingKm", inline=T), "to complete your next one. Why not get out on your bike tonight?")
        )
      )
  )
)


server <- function(input, output, session) {
  athleteStats <- reactive({ NULL })
  print("Load")
  outhContent <- reactive({
    queryString <- parseQueryString(session$clientData$url_search)
    req(queryString$code)
    outhResponse <- httr::POST(url="https://www.strava.com/oauth/token", body=list(client_id = strava_client_id, client_secret=strava_client_secret, code=queryString$code), encode="json")
    stop_for_status(outhResponse)
    outhContent <- content(outhResponse)
  })
  


  
  output$stravaData <- renderText({
      paste0("Welcome ", outhContent()$athlete$firstname,".")
  })
  athleteStats <- reactive({
    athleteId <- outhContent()$athlete$id
    print(athleteId)
    athleteStatsUrl <- paste0("https://www.strava.com/api/v3/athletes/",athleteId,"/stats")
    athleteBearer <- paste(outhContent()$token_type, outhContent()$access_token, sep=" ")
    
    athleteStatsResponse <- httr::GET(athleteStatsUrl, httr::add_headers(Authorization = athleteBearer ))
    stop_for_status(athleteStatsResponse)
    content(athleteStatsResponse)
  })
  
  all_tourNums <- reactive({
    calcAthleteTDFDist(athleteStats()$all_ride_totals$distance / 1000, tdfDistanceCum)
  })
  
  output$stravaDist <- renderText(
    paste0(comma(round(athleteStats()$all_ride_totals$distance / 1000)), "km.")
  )
  output$fullTdf <- renderText(
    
    all_tourNums()$fullTdf
  )
  output$imagePath <- renderText({c('<img src="',outhContent()$athlete$profile,'">')})
  
  output$partTdf <- renderText(
    
    substring(all_tourNums()$partTdf, 2)
  )
  output$remainingKm <- renderText(
    
    paste0(all_tourNums()$distToNextTour, "km")
  )
  
  bothDots <- reactiveVal( tdf %>% 
      filter(Year %in% c(2015, 2014, 1903, 1926)) %>%
      mutate(
        TDF=T,
        label=Year,
        dist = round(`Total distance (km)`)
      ) %>% 
      select(dist, TDF, label)
  )
  
  observeEvent(athleteStats(), {
    bothDots( bothDots() %>%
      rbind(
        data.frame(
          dist=c(
            round(athleteStats()$recent_ride_totals$distance / 1000),
            round(athleteStats()$ytd_ride_totals$distance / 1000),
            round(athleteStats()$all_ride_totals$distance / 1000)
          ),
          TDF=F, 
          label=c("You\n30D", "You\nYTD", "You\nAll"))
      ))
  }, ignoreNULL = T)
  
  output$distanceTable <- renderTable({bothDots()})
  output$distanceGraph <- renderPlot({
    ggplot(bothDots(), aes(x=1, y=dist, label = label, color=TDF)) +
      #geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
      geom_point(size = 15) + ylab("Total Distance (km)") + scale_color_manual(values=c("TRUE"="#fadd00", "FALSE"="#77B426"), guide=F) +
      geom_text(color = "black", size = 3, fontface="bold") + coord_flip() + expand_limits(y=0)  + scale_y_continuous(minor_breaks=NULL, breaks = bothDots()$dist, labels = bothDots()$dist, limits = c(0, max(bothDots() %>% filter(TDF==T) %>% select(dist))+50)) +
      theme_linedraw(base_size = 15) +
      theme(axis.text.x = element_text(angle = 60, hjust=1, vjust=1)
            ,axis.title.y = element_blank(), axis.text.y = element_blank(), axis.line.y = element_blank(), axis.ticks.y = element_blank()
            , rect = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()
      )
    
   })
}

shinyApp(ui, server)
