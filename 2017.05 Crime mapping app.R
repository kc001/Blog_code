###################
# Obtain the data #
###################

# https://data.brla.gov/Public-Safety/Baton-Rouge-Crime-Incidents/fabb-cnnu
library(RSocrata)
raw.data <- read.socrata("https://data.brla.gov/resource/5rji-ddnu.json")


#############################
# Prepare data for analysis #
#############################

str(raw.data)
range(raw.data$offense_date, na.rm=TRUE) # date range

data <- raw.data


# 1. Parse latitude and longitude variables
data$geolocation <- gsub(pattern="c(", rep="", x=data$geolocation.coordinates, fixed=TRUE)
data$geolocation <- gsub(pattern=")", rep="", x=data$geolocation, fixed=TRUE)

library(tidyr)
data <- separate(data, col=geolocation, into = c("longitude", "latitude"), sep = ", ", remove = FALSE)
options(digits = 9)
data$latitude <- as.numeric(data$latitude)
data$longitude <- as.numeric(data$longitude)


# 2. Categorical variable for time-of-day 
data$time <- as.numeric(data$offense_time)
data$time_of_day[data$time >= 700 & data$time <= 1500] <- "Morning (7 AM - 3 PM)"
data$time_of_day[data$time >= 1500 & data$time <= 2300] <- "Evening (3 PM - 11 PM)"
data$time_of_day[(data$time > 2300 & data$time <= 2359) | (data$time >= 0 & data$time < 700)] <- "Overnight (11 PM - 7 AM)"


# 3. Categorical variable for rolling, 12-month year
library(lubridate)
data$offense_date <- as.Date(data$offense_date)

today <- max(data$offense_date)
years.ago.1 <- today-years(1)
years.ago.2 <- today-years(2)
years.ago.3 <- today-years(3)
years.ago.4 <- today-years(4)
years.ago.5 <- today-years(5)
years.ago.6 <- today-years(6)

data$year_text[data$offense_date <= today & data$offense_date > years.ago.1] <- "5/2016 - 5/2017"
data$year_text[data$offense_date <= years.ago.1 & data$offense_date > years.ago.2] <- "5/2015 - 5/2016"
data$year_text[data$offense_date <= years.ago.2 & data$offense_date > years.ago.3] <- "5/2014 - 5/2015"
data$year_text[data$offense_date <= years.ago.3 & data$offense_date > years.ago.4] <- "5/2013 - 5/2014"
data$year_text[data$offense_date <= years.ago.4 & data$offense_date > years.ago.5] <- "5/2012 - 5/2013"
data$year_text[data$offense_date <= years.ago.5 & data$offense_date > years.ago.6] <- "5/2011 - 5/2012"
remove(today, years.ago.1, years.ago.2, years.ago.3, years.ago.4, years.ago.5, years.ago.6)


# 4. Crime incident labels
data$longer_desc <- paste(data$offense_date, data$offense_time, sep="  ")
data$longer_desc <- paste(data$longer_desc, data$offense_desc, sep=" ")


# 5. Create a folder for app and save data to app folder 
save(data, file="data.Rda")


##############
# app.R file #
##############

# Note that if you're re-creating the app on your own, the code below should be pasted into a separate file, and saved as "app.R"

library(shiny)
library(leaflet)
library(ggmap)
library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)

load("data.Rda")

# Subset data for past year
annual.data <- subset(data, year_text=="5/2016 - 5/2017")

# Remove observations with missing geocoordinates
annual.data <- subset(annual.data, !is.na(annual.data$latitude))

date.range <- range(annual.data$offense_date, na.rm=TRUE) # date range
date.header <- paste("Date Range = Rolling, 12-month year from", date.range[1], sep=" ")
date.header <- paste(date.header, "to", sep=" ")
date.header <- paste(date.header, date.range[2], sep=" ")

# Filter choices
types.of.crimes <- as.list(unique(data$crime))
types.of.crimes[[length(types.of.crimes)+1]] <- "All crimes"

times.of.day <- as.list(c("All times", "Morning (7 AM - 3 PM)", "Evening (3 PM - 11 PM)", "Overnight (11 PM - 7 AM)"))


######
# UI #
######

ui <- fluidPage(
    
    tags$h2("Baton Rouge Crime: Interactive Map"),
    tags$br(),
    wellPanel(h4("Filters"),
              fluidRow(
                  column(3, selectInput("type.of.crime", "Select type of crime:", choices = types.of.crimes, selected = "All crimes")),
                  column(3, selectInput(inputId = "time.of.day",  label = "Select the time of day:", 
                                        choices = times.of.day, selected = "All times")),
                  column(3, sliderInput("zoom", # set map's zoom level
                                        label = "Adjust map zoom level here or on the map itself.", 
                                        min = 10, max = 18, value = 12)), 
                  column(3, textInput(inputId = "address", label = "Add a marker for a Baton Rouge, LA address:", 
                                      value="", placeholder = "e.g., 900 North 3rd St"), 
                         actionButton("update" , label = HTML("Update map and center<br />around provided address"), icon("refresh"),
                                      class = "btn btn-primary"))
              )
    ), 
    
    fluidRow(
        column(8,
               tags$h4("Location of Crime Incidents in Baton Rouge"),
               tags$h5(date.header),
               tags$br(),
               leafletOutput("crime_map"),
               tags$br(),
               tags$p("Click on a numbered cluster to zoom in and view the locations of the included incidents in more detail. 
                      Zoom in to the level of a single incident and hover over the blue dot to see the date, time, and nature of each incident.")
               ),
        column(4,
               tags$h4("Crime over Time in the Mapped Area"),
               conditionalPanel("$('#bargraph').hasClass('recalculating')", tags$div('Recalculating... ')),
               plotOutput('bargraph')
        )
        ), 
    
    tags$br(),
    tags$hr(),
    tags$p("Crime data is from the Baton Rouge Police Department", 
           tags$a(href = "https://data.brla.gov/Public-Safety/Baton-Rouge-Crime-Incidents/fabb-cnnu", "Crime Incidents"), "open data set."),
    
    tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }" )
    
    )


##########
# Server #
##########

server <- function(input, output) {
    
    output$crime_map <- renderLeaflet({
        
        # Create map.data based upon filter criteria
        if (input$type.of.crime == "All crimes" & input$time.of.day == "All times") { 
            map.data <- annual.data
        } else if (input$type.of.crime == "All crimes" & input$time.of.day != "All times") {
            map.data <- subset(annual.data, time_of_day == input$time.of.day)
        } else if (input$type.of.crime != "All crimes" & input$time.of.day == "All times") {
            map.data <- subset(annual.data, crime == input$type.of.crime)
        } else { 
            map.data <- subset(annual.data, crime == input$type.of.crime & time_of_day == input$time.of.day)
        }
        
        crime_map <- leaflet(map.data) %>% 
            addTiles() %>%
            setView(-91.10, 30.42, zoom = input$zoom) %>% 
            addCircleMarkers(lng=-91.187409, lat=30.4570524, 
                             popup="<b>State Capitol Building</b><br>900 North 3rd St<br>", radius = 5, color = 'black') %>% 
            addCircleMarkers(data = map.data, ~longitude, ~latitude, label = ~longer_desc,
                             clusterOptions = markerClusterOptions(iconCreateFunction=JS("function (cluster) {  
                                                                                         var childCount = cluster.getChildCount(); 
                                                                                         var c = ' marker-cluster-';  
                                                                                         if (childCount >= 1000) {  
                                                                                         c += 'large';  
                                                                                         } else if (childCount < 1000 & childCount >= 100) {  
                                                                                         c += 'medium';  
                                                                                         } else { 
                                                                                         c += 'small';  
                                                                                         }  
                                                                                         
                                                                                         return new L.DivIcon({ 
                                                                                         html: '<div><span>' + childCount + '</span></div>', 
                                                                                         className: 'marker-cluster' + c, iconSize: new L.Point(40, 40) 
                                                                                         }); 
    }")))
        crime_map
        
})
    
    # Add in any manually entered address
    observeEvent(input$update, {
        
        # Geocode address
        full.address <- paste(input$address, "Baton Rouge, LA", sep=", ")
        geocoded.address <- geocode(full.address, messaging=TRUE, override_limit=TRUE)
        longitude <- geocoded.address$lon
        latitude <- geocoded.address$lat
        
        leafletProxy("crime_map") %>% 
            setView(lng=longitude, lat=latitude, zoom=input$zoom) %>% 
            addMarkers(lng=longitude, lat=latitude, popup=input$address)
        
    })
    
    # Returns aggregated crime incidents (over all years) that are in map bounds
    incidentsInBoundsAggregated <- reactive({
        
        bounds <- input$crime_map_bounds
        latRng <- range(bounds$north, bounds$south)
        lngRng <- range(bounds$east, bounds$west)
        
        
        # Remove datapoints with missing year_text values
        step1 <- base::subset(data, !is.na(data$year_text))
        
        # Select data points within the map bounds
        step2 <- base::subset(step1, latitude >= latRng[1] & latitude <= latRng[2] & 
                                  longitude >= lngRng[1] & longitude <= lngRng[2])
        
        # Select incidents that meet the user-specified filter criteria
        if (input$type.of.crime == "All crimes" & input$time.of.day == "All times") { 
            step3 <- step2
        } else if (input$type.of.crime == "All crimes" & input$time.of.day != "All times") {
            step3 <- subset(step2, time_of_day == input$time.of.day)
        } else if (input$type.of.crime != "All crimes" & input$time.of.day == "All times") {
            step3 <- subset(step2, crime == input$type.of.crime)
        } else { 
            step3 <- subset(step2, crime == input$type.of.crime & time_of_day == input$time.of.day)
        }
        
        # Aggregate data by year_text and crime
        step4 <- step3 %>% 
            dplyr::group_by(year_text, crime) %>% # grouping by year and crime
            dplyr::summarise(annual.incidents = n())
        
        return(step4)
        
    })
    
    output$bargraph <- renderPlot({
        
        # If no incidents are in view, don't plot
        if (nrow(incidentsInBoundsAggregated()) == 0) {
            return(NULL)
        } else {
            
            colourCount <- length(unique(data$crime)) # the number of colors we'll need for the bar chart
            getPalette <- colorRampPalette(brewer.pal(9, "Set1")) # use colorBrewer palette Set3
            
            ggplot(data=incidentsInBoundsAggregated(), aes(x = year_text, y = annual.incidents, fill=crime)) + 
                geom_bar(stat="identity") +
                scale_fill_manual(values = getPalette(colourCount)) + # use the number of colors and the color palette selected above
                scale_y_continuous(labels=comma) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
                      axis.title=element_text(size=14,face="bold")) +
                ylab("Annual Crime Incidents in This Area") +
                xlab("Year")
        } 
        
    })
    
}

shinyApp(ui = ui, server = server)
