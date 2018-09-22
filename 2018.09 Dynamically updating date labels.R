
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

# date range
range(raw.data$offense_date, na.rm=TRUE)
min.date <- range(raw.data$offense_date, na.rm=TRUE)[1]
max.date <- range(raw.data$offense_date, na.rm=TRUE)[2]

data$year_text <- NA

for (i in 0:(year(max.date)-year(min.date))) {
    
    print(i)
    
    # For each rolling, 12-month year, pull the two years that bookend the date range:
    this.year <- max.date-years(i)
    last.year <- max.date-years(i+1) 
    last.year <- last.year + days(1) # +1 day so the days defining the boundary between rolling, 12-month years are not counted twice
    
    # text labels defining the month and year with which the rolling, 12-month year began, 
    # but use different logic for the first year in the data:
    if (year(this.year) > year(min.date)) {
        
        text <- paste(month(last.year), day(last.year), sep="/")
        text <- paste(text, year(last.year), sep="/")
        
    } else if (year(this.year)==year(min.date)) {
        
        text <- paste(month(min.date), day(min.date), sep = "/")
        text <- paste(text, year(min.date), sep = "/")
        
    }
    
    # Add text to the label indicating the month and year in which the rolling, 12-month year ended:
    text <- paste(text, as.character(month(max.date)), sep=" - ")
    text <- paste(text, day(this.year), sep="/")
    text <- paste(text, year(this.year), sep="/")
    
    # Assign individual crime incidents to their corresponding rolling, 12-month years.
    data$year_text[data$offense_date > max.date-years(i+1) & data$offense_date <= max.date-years(i)] <- text
    
    remove(this.year, last.year, text)
    
}

remove(min.date, max.date, i)

# Check that the labels were assigned correctly
library(dplyr)
data %>%
    group_by(year_text) %>%
    summarize(numberOfRecords = n(),
              min = min(offense_date, na.rm=TRUE),
              max = max(offense_date, na.rm=TRUE))

# 4. Crime incident labels
data$longer_desc <- paste(data$geolocation_address, data$offense_desc, sep=" - ")
data$longer_desc <- paste(data$longer_desc, data$offense_date, sep=" ")

# 5. Create a folder for app and save data to app folder 
original.wd <- getwd()
setwd("set this to the location of your app folder")
save(data, file="data.Rda")
setwd(original.wd)
remove(original.wd)



##############
# app.R file #
##############

# Note that if you're re-creating the app on your own, the code below should be pasted into a separate file, and saved as "app.R"

library(shiny)
library(leaflet)
library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)


load("data.Rda")


# Select year text for most recent year
most.recent <- max(data$offense_date, na.rm=TRUE)
most.recent <- subset(data, offense_date == most.recent, select = "year_text")
most.recent <- most.recent$year_text[1]

# Subset data for past year
annual.data <- subset(data, year_text == most.recent)

# Remove observations with missing geocoordinates
annual.data <- subset(annual.data, !is.na(annual.data$latitude))

date.range <- range(annual.data$offense_date, na.rm=TRUE) # date range
date.header <- paste("Date Range = Rolling, 12-month year from", date.range[1], sep=" ")
date.header <- paste(date.header, "to", sep=" ")
date.header <- paste(date.header, date.range[2], sep=" ")

# Filter choices
types.of.crimes <- unique(data$crime)
types.of.crimes <- sort(types.of.crimes)
types.of.crimes <- append("All crimes", types.of.crimes)

times.of.day <- as.list(c("All times", "Morning (7 AM - 3 PM)", "Evening (3 PM - 11 PM)", "Overnight (11 PM - 7 AM)"))


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
        
        # use leaflet to create the map, using the map.data dataset created above
        crime_map <- leaflet(map.data) %>% 
            addTiles() %>%
            setView(-91.10, 30.42, zoom = input$zoom) %>%  # sets the zoom level to whatever the user specifies or the default
            addCircleMarkers(lng=-91.187409, lat=30.4570524, 
                             popup="<b>State Capitol Building</b><br>900 North 3rd St<br>", radius = 5, color = 'black') %>% 
            addCircleMarkers(data = map.data, ~longitude, ~latitude, label = ~longer_desc, radius = 5, color = 'green',
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
        
        # this updates the crime_map by: 
        leafletProxy("crime_map") %>% 
            setView(lng=longitude, lat=latitude, zoom=input$zoom) %>%  # recentering it around the provided address and setting zoom level
            addMarkers(lng=longitude, lat=latitude, popup=input$address) # adding a marker for the address
        
    })
    
    # This code subsets the full dataset (over all years) 
    # by selecting the crime incidents that happened within current map bounds
    # and meet any user-specified filter criteria (in terms of type of crime or time-of-day),
    # then aggregates these by rolling, 12-month year and type-of-crime
    # to return an aggregated dataset.
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
    
    # this code generates the bar graph depicting year-over-year change in crime
    output$bargraph <- renderPlot({
        
        # If no incidents are in view, don't plot
        if (nrow(incidentsInBoundsAggregated()) == 0) {
            return(NULL)
        } else {
            
            colourCount <- length(unique(data$crime)) # the number of colors we'll need for the bar chart
            getPalette <- colorRampPalette(brewer.pal(9, "Set1")) # use colorBrewer palette Set1
            
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
           tags$a(href = "https://data.brla.gov/Public-Safety/Baton-Rouge-Crime-Incidents/fabb-cnnu", "Crime Incidents"), "open data set.")
    
                             )


shinyApp(ui = ui, server = server)

