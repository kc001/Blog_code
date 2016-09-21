# To obtain the BLS data for this post, run the code from the previous post.

################
# Prepare data #
################

nola <- subset(series, area_code == 35380)

nola <- merge(industry, nola, by="industry_code", all.y = TRUE)
nola <- merge(data_type, nola, by="data_type_code", all.y = TRUE)
nola <- merge(area, nola, by="area_code", all.y = TRUE)

nola <- nola[order(nola$industry_code),]
row.names(nola) <- NULL
remove(industry, data_type, area)

# Select only those series that represent the number of employees over time and a non-seasonal series
nola <- subset(nola, data_type_code == 1 & seasonal == "U")
series.nums <- unique(nola$series_id)

# Select the series id numbers for the New Orleans area
# Remove month 13 ("M13") observations, as these contains annual averages
data <- subset(data.19.Louisiana, series_id %in% series.nums & period != "M13")
remove(series.nums, series, data.19.Louisiana)

# Create a date variable that R recognizes as a date
data$month <- as.numeric(gsub("M" , "", data$period))
data$date <- paste(data$month, '15', sep="-")
data$date <- as.Date(paste(data$date, data$year, sep="-"), format='%m-%d-%Y ')

# Employees is measured in thousands
data$employees <- data$value * 1000

data <- merge(data, nola, by="series_id", all.x = TRUE)
data <- data[order(data$industry_code, data$date),]

# Create a folder for your app and save data to app folder
setwd("** this is the path to your app folder **")
save(data, file="data.Rda")
save(nola, file="nola.Rda")


##############
# app.R file #
##############

# Note: the code below should be saved in a separate file from the data preparation code above, named app.R, 
# and saved to the app folder you created.

library(shiny)

load("data.Rda")
load("nola.Rda")
industries <- as.list(unique(nola$industry_name))

ui <- fluidPage(
    
    tags$h2("New Orleans Historic and Forecasted Employment Numbers, by Industry"),
    tags$br(),
    wellPanel(
        fluidRow(
            column(6, selectInput("industry", "Select industry:", choices = industries)),
            column(6, sliderInput(inputId = "start.year", 
                                  label = "Select the year with which to start the graph:", 
                                  value = 1995, min = 1990, max = 2015, sep=""))
        )
    ),
    tags$br(),
    plotOutput("graph"),
    
    tags$p("Black line represents observed, historic data. 
           Blue line represents forecasts, with 80% (dark grey) and 95% (light grey) confidence intervals. 
           Forecasts are generated using exponential smoothing.
           Note that confidence intervals are larger for date ranges that include Hurricane Katrina,
           as modeling this event incorporates a massive, unprecedented disruption in the overall trend."),
    tags$p("Vertical, dotted, red line represents date of Hurricane Katrina on August 29, 2005."),
    
    tags$hr(),
    tags$p("Industry and employment data from The US Bureau of Labor Statistics", 
           tags$a(href = "http://www.bls.gov/ces/", "Current Employment Statistics"), "survey.")
    
    )

server <- function(input, output) {
    
    industries <- as.list(unique(nola$industry_name))
    
    output$graph <- renderPlot({
        
        series <- as.character(subset(nola, industry_name == input$industry, select = "series_id"))
        
        temp.data <- subset(data, series_id == series & year >= input$start.year)
        
        # Create time series
        temp.data.ts <- ts(temp.data$employees, frequency=12, start=c(temp.data$year[1],temp.data$month[1]))
        
        # Generate forecast
        library(fpp)
        ets.model <- ets(temp.data.ts)
        
        # library(forecast)
        ets.fcasts <- forecast(ets.model, h=12, level=c(80,95))
        
        ets.fcasts <- as.data.frame(ets.fcasts)
        ets.fcasts$date <- as.Date(as.yearmon(row.names(ets.fcasts))) + 14
        names(ets.fcasts) <- c('forecast','lo80','hi80','lo95','hi95','date')
        
        temp.data <- merge(temp.data, ets.fcasts, by="date", all = TRUE)
        
        library(ggplot2)
        library(scales) # formatting numbers
        
        katrina <- as.Date("2005-08-29")
        
        title <- paste(input$industry, "Employees", sep = " ")
        title <- paste(title, "\nNew Orleans, LA Metropolitan Area", sep = "")
        
        graph <- ggplot(temp.data, aes(x=temp.data$date, y=temp.data$employees))
        graph <- graph + geom_line()              # add a line connecting the dots
        graph <- graph + geom_ribbon(aes(x = temp.data$date, ymin = temp.data$lo95, ymax = temp.data$hi95), fill="lightgrey")
        graph <- graph + geom_ribbon(aes(x = temp.data$date, ymin = temp.data$lo80, ymax = temp.data$hi80), fill="darkgrey")
        graph <- graph + geom_line(aes(x = temp.data$date, y = temp.data$forecast), colour="blue")
        graph <- graph + geom_vline(xintercept = as.numeric(katrina), linetype="dotted", colour="red")
        graph <- graph + labs(x="Year") 
        graph <- graph + scale_y_continuous(name="Number of Employees", labels = comma)
        graph <- graph + ggtitle(title)
        graph
        
    })
}

shinyApp(ui = ui, server = server)