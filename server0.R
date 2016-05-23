# Required libraries
require(dplyr)
require(lubridate)
require(ggplot2)
library(shiny)
library(DT)

# Load and prepare the data
data_raw <- read.csv('./GlobalLandTemperaturesByMajorCity.csv')
data0 <- na.omit(data_raw) # Remove incomplete records
data0 <- data0 %>% mutate(Date = ymd(dt)) %>% mutate(Year = year(Date))
min_year = min(data0$Year)
max_year = max(data0$Year)

# Vector of countries
countries <- distinct(data0, Country)$Country
countriesCount <- length(countries)

# Cities by Country
#citiesByCountries <- select(data0, c(City, Country)) %>% distinct %>% arrange(City)
#allCities <- select(citiesByCountries, City)

shinyServer(function(input, output) {
        
    filterByCountry <- reactive({
        filterByCountry <- !is.null(input$filterByCountry) && input$filterByCountry == TRUE && !is.null(input$country) && input$country != ""
        message(paste0('filterByCountry()=', filterByCountry, ' [input$filterByCountry=', input$filterByCountry, ', input$country=', input$country, ']'))
        filterByCountry
    })
    
#     filterByCity <- reactive({
#         filterByCity = !is.null(input$filterByCity) && input$filterByCity == TRUE && !is.null(input$city) && input$city != ""
#         message(paste0('filterByCity()=', filterByCity, ' [input$filterByCity=', input$filterByCity, ', input$city=', input$city, ']'))
#         filterByCity        
#     })    

    filterByYear <- reactive({
        filterByYear <- !is.null(input$filterByYear) && input$filterByYear == TRUE
        message(paste0('filterByYear()=', filterByYear, ' [input$yearFrom=', input$yearFrom, ', input$yearTo=', input$yearTo, ']'))
        filterByYear        
    })
    
    dataset <- reactive({            
        filteredData = data0
        # Filter by country?
        if (filterByCountry()) {
            message(paste0('Filtering with country == ', input$country))
            filteredData <- data0 %>% filter(Country == input$country)            
        } 
        # Filter by city?
#         if (filterByCity()) {
#             message(paste0('Filtering with city == ', input$city))
#             filteredData <- filteredData %>% filter(City == input$city)            
#         }       
        # Filter by year?
        if (filterByYear()) {
            year_from = as.numeric(input$yearFrom)
            year_to = as.numeric(input$yearTo)
            if (year_from > year_to) {
                year_tmp = year_from
                year_from = year_to
                year_to = year_tmp
            }
            message(paste('Year range:', year_from, 'to', year_to))            
        }

        rowsCount = nrow(filteredData)
        message(paste0(rowsCount, ' rows retrieved.'))
        filteredData
    })        
    
    # Select a country drop-down:
    output$countries <- renderUI({ 
        if (input$filterByCountry) {
            selectInput("country", paste0("Select a country (", countriesCount, ' to choose from):'), choices = as.vector(sort(countries)))
        }
    })          
        
    # Select a city drop-down:
    city_data <- reactive({            
        cities = allCities
        if (filterByCountry()) {
            cities = citiesByCountries %>% filter(Country == input$country) %>% select(City)            
        }
        cities$City
    })        
    
    output$cities <- renderUI({ 
        if (input$filterByCity) {
            cities = city_data()
            citiesCount = length(cities)
            caption = paste0('Select a city (', citiesCount, ' to choose from):')
            if (filterByCountry()) {            
                caption = paste0("Select a city in ", input$country, ' (', citiesCount, ' to choose from):')
            }
            selectInput("city", caption, choices = as.vector(cities))
        }
    })    
    
    # Years range
    output$yearFrom <- renderUI({
        if (input$filterByYear) {
            #sliderInput("yearRange", label = p('Select a year range:'), min = min_year, max = max_year, value = c(min_year, max_year))
            selectInput("yearFrom", 'Between Year:', choices = seq(from = min_year, to = max_year, by = 1))
        }
    })
    
    output$yearTo <- renderUI({
        if (input$filterByYear) {            
            selectInput("yearTo", 'And Year:', choices = seq(from = min_year, to = max_year, by = 1))
        }
    })    
    
    # Group the data by year, and average the average temperature for each year
    dataset_grouped_by_year <- reactive({            
        dataset() %>% group_by(Year) %>% summarize(AverageTemperature = mean(AverageTemperature))        
    })        
    
    # Tab: Data
    output$table1 <- DT::renderDataTable(
        DT::datatable(dataset(), options = list(pageLength = 15))
    )    
    
    # Tab: Plot
    output$plot <- renderPlot({
        plotTitle = 'Entire dataset'
        if (filterByCountry()) {
            plotTitle = input$country
            if (filterByCity()) {
                plotTitle = paste0(input$city, ', ', input$country)
            }
        } else if (filterByCity()) {
            plotTitle = input$city
        }
        
        if (nrow(dataset()) > 0) {
            qplot(Year, AverageTemperature, data=dataset_grouped_by_year(), geom=c("point","smooth"), main = plotTitle) + 
                aes(colour = AverageTemperature) + 
                scale_color_gradient(low="blue", high="red")        
        }
    })
        
    # Download button
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste(input$country, '.csv', sep='') 
        },
        content = function(file) {
            write.csv(country_data(), file)
        }
    )    
})