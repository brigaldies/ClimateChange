# Required libraries
require(dplyr)
require(lubridate)
require(ggplot2)
library(shiny)
library(DT)

# Load and prepare the data
execTime <- system.time({ 
    data_raw <- read.csv('./GlobalLandTemperaturesByMajorCity.csv')
})
message(paste("Data loaded in", round(execTime["elapsed"], 2), "secs"))
data0 <- na.omit(data_raw) # Remove incomplete records
message(paste0(nrow(data_raw), ' rows loaded.'))
data0 <- data0 %>% mutate(Date = ymd(dt)) %>% mutate(Year = year(Date))
min_year = min(data0$Year)
max_year = max(data0$Year)

# Vector of countries
countries <- sort(distinct(data0, Country)$Country)
countriesCount <- length(countries)

# T-test calculation between two populations
cityT.Test <- function(city, data_grouped_by_city_year, year_from, year_to) {
    range_1_year_from = max(year_from, min_year+5) - 5
    range_1_year_to = max(year_from, min_year+5) + 5
    range_2_year_from = min(year_to, max_year-5) - 5
    range_2_year_to = min(year_to, max_year-5) + 5
    range_1 <- filter(data_grouped_by_city_year, City == city & Year >= range_1_year_from & Year <= range_1_year_to) # +-5 years around 'year_from'
    range_2 <- filter(data_grouped_by_city_year, City == city & Year >= range_2_year_from & Year <= range_2_year_to) # +-5 years around 'year_to'
    # Return the student t.test p-value
    t.test(range_2$AverageTemperature - range_1$AverageTemperature, alternative = "greater")$p.value
}

trendByCountryAndCity <- function(country_data, country, year_from, year_to) {
    message(paste('country:', country))
    message(paste('year_from:', year_from))
    message(paste('year_to:', year_to))
    range_1_year_from = max(year_from, min_year+5) - 5; message(paste('range_1_year_from:', range_1_year_from))
    range_1_year_to = max(year_from, min_year+5) + 5; message(paste('range_1_year_to:', range_1_year_to))
    range_2_year_from = min(year_to, max_year-5) - 5; message(paste('range_2_year_from:', range_2_year_from))
    range_2_year_to = min(year_to, max_year-5) + 5; message(paste('range_2_year_to:', range_2_year_to))    
    cities <- as.vector(distinct(country_data, City)$City)
    message(paste(cities, ', '))
    country_data_grouped_by_city_year <- country_data %>% group_by(City, Year) %>% summarize(AverageTemperature = mean(AverageTemperature))
    
    city_pvalues <- sapply(cities, cityT.Test, data_grouped_by_city_year = country_data_grouped_by_city_year, year_from = year_from, year_to = year_to)
    message(paste(round(city_pvalues, 6), ''))
    
    # Plot labeller
    cityAndPvalue <- function(value) { 
        paste(value, 'p-value =', round(city_pvalues[value], 6), ifelse(city_pvalues[value] < 0.05, '\nH0 rejected', '\nH0 accepted'))
    }
    
    country_data_grouped_by_city_year <- mutate(country_data_grouped_by_city_year,
                                                TestPop = ifelse(Year >= range_1_year_from & Year <= range_1_year_to, 'T-Test Pop. 1',
                                                                 ifelse(Year >= range_2_year_from & Year <= range_2_year_to, 'T-Test Pop. 2', 'Excluded from T-Test')))
    
    hypothesis0 = '\n(Hypothesis H0: Temperatures are *not* rising)'
    legendTitle = 'Legend:'
    plot_title = paste('Yearly Average Temperatures in', country, 'Between', year_from, 'and', year_to, hypothesis0)
    #    plot_title = expression('Yearly Average Temperatured in ' * country * ' Between ' * year_from * 'and ' * year_to * '\n(Hypothesis ' * H[0] * ': Temperatures are *not* rising)')
    data_for_year_range = filter(country_data_grouped_by_city_year, Year >= year_from & Year <= year_to)
    minTemperature = floor(min(data_for_year_range$AverageTemperature))
    maxTemperature = ceiling(max(data_for_year_range$AverageTemperature))
    message(paste('minTemperature:', minTemperature))
    message(paste('maxTemperature:', maxTemperature))
    ggplot(data_for_year_range,
           aes(x = Year, y = AverageTemperature)) + 
        #geom_point(shape = 19, aes(colour = TestPop)) +     
        #geom_point(shape = 1, aes(colour = TestPop)) + 
        geom_point(aes(shape = TestPop, colour = TestPop)) + 
        scale_color_manual(name=legendTitle, values = c("T-Test Pop. 1" = "blue", "Excluded from T-Test" = "black", "T-Test Pop. 2" = "red")) +
        scale_shape_manual(name=legendTitle, values = c("T-Test Pop. 1" = 19, "Excluded from T-Test" = 1, "T-Test Pop. 2" = 19)) +
        geom_smooth(method=lm, colour = 'black', size = 1) +
        scale_x_continuous(breaks = seq(year_from, year_to, by = 10)) +
        scale_y_continuous(breaks = seq(minTemperature, maxTemperature, by = 1)) +
        facet_wrap(~City, labeller = labeller(City = cityAndPvalue)) +
        labs(x = 'Year', y = expression('Average Temperature (' * Degree~degree*C * ')'), title = plot_title) +
        theme(plot.title = element_text(lineheight=.8, face="bold"))
}

shinyServer(function(input, output) {
    
    # Select a country drop-down:
    output$countries <- renderUI({         
        selectInput("country", paste0("Select a country (", countriesCount, ' to choose from):'), 
                    choices = as.vector(countries),
                    selected = countries[0]
                    )        
    })  
    
    isCountrySelected <- reactive({
        isCountrySelected <- !is.null(input$country) && input$country != ""
        message(paste0('isCountrySelected()=', isCountrySelected, ', input$country=', input$country))
        isCountrySelected
    })      
    
    # Years range:
    output$yearFrom <- renderUI({
        if (isCountrySelected()) {
            min_year = min(country_data()$Year)
            max_year = max(country_data()$Year)
            selectInput("yearFrom", 'Between Year:', choices = seq(from = min_year, to = max_year, by = 1), selected = min_year)
        }
    })
    
    output$yearTo <- renderUI({
        if (isCountrySelected()) {
            min_year = min(country_data()$Year)
            max_year = max(country_data()$Year)            
            selectInput("yearTo", 'And Year:', choices = seq(from = min_year, to = max_year, by = 1), selected = max_year)
        }
    })        
                
    isYearRangeSelected <- reactive({
        isYearRangeSelected <- !is.null(input$yearFrom) && input$yearFrom != "" && !is.null(input$yearTo) && input$yearTo != ""
        message(paste0('isYearRangeSelected()=', isYearRangeSelected, ', input$yearFrom=', input$yearFrom, ', input$yearTo=', input$yearTo))
        isYearRangeSelected        
    })
    
    year_from <- reactive({
        year_from = as.numeric(input$yearFrom)
        year_to = as.numeric(input$yearTo)
        min(year_from, year_to)
    })
    
    year_to <- reactive({
        year_from = as.numeric(input$yearFrom)
        year_to = as.numeric(input$yearTo)
        max(year_from, year_to)        
    })    
    
    country_data <- reactive({
        filteredData = NULL
        if (isCountrySelected()) {
            message(paste0('Filtering with country == ', input$country))            
            filteredData <- data0 %>% filter(Country == input$country)                        
        }
        
        rowsCount = nrow(filteredData)
        message(paste0('country_data:', rowsCount, ' rows retrieved.'))
        filteredData        
    })    
    
    dataset <- reactive({            
        filteredData = NULL
        # Filter by country?
        if (isCountrySelected() && isYearRangeSelected()) {
            message(paste0('Filtering with country == ', input$country))
            message(paste('Year range:', year_from(), 'to', year_to()))   
            filteredData <- data0 %>% filter(Country == input$country & Year >= year_from() & Year <= year_to())                        
        }

        rowsCount = nrow(filteredData)
        message(paste0('dataset:', rowsCount, ' rows retrieved.'))
        filteredData
    })  
                
    # Group the data by year, and average the average temperature for each year
#     dataset_grouped_by_year <- reactive({              
#         dataset() %>% group_by(Year) %>% summarize(AverageTemperature = mean(AverageTemperature))        
#     })        
    
    # Tab: Data
    output$table1 <- DT::renderDataTable(
        DT::datatable(dataset(), options = list(pageLength = 10))
    )    
    
    # Tab: Plot
    output$temperatureplot <- renderPlot({
        if (!is.null(dataset()) && nrow(dataset()) > 0) {
            trendByCountryAndCity(country_data = country_data(), country = input$country, year_from(), year_to())        
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