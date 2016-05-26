# Required libraries
require(dplyr)
require(lubridate)
require(ggplot2)
library(shiny)
library(DT)
library(mgcv)

# -----------------------------------------------------------------------------
# Startup processing (One-time)
# -----------------------------------------------------------------------------

# Load and prepare the data
execTime <- system.time({ 
    data_raw <- read.csv('./GlobalLandTemperaturesByMajorCity.csv')
})
message(paste("Data loaded in", round(execTime["elapsed"], 2), "secs"))
data0 <- na.omit(data_raw) # Remove incomplete records
message(paste0(nrow(data_raw), ' rows loaded.'))
data0 <- data0 %>% mutate(Date = ymd(dt)) %>% mutate(Year = year(Date))

# Vector of countries
countries <- sort(distinct(data0, Country)$Country)
countriesCount <- length(countries)

# -----------------------------------------------------------------------------
# Function: formatParameter
# 
# Produces a string "parameter_name = parameter_value" for logging purposes.
#
# Arguments:
# param_name: Parameter name.
# param_value: Parameter value.
#
# Returns: The string "parameter_name = parameter_value". If parameter_value is
# NULL, the returned string is "parameter_value = NULL".
# -----------------------------------------------------------------------------
formatParameter <- function(param_name, param_value) {
    val = param_value
    if (is.null(param_value)) val = 'NULL'
    paste(param_name, '=', val)
}

# -----------------------------------------------------------------------------
# Function: cityT.Test
# 
# T-test temperature difference calculation between two decades.
#
# Arguments:
# city: The selected city.
# data_grouped_by_city_year: The country data grouped by city and year and aggregated average temperature.
# year_from: Selected starting year.
# year_to: Selected ending year.
# min_year: Min year of the country data (Max of the year mins by city)
# max_year: Max yaer of the country data (Min of the year maxs by city)
# 
# Returns: The t.test P-value.
# -----------------------------------------------------------------------------
cityT.Test <- function(city, data_grouped_by_city_year, year_from, year_to, min_year, max_year) {
    range_1_year_from = max(year_from, min_year+5) - 5
    range_1_year_to = max(year_from, min_year+5) + 5
    range_2_year_from = min(year_to, max_year-5) - 5
    range_2_year_to = min(year_to, max_year-5) + 5
    range_1 = filter(data_grouped_by_city_year, City == city & Year >= range_1_year_from & Year <= range_1_year_to) # +-5 years around 'year_from'
    range_2 = filter(data_grouped_by_city_year, City == city & Year >= range_2_year_from & Year <= range_2_year_to) # +-5 years around 'year_to'
    range_1_len = nrow(range_1)
    message(paste(city, 'range_1:', range_1_len, 'rows'))    
    range_2_len = nrow(range_2)
    message(paste(city, 'range_2:', range_2_len, 'rows'))
    range_len = min(range_1_len, range_2_len)
    range_1_adjusted = range_1[1:range_len,]
    range_2_adjusted = range_2[(range_2_len - range_len + 1):range_2_len,]
    # Return the student t.test p-value
    t.test(range_2_adjusted$AverageTemperature - range_1_adjusted$AverageTemperature, alternative = "greater")$p.value
}

# -----------------------------------------------------------------------------
# Funcion: trendByCountryAndCity
#
# For a selected country and range of years, trendByCountryAndCity performs the
# t.test of the null hypothesis (The temperature is NOT rising), and graphs a 
# scatter plot, one for each city in the selected country.
#
# Arguments:
# country_data: Filtered temoperature data by country.
# country: Selected country.
# year_from: Selected starting year.
# year_to: Selected ending year.
# min_year: Min year of the country data (Max of the year mins by city)
# max_year: Max yaer of the country data (Min of the year maxs by city)
#
# Returns: The plot.
# -----------------------------------------------------------------------------
trendByCountryAndCity <- function(country_data, country, year_from, year_to, min_year, max_year) {
    # Log arguments
    message(paste('country:', country))
    message(paste('year_from:', year_from))
    message(paste('year_to:', year_to))
    message(paste('min_year:', min_year))
    message(paste('max_year:', max_year))
    
    # Group the data by City and Year
    country_data_grouped_by_city_year <- country_data %>% group_by(City, Year) %>% summarize(AverageTemperature = mean(AverageTemperature))

    # Calculate the starting and ending decades to be used in the t.test
    range_1_year_from = max(year_from, min_year+5) - 5; message(paste('range_1_year_from:', range_1_year_from))
    range_1_year_to = max(year_from, min_year+5) + 5; message(paste('range_1_year_to:', range_1_year_to))
    range_2_year_from = min(year_to, max_year-5) - 5; message(paste('range_2_year_from:', range_2_year_from))
    range_2_year_to = min(year_to, max_year-5) + 5; message(paste('range_2_year_to:', range_2_year_to))    
    
    # Log the cities
    cities <- as.vector(distinct(country_data, City)$City)
    message(paste(cities, ', '))    
    
    # Apply the t.test for each city
    city_pvalues <- sapply(cities, 
                           cityT.Test, 
                           data_grouped_by_city_year = country_data_grouped_by_city_year, 
                           year_from = year_from, 
                           year_to = year_to,
                           min_year = min_year,
                           max_year = max_year)
    
    # Log the p-values
    message(paste(round(city_pvalues, 6), ''))
    
    # Plot labeller function (Used in ggplot)
    cityAndPvalue <- function(value) { 
        paste(value, 'p-value =', round(city_pvalues[value], 6), ifelse(city_pvalues[value] < 0.05, '\nH0 rejected', '\nH0 accepted'))
    }
    
    # Add a t.test population factor column in order to differentiate the data points used in the t.test
    country_data_grouped_by_city_year <- mutate(country_data_grouped_by_city_year,
                                                TestPop = ifelse(Year >= range_1_year_from & Year <= range_1_year_to, 'T-Test Pop. 1',
                                                                 ifelse(Year >= range_2_year_from & Year <= range_2_year_to, 'T-Test Pop. 2', 'Excluded from T-Test')))
    
    # Misc. labels and captions
    hypothesis0 = '\n(Null Hypothesis H0: Temperatures are *not* rising)'
    legendTitle = 'Legend:'
    plot_title = paste('Yearly Average Temperatures in', country, 'Between', year_from, 'and', year_to, hypothesis0)    
    
    # Filter the country data with the provided range of years
    data_for_year_range = filter(country_data_grouped_by_city_year, Year >= year_from & Year <= year_to)
    
    # Determine the min and max temperature in order to set the Y scale.
    minTemperature = floor(min(data_for_year_range$AverageTemperature))
    maxTemperature = ceiling(max(data_for_year_range$AverageTemperature))
    message(paste('minTemperature:', minTemperature))
    message(paste('maxTemperature:', maxTemperature))
    
    # Now plot!
    ggplot(data_for_year_range,
           aes(x = Year, y = AverageTemperature)) + 
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

# -----------------------------------------------------------------------------
# Shiny server
#
# The server uses a series of reactive programming in order to provide dynamic
# content for the country and years drop-downs.
# -----------------------------------------------------------------------------
shinyServer(function(input, output) {
    
    # Top-level help text:
    output$helpText <- renderUI({  
        if (isCountrySelected() && isYearRangeSelected()) {
            helpText("Select a country and year range:")
        } else {
            helpText("Loading ...")
        }
    })  
    
    # Select a country drop-down:
    output$countries <- renderUI({         
        selectInput("country", paste0("Select a country (", countriesCount, ' to choose from):'), 
                    choices = as.vector(countries),
                    selected = countries[0]
                    )        
    })  
    
    # Is a country selected?
    isCountrySelected <- reactive({
        isCountrySelected <- !is.null(input$country) && input$country != ""
        message(paste0('isCountrySelected()=', isCountrySelected, ', ', formatParameter('input$country', input$country)))
        isCountrySelected
    })      
    
    # For a given country: Min year for all cities (max of the mins)
    min_year <- reactive({
        min_year = max(country_data_grouped_by_city()$MinYear)
        message(paste('min_year =', min_year))
        min_year
    })
    
    # For a given country: Max year for all cities (min of the maxs)
    max_year <- reactive({
        max_year = min(country_data_grouped_by_city()$MaxYear) 
        message(paste('max_year =', max_year))
        max_year        
    })
    
    # Year From drop-down:
    output$yearFrom <- renderUI({
        if (isCountrySelected()) {                            
            selectInput("yearFrom", 'Between Year:', choices = seq(from = min_year(), to = max_year(), by = 1), selected = min_year())
        }
    })
    
    # Year To drop-down:
    output$yearTo <- renderUI({
        if (isCountrySelected()) {
            selectInput("yearTo", 'And Year:', choices = seq(from = min_year(), to = max_year(), by = 1), selected = max_year())
        }
    })        
                
    # Is a year range selected?
    isYearRangeSelected <- reactive({
        isYearRangeSelected <- !is.null(input$yearFrom) && input$yearFrom != "" && !is.null(input$yearTo) && input$yearTo != ""
        message(paste0('isYearRangeSelected()=', isYearRangeSelected, ', ', formatParameter('input$yearFrom', input$yearFrom), ', ', formatParameter('input$yearTo', input$yearTo)))
        isYearRangeSelected        
    })
    
    # Selected Year From:
    year_from <- reactive({
        year_from = as.numeric(input$yearFrom)
        year_to = as.numeric(input$yearTo)
        min(year_from, year_to)
    })
    
    # Selected Year To:
    year_to <- reactive({
        year_from = as.numeric(input$yearFrom)
        year_to = as.numeric(input$yearTo)
        max(year_from, year_to)        
    })    
    
    # Data filtered by the selected country:
    country_data <- reactive({
        filteredData = NULL
        if (isCountrySelected()) {
            message(paste0('Filtering with country == ', input$country))            
            filteredData <- data0 %>% filter(Country == input$country)                        
        }
        
        rowsCount = ifelse(is.null(filteredData), 0, nrow(filteredData))
        message(paste('country_data:', rowsCount, 'rows retrieved.'))
        filteredData        
    })    
    
    # Country data grouped by City to determine min and max years.
    country_data_grouped_by_city <- reactive({
        groupedData = NULL
        if (isCountrySelected()) {            
            groupedData <- country_data() %>% group_by(City) %>% summarize(MinYear = min(Year), MaxYear = max(Year))                        
        }        
        groupedData
    })
    
    # Country data filtered by year range:
    dataset <- reactive({            
        filteredData = NULL
        # Filter by country?
        if (isCountrySelected() && isYearRangeSelected()) {            
            message(paste('Filtering with year range:', year_from(), 'to', year_to()))   
            filteredData <- country_data() %>% filter(Year >= year_from() & Year <= year_to())                        
        }

        rowsCount = ifelse(is.null(filteredData), 0, nrow(filteredData))        
        message(paste('dataset:', rowsCount, 'rows retrieved.'))
        filteredData
    })  
                    
    # Tab: Data grid
    output$table1 <- DT::renderDataTable(
        DT::datatable(dataset(), options = list(pageLength = 10))
    )    

    # Tab: Average Temperature Uncertainty
    output$uncertaintyPlot <- renderPlot({
        if (!is.null(dataset()) && nrow(dataset()) > 0) {        
            plot_title = paste('Average Temperature Uncertainty in', input$country, 'Between', year_from(), 'and', year_to())    
            ggplot(dataset(), aes(x = Date, y = AverageTemperatureUncertainty)) +
                geom_point(shape = 1) +
                geom_smooth() + 
                labs(x = 'Year', y = expression('Average Temperature Uncertainty (' * Degree~degree*C * ')'), title = plot_title) +
                theme(plot.title = element_text(lineheight=.8, face="bold"))            
        }
    })
    
    # Tab: Trend line and Hypothesis Testing
    output$temperatureplot <- renderPlot({
        if (!is.null(dataset()) && nrow(dataset()) > 0) {
            trendByCountryAndCity(country_data = country_data(), country = input$country, year_from(), year_to(), min_year(), max_year())        
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