# Required libraries
require(dplyr)
require(lubridate)
require(ggplot2)

data_raw <- read.csv('./GlobalLandTemperaturesByMajorCity.csv')
data0 <- na.omit(data_raw) # Remove incomplete records
data0 <- data0 %>% 
    mutate(Date = ymd(dt)) %>% 
    mutate(Year = year(Date)) %>% 
    mutate(Decade = as.factor(floor(Year/10)*10)) %>% 
    mutate(Month=month(Date)) %>%
    mutate(MonthLabel=month(Date, label = TRUE))
min_year = min(data0$Year)
max_year = max(data0$Year)

# Vector of countries
countries <- distinct(data0, Country)$Country

us_data <- data0 %>% filter(Country == 'United States')
us_data_grouped_by_year <- us_data %>% group_by(Year) %>% summarize(AverageTemperature = mean(AverageTemperature))
qplot(Year, AverageTemperature, data=us_data_grouped_by_year, geom=c("point","smooth"), main = c('United States')) + 
    aes(colour = AverageTemperature) + 
    scale_color_gradient(low="blue", high="red")

library(rsconnect)
deployApp(appDir='.', appFileManifest = './appFileManifest', appName = 'ClimateChange')

# More data exploration
distinct(filter(data0, Country == 'France'), City)$City
# Select a city
city <- 'Paris'
# Select decades
decades <- c(1900, 2010)
city_data <- filter(data0, City == city)
city_data_grouped_by_year <- city_data %>% group_by(Year) %>% summarize(AverageTemperature = mean(AverageTemperature))
city_data_grouped_by_decade_month <- city_data %>% group_by(Decade, Month, MonthLabel) %>% summarize(AverageTemperature = mean(AverageTemperature))

# Points and lines:
ggplot(filter(city_data_grouped_by_year, Year >= 1900),
       aes(x = Year, y = AverageTemperature)) + 
    geom_point(shape = 1) + 
    geom_smooth(method=lm)

# Points and lines:
ggplot(filter(city_data_grouped_by_decade_month, Decade %in% decades),
    aes(x = MonthLabel, y = AverageTemperature, group = Decade, colour = Decade)) + 
    geom_point() + 
    geom_line()

# Bar plot
ggplot(filter(city_data, Decade %in% decades),
       aes(x = MonthLabel, y = AverageTemperature, fill=Decade)) +
    geom_bar(stat="identity", position=position_dodge())

# Boxplots
ggplot(filter(city_data, Decade %in% decades),
       aes(x = MonthLabel, y = AverageTemperature)) +
    geom_boxplot() + facet_grid(~ Decade)

# H0: The temperature has NOT increased for a given city between two given decades
decade_1 <- filter(city_data_grouped_by_decade_month, Decade == 1960)
decade_2 <- filter(city_data_grouped_by_decade_month, Decade == 1980)
# Student t.test
t1 <- t.test(decade_2$AverageTemperature - decade_1$AverageTemperature, alternative = "greater", var.equal = TRUE)
t1$p.value < .05

# Select a country
country <- 'United States'
#country <- 'France'
#country <- 'India'
# Select a year range
year_from = 1950
year_to = 2013

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

trendByCountryAndCity <- function(temperature_data, country, year_from, year_to) {
    message(paste('country:', country))
    message(paste('year_from:', year_from))
    message(paste('year_to:', year_to))
    range_1_year_from = max(year_from, min_year+5) - 5; message(paste('range_1_year_from;', range_1_year_from))
    range_1_year_to = max(year_from, min_year+5) + 5; message(paste('range_1_year_to;', range_1_year_to))
    range_2_year_from = min(year_to, max_year-5) - 5; message(paste('range_2_year_from;', range_2_year_from))
    range_2_year_to = min(year_to, max_year-5) + 5; message(paste('range_2_year_to;', range_2_year_to))
    country_data <- temperature_data %>% filter(Country == country)
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

# TODO:
# 1. Dynamic Point colors & shapes. [DONE]
# 2. Year x axis by +10 increment. [DONE]
# 3. Label of the legend. [DONE]
# 4. Bold plot title. [DONE]
# 5. Write degree C (DONE) and H0 with proper subscripting

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


# Temperature uncertainty
ggplot(country_data, aes(x=Year,y=AverageTemperatureUncertainty))+
    geom_point(shape=1)+geom_smooth()+ggtitle(paste0("Average Temperature Uncertainty\n", country))+
    xlab("Year")+ylab("Average Temperature Uncertainty")