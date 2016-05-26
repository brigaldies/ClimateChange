library(shiny)
library(DT)

shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Climate Change - Is Your Country Warming Up?"),
        wellPanel(
            fluidRow(
                column(10, 
                       uiOutput("helpText")
                )            
            ),
            fluidRow(
                column(4, 
                        uiOutput("countries")
                ),
                column(2,
                       uiOutput("yearFrom")
                ),
                column(2,
                       uiOutput("yearTo")
                )
            ),
            fluidRow(
                column(10, 
                       p(strong("Data source"), 
                         ": ",
                         tags$i('Climate Change: Earth Surface Temperature Data, Exploring global temperatures since 1750'),
                         ", prepared by ",
                         a(href = "https://www.kaggle.com/berkeleyearth/climate-change-earth-surface-temperature-data", "Kaggle"),
                         ", with the raw data provided by ",
                         a(href = "http://berkeleyearth.org/data/", "Berkeley Earth"))
                )            
            )            
        ),
        tabsetPanel(type = "tabs",
                    tabPanel("Browse Data", 
                             verticalLayout(
                                 helpText("Use the grid below to browse the selected temperature data. \
                                          See the pagination buttons below the grid. Use the Search input field to sub-filter the data."),
                                 DT::dataTableOutput('table1'))),
                    tabPanel("Temperature Uncertainty", 
                             verticalLayout(
                                 helpText("This graph plots the temperature measurement uncertainty over time. \
                                          This could help in the selection of a year range with a consistent uncertainty."),
                                 plotOutput("uncertaintyPlot"))),
                    tabPanel("Trend Line and Hypothesis Testing", 
                             verticalLayout(
                                 helpText("This graph plots the temperature trend over time for your selected country and by city using a 'linear smooth' regression.\
                                          The graph also shows for each city the result of a Student t-test (Using R t.test) of the null hypthesis that the temperature has not statistically increased \
                                          between the two decades surrounding the selected starting and end years respectively. \
                                          The t.test is calculated as one-sided (alternative='greater'), with a 5% significance level (H0 rejected if p-value <= 0.05), not paired, and without equal variance. \
                                          The blue and red data points represent the two compared populations in the t.test."),
                                 plotOutput("temperatureplot")))
        ),
        wellPanel(
            fluidRow(
                column(6, 
                       helpText("Click on the Download button to download the selected data:")
                ),
                column(4,
                       downloadButton('downloadData', 'Download')
                )
            )
        )
    )
))