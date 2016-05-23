library(shiny)
library(DT)

shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Climate Change: Is Your Country Warming Up?"),
        wellPanel(
            fluidRow(
                column(3, 
                        checkboxInput("filterByCountry", "Filter by country", FALSE),
                        uiOutput("countries")
                ),
#                 column(3,
#                         checkboxInput("filterByCity", "Filter by city", FALSE),
#                         uiOutput("cities")
#                 ),
                column(2,
                       checkboxInput("filterByYear", "Filter by year", FALSE)
                ),
                column(2,
                       uiOutput("yearFrom")
                ),
                column(2,
                       uiOutput("yearTo")
                )
                
            )
        ),
        tabsetPanel(type = "tabs",
                    tabPanel("Browse Data", DT::dataTableOutput('table1')),
                    tabPanel("Plot", plotOutput("plot"))
        ),
        wellPanel(
            fluidRow(                
                column(4, downloadButton('downloadData', 'Download'))))
    )
))