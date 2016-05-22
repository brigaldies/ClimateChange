library(shiny)
library(DT)

shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Climate Change"),
        wellPanel(
            fluidRow(
                column(3, 
                        checkboxInput("filterByCountry", "Filter by country", FALSE),
                        uiOutput("countries")
                ),
                column(3,
                        checkboxInput("filterByCity", "Filter by city", FALSE),
                        uiOutput("cities")
                )
            )
        ),
        tabsetPanel(type = "tabs", 
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Browse Data", DT::dataTableOutput('table1'))
        ),
        wellPanel(
            fluidRow(                
                column(4, downloadButton('downloadData', 'Download'))))
    )
))