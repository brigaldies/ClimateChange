library(shiny)
library(DT)

shinyUI(fluidPage(
    verticalLayout(
        titlePanel("Climate Change: Is Your Country Warming Up?"),
        wellPanel(
            fluidRow(
                column(10, 
                       #helpText("Select a country and year range, and click on the Submit button to retrieve and perform the analyses:")
                       #helpText("Select a country and year range:")
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
#                 ,
#                 column(2,
#                        submitButton('Submit')
#                 )                
            )
        ),
        tabsetPanel(type = "tabs",
                    tabPanel("Browse Data", 
                             verticalLayout(
                                 helpText("Use the grid below to browse the selected data:"),
                                 DT::dataTableOutput('table1'))),
                    tabPanel("Temperature Uncertainty", 
                             verticalLayout(
                                 helpText("The plot below plots the temperature measurement uncertainty over time:"),
                                 plotOutput("uncertaintyPlot"))),
                    tabPanel("Trend", 
                             verticalLayout(
                                 helpText("The plot below provides the temperature trend over time:"),
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