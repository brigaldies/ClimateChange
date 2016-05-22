library(shiny)
library(DT)

shinyUI(fluidPage(
    headerPanel("Climate Change"),
    sidebarPanel(
        # selectInput("country", "Choose a country:", choices = c("United States", "France", "Australia"))
        #conditionalPanel(
        #    condition = "output.loading",
        #    div(id = 'status', p('Loading...'))            
        #),
        uiOutput("countries")
    ),
    mainPanel(
        tabsetPanel(type = "tabs", 
                    tabPanel("Plot", plotOutput("plot")),
                    tabPanel("Data", DT::dataTableOutput('table1'))
        )        
    )
))