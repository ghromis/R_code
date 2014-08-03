library(shiny)

def= helpText(
     p("The Dow Jones Industrial Average is a price-weighted average of 30 
        significant stocks traded on the New York Stock Exchange and the Nasdaq. 
        The DJIA was invented by Charles Dow back in 1896."),

    p("Often referred to as \"the Dow,\" the DJIA is one of the oldest and single 
        most watched index in the world. The DJIA includes companies like 
        General Electric, Disney, Exxon and Microsoft. When the TV networks say 
        \"the market is up today,\" they are generally referring to the Dow."))


# Define UI for random distribution application 
shinyUI(fluidPage(
    
    # Application title
    titlePanel("Dow Jones Industrial Average"),
    
    # Sidebar with controls to select the random distribution type
    # and number of observations to generate. Note the use of the br()
    # element to introduce extra vertical spacing
    sidebarLayout(
        sidebarPanel(
            helpText("Dow Jones Industrial Average (DJIA) returns.
                             Information will be collected from yahoo finance."),
            
            #textInput("symb", "Symbol", "SPY"),
            
            dateRangeInput("dates", 
                           "Date range",
                           start = "2013-01-01", end = as.character(Sys.Date())),
            br(),
            br(),
            checkboxInput("log", "Plot y axis on log scale", 
                          value = FALSE),
            checkboxInput("adjust", "Adjust prices for inflation", 
                          value = FALSE)
            
        ),
        
        # Show a tabset that includes a plot, summary, and table view
        # of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", 
                        tabPanel("Plot", plotOutput("plot")), 
                        tabPanel("Summary", def), 
                        tabPanel("Table", tableOutput("table"))
            )
        )
    )
))

# shinyUI(fluidPage(
#     
#     titlePanel("stockVis"),
#     
#     sidebarLayout(
#         sidebarPanel(
#             helpText("Select a stock to examine. 
#                  Information will be collected from yahoo finance."),
#             
#             #textInput("symb", "Symbol", "SPY"),
#             
#             dateRangeInput("dates", 
#                            "Date range",
#                            start = "2013-01-01", end = as.character(Sys.Date())),
#             br(),
#             br(),
#             checkboxInput("log", "Plot y axis on log scale", 
#                           value = FALSE),
#             checkboxInput("adjust", "Adjust prices for inflation", 
#                           value = FALSE)
#         ),
#         
#         mainPanel(
#            plotOutput("plot")),
#             
#     )
#     
# ))
