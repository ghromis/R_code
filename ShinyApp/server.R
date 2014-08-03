library(quantmod)
library(portfolio)
data(dow.jan.2005)
source("helper.R")
# input$symb

shinyServer(function(input, output) {
    
     dataInput <- reactive({
        getSymbols("DJIA", src = "yahoo",
                   from = input$dates[1],
                   to = input$dates[2],
                   auto.assign = FALSE)
    })
    
    DJIA <- reactive({
        if (!input$adjust) return(dataInput())
        adjust(dataInput())
    })
    
    output$plot <- renderPlot({
        chartSeries(DJIA(), theme = chartTheme("white", bg.col="white"), 
                    type = "line", log.scale = input$log, TA = NULL)
        
    })
    
    output$summary <- renderPrint({
        def
    })
    
    output$table <- renderTable({
        data.frame(dow.jan.2005)
    })
    
})

