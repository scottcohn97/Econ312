#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dygraphs)

# Define UI for application that draws a histogram
ui <- fluidPage(

     mainPanel(
       dygraphOutput("dygraph", width = "150%")
     )
)

# Server
server <- shinyServer(function(input, output) {
  
  
  output$dygraph <- renderDygraph({
    dygraph(FairVersusActual,
            main = "Model Prediction vs Actual Outcome",
            ylab = "Vote Percentage",
            xlab = "Year") %>%
      dySeries("Fair_Prediction", label = "Prediction") %>%
      dySeries("Actual_Vote_Share", label = "Actual Vote") %>%
      # dyGroup(c("Fair_Prediction", "Actual_Vote_Share"), drawPoints = TRUE) %>% 
      dyOptions(stackedGraph = FALSE) %>%
      dyRangeSelector(height = 10) %>% 
      dyAxis("x", drawGrid = FALSE) %>% 
      dyLegend(width = 500) %>% 
      dyEvent("1964", "LBJ", labelLoc = "bottom") %>%
      dyEvent("1992", "Ross Perot", labelLoc = "bottom")
  })
  
})
# Run the application 
shinyApp(ui, server)

