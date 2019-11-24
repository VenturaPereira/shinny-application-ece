#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("variable","Variable:",
                        c("Cylinders"="cyl",
                          "Transmission" = "am",
                          "Gears" = "gear"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    cars <- mtcars
    
    formulaText <- reactive({
        paste("mpg ~", input$variable)
    })
    
    output$caption <- renderText({
        formulaText()
    })

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        boxplot(as.formula(formulaText()),
                data = cars,
                col = "#75AADB", pch = 19)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
