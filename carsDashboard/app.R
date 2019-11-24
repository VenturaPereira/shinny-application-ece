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
    titlePanel("Cars Dashboard"),

    sidebarLayout(
        sidebarPanel(
            selectInput("variable_one","Variable:",
                        c("Cylinders"="cyl",
                          "Transmission" = "am",
                          "Gears" = "gear"))
        ),
     

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
        )
    ),
    sidebarLayout(
        sidebarPanel(
            selectInput("variable_two","Variable:",
                        c("Cylinders"="cyl",
                          "Hp" = "hp",
                          "Gears" = "gear"),
                        selected = "hp"),
            radioButtons("plotType","Plot type",
                         c("Scatter"="p", "Line"="l"))

        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("barPlot")
        )
    ),
   
    
    
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    cars <- mtcars
    
    formulaText <- reactive({
        paste("mpg ~", input$variable_one)
    })
    
    output$caption <- renderText({
        formulaText()
    })
    
    formulaTexts <- reactive({
        paste(input$variable_two)
    })
    output$try <- renderText({
        formulaTexts()
    })


    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        boxplot(as.formula(formulaText()),
                data = cars,
                col = "#75AADB", pch = 19)
    })
    
    output$barPlot <- renderPlot({
        plot(cars$mpg, cars[[formulaTexts()]], col = cars$am, type=input$plotType)
    })    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
