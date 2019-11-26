#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("ggplot2")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Cars Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("What we are doing here is a boxplotting representing the relation between two variables.
                     Here our reference variable will always be mpg, and we can compare it to Cylinders, Hp and Gears. 
                     Each part of the boxplot represent a quartile. The black bar in the middle represent the average.
                     For instance if you want to see a boxplot showing relation between mpg and Gears, you juste have
                     to select Gears in the box."),
            
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
            helpText("Here we are plotting the relation between two columns : the reference column which here is mpg
                     and the chosen column (Cylinders, Hp, Gears )."),
            
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
    
    
    sidebarLayout(
        sidebarPanel(
            helpText("What we display here is a ggplot graphic showing the relation between two columns given an other column.
                     For instance,  if You choose Transmissions, Fuel efficiency and Gears it will create a ggplot for X being 
                     Transmissions and Y being Fuel eff./gears"),
            
            selectInput("variable_three","Variable:",
                        c("Transmission" = "am",
                          "Gears" = "gear")),
            selectInput("variable_four","Variable:",
                        c("Cylinders"="cyl",
                          "Weigth"="wt",
                          "Fuel efficiency"="mpg"),
                        selected = "wt"),
            selectInput("variable_five","Variable:",
                        c("Cylinders"="cyl",
                          "Weigth"="wt",
                          "Fuel efficiency"="mpg"
                        ),
                        selected = "mpg"),
            
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("dynamicPlot"),
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
    
    formulaTextdynamic <- reactive({
        paste(input$variable_three)
    })
    output$caption_one <- renderText({
        formulaText()
    })
    
    formulaTextdynamic_two <- reactive({
        paste(input$variable_four)
    })
    output$caption_two <- renderText({
        formulaText()
    })
    
    formulaTextdynamic_three <- reactive({
        paste(input$variable_five)
    })
    output$caption_three <- renderText({
        formulaText()
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
    
    output$dynamicPlot <-renderPlot({
        g <-  ggplot(cars, aes(cars[[formulaTextdynamic()]], cars[[formulaTextdynamic_two()]]/cars[[formulaTextdynamic_three()]])) 
        g + geom_point(size=4)
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)