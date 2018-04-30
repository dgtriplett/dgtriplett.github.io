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
ui <- shinyUI(fluidPage(
  
  # Application title
  titlePanel("MTCars Data - Regression Analysis"),
  
  # Sidebar with a slider input for value of MPG 
  sidebarLayout(
    sidebarPanel(
      selectInput("variable2", "Select the outcome variable for the linear regression:",
                  choices = c("Miles per Gallon" = "mpg",
                              "Horse Power" = "hp"),
                  selected = "mpg"),
      
      selectInput("variable1", "Select the predictor variable for the linear regression:",
                  choices = c("Cylinders" = "cyl",
                              "Transmission" = "am",
                              "Gears" = "gear"),
                  selected = "cyl"),
      
      checkboxInput("Abline", "Draw AB Line", TRUE)
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({    
    
    # generate CYL based on input$MPG from ui.R
    # x    <- mtcars[, 1] 
    #  mpg <- seq(min(x), max(x), length.out = 30)
    
    # draw the histogram with the specified value of mpg
    
    # hist(x, breaks = mpg, col = 'darkgray', border = 'white', main=paste("Histogram of MPG"),xlab="MPG")
    
    # mtcars.cyl.formula <- mpg ~ cyl
    
    mtcars.cyl.formula <- mtcars[,input$variable1] ~ mtcars[,input$variable2]
    mtcars.cyl.lm <- lm(formula=mtcars.cyl.formula, data=mtcars)
    
    # draw the scatter diagram with the specified value of mpg
    plot(mtcars.cyl.formula, data=mtcars, xlab = input$variable1, ylab = input$variable2)
  
    if (input$Abline){abline(mtcars.cyl.lm,col="Red")}
    
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

