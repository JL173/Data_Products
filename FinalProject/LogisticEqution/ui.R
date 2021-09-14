
library(shiny)

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("The Logistic Equation"),
    
    mainPanel(
        h3("The equation looks like this"),
    
        h4("X[n+1]=rX[n](1-X[n])"),
    
        p("Where X[n] is the population size, between zero and one,
        at point 'n' in time, and where 'r', is the growth rate,
        how fast a population grows. Between 0 and 4. Why between
         0 and 4? Well, you'll see why."),
        h4("How high can you get the average population? (Blue Line)"),
        
        plotOutput("UserLogistic"),
        
        plotOutput("StableSolutions")
    ),
    
    sidebarPanel(
        sliderInput("X0", "starting Population",
                    value = 0.5, min = 0, max = 1, step = 0.01),
        sliderInput("R", "Growth Rate",
                    value = 0.5, min = 0, max = 4, step = 0.01)
    ),
    sidebarPanel(
        h3("See any Patterns?"),
        p("There are 'repeating' populations that occur,
          when r > 3. It eventually turns chaotic!"),
        h5("Click the button below to see"),
        
        actionButton("StableButton", "Stable Solutions"),
        p("You may have to wait a moment")
    )
    
))
