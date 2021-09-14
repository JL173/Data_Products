
library(shiny)
library(dplyr)
## Mathematical Functions Below

# LogisticEquation
LogisticEquation <- function(Xn, r){
    
    # Xn, the first position, proportional to
    # the current population 0 < Xn < 1
    
    # r, the growth rate
    
    Xn_1 <- Xn
    Xn_1 <- r * Xn * (1 - Xn)
    Xn_1
}

# Repeated LogisticEquation to return a vector
LogisticLoop <- function(Xi, r, iter = 100){
    
    # Xi, the first position, proportional to
    # the current population 0 < Xi < 1
    
    # r, the growth rate
    
    # iter, number of iterations
    
    X_vector <- c(Xi)
    Xii <- NULL
    for (index in 1:iter){
        Xii <- LogisticEquation(Xi, r)
        X_vector <- c(X_vector, Xii)
        Xi <- Xii
    }
    X_vector
}

# Calculates Stable solutions for growth rates
StableSolutions <- function(n = 5, iter = 1500, tol = 1e-3){
    # n, power of 2 for bifurcations
    # iter, number of test solution pairs squared
    # tol, tolerance
    
    x <- seq(from = 0, to = 1, length.out = iter)
    
    r1 <- seq(from = 0, to = 3, length.out = iter*0.2)
    r2 <- seq(from = 3, to = 4, length.out = iter*0.8)
    
    r <- c(r1, r2)
    
    solutions <- data.frame(R = c(), X = c())
    
    for (ri in r){
        
        stable <- (ri - 1)/(ri + 1e-16)    # avoid /0
        
        for (xi in x){
            
            if (abs(xi - stable) <= tol){
                
                stable_pairs <- data.frame(R = c(ri), X = c(xi))
                
                sols <- c(xi)
                sol1_x <- xi
                
                for (index in 1:2^n){
                    
                    sol2_x <- LogisticEquation(sol1_x, ri)
                    
                    if ((abs(sol1_x - sol2_x) > tol) & !(sol2_x %in% sols)){
                        
                        sols <- c(sols, sol2_x)
                        stable_pairs <- rbind(stable_pairs, c(ri, sol2_x))
                        sol1_x <- sol2_x
                        
                    } else { 
                        
                        break
                    }
                }
                
                solutions <- rbind(solutions, stable_pairs)
            }
        }
    }
    names(solutions) <- c("R", "X")
    solutions
}


# Define server logic
shinyServer(function(input, output) {
    
    output$UserLogistic <- renderPlot({
        
        UserX <- input$X0
        UserR <- input$R
        
        UserTitle <- paste("Population with X[0] =",
                           UserX, "and Growth Rate =",
                           UserR)
        
        UserVector <- LogisticLoop(UserX, UserR, iter=25)
        
        UserMean <- mean(UserVector)
        
        plot(UserVector, xlab = "Iterations",
              ylab = "Population", col = "red",
              main = UserTitle, ylim = c(0,1))
        lines(UserVector, col = "red", lwd = 1)
        abline(h = UserMean, col = "blue")
        

    })
    
    output$StableSolutions <- renderPlot({
        
        if (input$StableButton == 0)
            return()
        
        solutions <- isolate(StableSolutions())
        
        stableMeans <- solutions %>%
            group_by(R) %>%
            summarise(mean(X))
        
        
        plot(solutions, xlab = "Growth Rate",
            ylab = "Population", col = "red",
            pch = ".",
            main = "Stable Solutions of the Logistic Equation")
        lines(stableMeans, col = "blue")
        
    })

})





