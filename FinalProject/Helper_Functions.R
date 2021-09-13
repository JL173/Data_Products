## Helper Functions

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
StableSolutions <- function(n = 5, iter = 1000, tol = 1e-3){
  # n, power of 2 for bifurcations
  # iter, number of test solution pairs squared
  # tol, tolerance
  
  x <- seq(from = 0, to = 1, length.out = iter)
  
  r <- seq(from = 0, to = 4, length.out = iter)
  
  solutions <- data.frame(R = c(), X = c())
  
  for (ri in r){
    
    stable <- (ri - 1)/(ri + 1e-12)    # avoid /0
    
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
