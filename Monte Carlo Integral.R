# Monte Carlo Integral
# Set function
fx <- function(x){
  y <- x^(-1/3) + x/10
  return(y)
}
px <- function(x){
  y <- 1
  return(y)
}
# Generate points 
n = 5000
t = runif(n)

# Main function
Integral_M <- function(a,b){
  y <- (1/n)*sum(fx(t)/px(t))
  return(y)
}

# Calculate variancnce
Var_M = 0
for (i in 1:5000){
  t <- runif(n)
  M <- (Integral_M(0,1)-1.55)^2
  Var_M <- Var_M + M
  Var_M <- (1/(5000-1))*Var_M
}
Var_M
ans <- Integral_M(0,1)
