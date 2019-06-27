# Set function
fx<- function(x){
  y<- x^3-x-1
  return(y)
}

# Main function
a<-0 # number of times
x<- 1 # start point
y<- 1 # start point
repeat{
  x <- y
  y <- x-(fx(x)/grad(fx,x))
  a<- a+1
  if (abs(x-y)<(10^(-6))) break
}
print(x)
