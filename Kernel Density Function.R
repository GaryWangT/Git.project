# Name data samples and check 
hist(samples)

# Set Normal being the kernel function 
kx <- function(u){
  y <- (1/((2*pi)^(1/2)))*exp(-u^2/2)
} 

# Main function
KDE <- function(n,b,x){
  y <- (1/(n*b))*sum(kx((x-samples)/b))
  return(y)
}

ans=c()
x<- runif(2000,-20,20)
for (i in 1:2000){
  a<-KDE(2000,5,x[i])
  ans<-c(ans,a)
}
plot(x,ans)
