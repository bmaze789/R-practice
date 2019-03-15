
#What is the mean number of boxes that a consumer must purchase to get a complete set?

simulate <- function(n) {

y <- 0
prize <- numeric()
 
while (length(prize) < n) {
  x <- sample(c(1,2,3,4),1, prob=c(.1,.25,.25,.4), replace=TRUE)
  
  if(is.element(x, prize)==FALSE){
  prize <- append(prize, x) 
  }
  y <- y+1
}
 
y
}

#Monte Carlo to find mean
n <- 4
nReps <- 10000
boxes <- replicate(nReps, simulate(n))
total <- mean(boxes)
total

#Monte Carlo error
CI <- total + c(-1,1) * qt(1-0.05/2,df=nReps-1) * sd(boxes)/sqrt(nReps)
CI

#What proportion of consumers will need to purchase 14 boxes or more to complete a set?

est <- mean(boxes >= 14)
est




