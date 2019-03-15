ci <- function(x, sd=NA, confidence.level=0.95){
  n <- length(x)
  a <- (1-confidence.level)/2
  if (is.na(sd)){             #sd is not provided 
    lower <- mean(x)-(qt(a, df=(n-1),lower.tail=FALSE)*(sd(x)/(sqrt(n))))
    higher <- mean(x)+(qt(a, df=(n-1),lower.tail=FALSE)*(sd(x)/(sqrt(n))))
    
  } else {
    lower <- mean(x)-(qnorm(a,lower.tail=FALSE)*(sd/sqrt(n)))
    higher <- mean(x)+(qnorm(a,lower.tail=FALSE)*(sd/sqrt(n)))
  }
  interval <- c(lower, higher)
  names(interval) <- c("Lower","Upper")
  interval
} 

nReps <-10000
p <- .4
nTacks <- replicate(nReps, ci(rnorm(50,0,1)))
contain <- numeric(nReps)
for (i in 1:nReps) {
  contain[i] <- nTacks[1,i] < p && nTacks[2,i] > p 
}
coverage <- mean(contain)
coverage
coverage + c(-1,1)*qnorm(0.975)*sqrt(coverage*(1-coverage)/nReps)  #monte carlo error






