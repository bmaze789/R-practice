probablity <- function(n){
  
  x <- numeric(n)  
  for (i in 1:n){
    q <- 1 - (0:(i - 1))/365  
    x[i] <- 1 - prod(q)  
  }
  x[n]
}

birthday <- function(n, nReps){
     
  trials <- replicate(nReps, probablity(n))
  est <- mean(trials)
    
  CI <- est + c(-1,1) * qt(1-0.05/2,df=nReps-1) * sd(x)/sqrt(nReps)
    
  est_CI <- c(est, CI)
  est_CI
}

points<- sapply(1:65, function(i) probablity(i)) 
plot(points,xlab ="Number of People", ylab = "Probability of Sharing", type="l", col="red")  


bir



estPoints<- sapply(1:65, function(i) birthday(i,10000)) 
plot(z,xlab ="Number of People", ylab = "Probability of Sharing", type="l", col="black")  
z<-estPoints[2,]

