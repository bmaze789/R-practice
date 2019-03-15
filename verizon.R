ILEC <- c(1,1,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,2,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,3,1,1,1,1,2,4,1,1,1,1,2,5,1,1,1,1,2,5,1,1,1,1,2,6,1,1,1,1,2,8,1,1,1,1,2,15,1,1,1,2,2) 

CLEC <- c(1,1,5,5,5,1,5,5,5,5) 

# Test statistic of ratio of variances with p-value (given by the hw question)
varCLEC <- var(CLEC)
varILEC <- var(ILEC)

N1 <- length(CLEC)
N2 <- length(ILEC)

observed.test.statistic <- varCLEC/varILEC
pf(observed.test.statistic,N1-1,N2-1,lower.tail=FALSE) 

num <- 10000            
copy <- numeric(num)
for ( i in 1:length(copy) ) {
 # a <- sample(CLEC, replace = TRUE)
  b <- sample(ILEC, replace = TRUE)
  copy[i] <- var(CLEC)/var(b)  
}
p.value <- mean(copy <= observed.test.statistic)
p.value


#The pvalue is 0.43 which means the probablity of finding the observed or more extreme is likely
#The Ftest is close to being valid but it's not perfect