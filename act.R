a <- read.table("act.txt",header=TRUE)

class1 <-a[1:20,1:2]
class2 <-a[21:50, 1:2]


permutation <-function(class){
  
obs <- mean(class$post) - mean(class$pre)
num <- numeric(10000)

for ( i in 1:length(num) ) {
  x <- class
  for ( j in 1:nrow(class)) {
    x[j,] <- sample(x[j,])
  }
  num[i] <- mean(x$post) - mean(x$pre)
}

p.value <- mean(num >= obs)
info<-c(obs, p.value)
names(info)<-c("observed", "pvalue")
info
}


permutation(class1)
permutation(class2)

