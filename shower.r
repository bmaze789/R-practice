roommate <- c(6,6.25,5.75,6.5,6,15,35,20,4,5.25,5.75)

you <- c(3.5,5.5,4,4,4,6,11,12,3,4,4.25,7,3.25)

t.test(roommate,you,conf.level=0.9)


B <- 10000      
difference <- numeric(B)


for ( b in 1:B ) {
  rm <- sample(roommate, replace=TRUE)
  you <- sample(you, replace=TRUE)
  difference[b] <- (mean(rm) - mean(you))
}

ci <- quantile(difference, probs=c(0.05, 0.95))
ci


# The interval is about (2.4, 11.5) so we can conclude that 90% the true mean falls
# in this interval which means the roommate is wrong and there is a difference in time.


