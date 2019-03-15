

B <- 5000       
df <- numeric(B)

for ( b in 1:B ) {
  x<- trees[sample(nrow(trees),replace = TRUE ), ]
  a<-lm(Volume ~ Height + Girth, data=x)

  df[b] <- a[[1]][["Height"]]/a[[1]][["Girth"]]
}

mean(df)

quantile(df,c(0.025, 0.975))


