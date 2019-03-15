# Consider the data in the accompanying 'crime.csv' file.  We only desire the
# data in those rows that contain state names in capital letters as the first
# elements on the lines.  Without editing the 'crime.csv' file and without
# creating any other files, get the desired data in a data frame called
# 'crime'.  Hint: you should have exactly 50 rows and 35 columns.
# [7 pts.]
allData <- read.csv("crime.csv", stringsAsFactors=FALSE)
states<- grepl("[A-Z]$",allData[,1])
crime <-allData[states,]
crime
# Compute the mean of the third column in the crime data frame.
# [3 pts.]

a<-crime[,3] 
a<-as.numeric(gsub(",", "", a))
mean(a)


