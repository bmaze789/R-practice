 
MakeADeal <-function(change_door){

doors <- c(1:3L)
prize <- sample(1:3, 1)
firstChoice <- sample(1:3,1)

x <-(doors[which(doors!=prize & doors!=firstChoice)]) #choosing door to remove
  if (length(x)>1){
  y <- sample(x,1)
  removed_door<-doors [(-y)]    #removes door
  } else{
    removed_door<-doors [(-x)]
  }
#############################

  if (change_door == "yes"){
  secondChoice <-removed_door[removed_door!=firstChoice] #chooses door that is different
  } else {
    secondChoice <-firstChoice #stays with same door
  }
prize == secondChoice
}


nReps=10000
switching <- mean(replicate(nReps, MakeADeal("yes")))
Not_switching <- mean(replicate(nReps, MakeADeal("no")))
switching
Not_switching



# Computing the CI
z = qnorm(1-.95/2)
switching + c(-1,1)*z*sqrt(switching*(1-switching)/nReps)
Not_switching + c(-1,1)*z*sqrt(Not_switching*(1-Not_switching)/nReps)
