Bertrands_Box <- function(n)
{
  #the input parm "n" represents the number of trials needed
  #Let us assue that we have 3 chests, each containing 2 drawers
  #Arranged one after the other. 
  #We can imagine this as a 2X3 matrix
  
  #We first get 3 random numbers (for chests)
  #next we get 2 random numbers (for drawers within the chests)
  
  #Then we will assign the Gold coins, and Silver coins to each drawer.
    
  trials <- 0
  #Probability vector, containing successes and failures.
  
  p <- vector(length=n)
  
  #The chests matrix represent a drawer at each column and row
  #The 3 columns represent each of the three chests
  
  chests <- matrix(nrow=2,ncol=3)
  
  repeat{
  
  #select 3 random numbers, and place into the vector "col"
  col <- sample(1:3,3,replace=F)
  
  #select 2 random numbers and place into the vector "row"
  
  row <- sample(1:2,2,replace=F)
  
  
  #Place Gold coins in the random chest and random drawer selected in col[1] and row[1], and col[1] and row[2]
  chests[row[1],col[1]] <- "G"
  chests[row[2],col[1]] <- "G"
  
  #Place Gold coin in the random chest and random drawer selected in col[2] and row[1], and Silver coin at col[2] and row[2]
  chests[row[1],col[2]] <- "G"
  chests[row[2],col[2]] <- "S"
  
  #Place Silver coins in the random chest and random drawers selected in col[3] and row[1], col[3] and row[3]
  chests[row[1],col[3]] <- "S"
  chests[row[2],col[3]] <- "S"
  
  #Till now we randomly placed Gold coins in each of the 2 drawers of 3 chests.
  
  #Let us pick a chest and drawer within that chest randomly
  
  pick_chest <- sample(1:3,1)
  pick_chest_drawer <- sample(1:2,1)
  
  #Check if the picked drawer contains a Gold coin, if yes, then continue checking the coin in the other drawer in the same chest
  if(chests[pick_chest_drawer,pick_chest] == "G")
  {
    trials <- (trials + 1)
    #pick the other drawer in the same chest
    other_drawer <- ifelse (pick_chest_drawer == 1,2,1)
    #Now check if the other drawer in the same chest has a Gold coin
    if(chests[other_drawer,pick_chest] == "G") p[trials] <- 1
    else p[trials] <- 0
  }
  
  #After successfully conducting n trials, let us break the repetition
  if (trials == n) break
  #print(chests[,pick])
  
  }
  #return the probability
return(mean(p))
}