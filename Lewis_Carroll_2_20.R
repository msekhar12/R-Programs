Lewis_Carroll_2_20 <- function(n)
{
    
  trials <- 0
  #Probability vector, containing successes and failures.
  #1 - represents white
  #0 - represents black
  p <- vector(length=n)
  bag <- vector(length=2)
  repeat{
  
    bag[1] <- sample(0:1,1)
    bag[2] <- 1
    
    pick <- sample(1:2,1)
    if(bag[pick] == 1) #white is picked
    {
      trials <- (trials + 1)
      next_pick <- ifelse(pick == 1,2,1)
      if(bag[next_pick] == 1) p[trials] <- 1
      else p[trials] <- 0
    }
    
    if(trials == n) break
    
  }
  #return the probability
  return(mean(p))
}