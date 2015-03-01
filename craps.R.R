craps <- function(n)
{
  #n - Number of desired trials
  #Here are the game rules: You roll a pair of dice. If you get 7 or 11 (sum), then you win in the first shot
  #If you throw 2 or 3 or 12 in the first shot, you loose
  #If you get any other number in the first trial you will get the pick.
  #In case of pick, whetever number you get in the pick (which is the sum of 2 dice in the first shot and the sum is
  # NOT 7 or 11 or 2 or 3 or 12). If you get pick, then you keep on throwing the dice,
  #Till you either get pick or 7. If you get 7 before pick, then you will loose. If you get pick before 7 then u will 
  # win.
  
  p <- vector(length=n)
  pick <- vector(length=1)
  throw <- vector(length=1)
  for (i in 1:n)
  {
    d1 <- sample(1:6,1)
    d2 <- sample(1:6,1)
    throw <- (d1 + d2)
    if (throw == 7 || throw == 11) 
      {
        p[i] <- 1
        break
    }
      if (throw == 2 || throw == 3 || throw == 12) 
        {
        p[i] <- 0
        break
      }
      pick <- throw    
    
    repeat
      {
      d1 <- sample(1:6,1)
      d2 <- sample(1:6,1)
      throw <- (d1+d2)
      if(throw == 7) 
        {
        p[i] <- 0
        break
      }
      if(throw == pick)
      {
        p[i] <- 1
        break
      }
      
    }
    
    
    
  }
  return(mean(p))

}