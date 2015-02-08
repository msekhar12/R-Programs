Envelope_Game <- function()
{
  #Draw a random sample of 4, and assign the numbers to envelopes
  Envelopes <- sample(1:4,4,replace=F)
  
  #Get the index where the "number 4" is placed in x
  #The place where the number 4 is placed, will contain the 100$ bill
  best <- which(Envelope == 4)
  Envelope[best] <- 100
  #Put 0$ in all other envelopes
  Envelope[-best] <- 0
  
  #Now the player randomly picks an envelope
  pick <- sample(1:4,1)
  
  #Check if he picked the envelope which has 100$. 
  #Since our idea is to calculate the probability when the user swaps the envelope
  #at the end, we have to assume that he will fail, since he exchanges the envelope
  if(best == pick) return(0)
  
  #If the user picked an envelope, which has 0$, and if he exchanges the envelope at the end,
  # then he will get 100$
  if(best != pick) return(1)
    
}