who_wins_first <- function(P,N)
{
  #Two people try to toss the same coin.
  #The first person, say F starts tossing first, followed by the second person S. Who ever gets N number of heads first, will win
  #P represents the probability of getting Heads

  #To call the function 10000 times, and find the probability that the first person wins, use the following function call
  #mean(replicate(10000,who_wins_first(0.2,5)))
  
  #HF represents the outcome when first person tosses the coin first
  HF <- 0
  HS <- 0
  repeat
    {
       #0 represents tails, 1 represents H, in the following sample function
       F <- sample(0:1,1)
       if (F == 1)
       {
         HF <- (HF + 1)
         if (HF == N) 
           {
           break
           }
       
       }

    S <- sample(0:1,1)
    if (S == 1)
    {
      HS <- (HS + 1)
      if (HS == N) 
      {
        break
      }
      
    }
}
if(HS == HF) return(-1)
if(HS > HF) return(0)
if(HS < HF) return(1)
}