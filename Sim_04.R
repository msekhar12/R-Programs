#Sim of page 282, 1(c) problem, to check if the event is dependent/independent
Sim_04 <- function(p,w,t)
{
  #p - probability of win on any game
  #t - number of trails
  #w - wins in a row (current wins in a row).
  # If w wins were made, check what is the probability of next win (w+1)th win.
  # If (w+1)th wins in t trails is same as p, then we can conclude that p is independent of previous wins
 
  x <- vector(length=(w+1))
  y <- vector(length=t)
  j <- 1
  repeat
  {
    x <- sample(0:9,(w+1),rep=T)
    if (all(x[1:w] > 0))
    { 
      if (x[w+1] > 0) y[j] <- 1
      else y[j] <- 0
      
      j <- j + 1
    }
    if(j == t) break
  }

return(data.frame("Initial.Prob"=p, "Final.Prob"=(sum(y)/t)))

}