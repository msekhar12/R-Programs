problem_3_36 <- function(n)
  
{

  #Doborow problem 3.36
  
  #0 - red
  #1 - blue
  #r - represent Red pill
  #b - represent Blue pill 
  r <- 0
  b <- 0
  i <- 0
  
  repeat
  {
    pill <- sample(0:1,1)  
  
  if(pill == 0)
    {
      c <- rpois(1,1)
      if (c == 3)
      {
        r <- (1+r)
        i <- (i +1)
      }
    }
  
  if(pill == 1)
  {
    c <- rpois(1,4)
    if (c == 3)
    {
      b <- (1+b)
      i <- (i +1)
    }
  }

  if(i >n ) break
  }
  
  return (list(r=r,b=b,p_r=r/n,p_b=b/n))
  
}