Doborow_1_37 <- function(n)
{
  y <- vector(length=n)
  for(i in 1:n)
  {  
  x <- sample(1:5000,1)
  if(x %% 4 == 0 || x %% 7 == 0 || x%% 10 == 0) 
  {
    y[i] <- 1
  }
  else y[i] <- 0
  }
  return(mean(y))
}