Dice_Sim_Atleast_value <- function(f,n,m)
{
  #f = Number of faces f die
  #n = Number of dices
  #m - Minimum value of sum
  if (sum(sample(1:f,n,replace=T)) >= m) 
    p <- 1
  else p <- 0
  
  return(p)
}