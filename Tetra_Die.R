Tetra_Die <- function(a,n)
{
  #a - minimum value
  #n - Number of dice
  
  if(any(sample(1:4,n,replace=T) == a )) return(1)
  else return(0)
}