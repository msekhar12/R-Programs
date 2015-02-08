
library("gtools")
#You may need to install gtools using install.packages("gtools") command

problem_1_19 <- function(n,k)
{

  #n = Sequence of numbers
  #k = Numbers to be choosen from n
  
  p <- permutations(n,k,1:n)
  j <- vector(length=nrow(p))
  
  for(i in 1:nrow(p))
  {
    if(all(diff(p[i,]) > 0))
    {

      j[i] <- 1
    }
    else j[i] <- 0
      
  }
  return(list(Number_of_Ascending_values=sum(j),Permutations=nrow(p),prob=sum(j)/nrow(p)))
  
}