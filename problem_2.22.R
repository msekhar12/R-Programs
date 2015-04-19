problem_2.22 <- function()
{
  repeat
  {
    
    x <- sample(0:1,2,rep=T)
    if (x[1] == 1)
    {
      if(x[2] == 0)
      {
        return(1)
      }  
      
      if(x[2] == 1)
      {
        return(0)
      }
        
    }
    
  }
    
}