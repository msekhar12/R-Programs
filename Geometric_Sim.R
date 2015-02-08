Geometric_Sim <- function(p)
{
 chance <- vector(length=1000);
 prob <- vector(length=1000);
 for(i in 1:1000)
 {
  r <- sample(1:100,100,rep=TRUE);
  n <- which(r <= p*100)[1];
  chance[i] = n;
  prob[i] = (1-p)^(n-1) * p;
  }  
  return(data.frame(chance=chance,prob=prob)); 

}