#e - No. of employees
#s - No. of supervisors

Sim_01 <- function(s,e)
{
  j <- 0;
  for(i in 1:1000)
  {
     r <- sample(1:(s+e),2,rep=FALSE);
     if ((r[1] <= s && r[2] >= s && r[2] <= (s+e)) || (r[2] <= s && r[1] >= s && r[1] <= (s+e)) )
     {
        j <- j + 1;
     }

  }

return(j/1000);
  
}

