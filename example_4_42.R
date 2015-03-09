example_4_42 <- function(n)
{

ctr <- 0
p <- vector(length=n)
 repeat{
   x <- sample(1:4,1)
   y <- sample(1:x,1)
   
   
   if(y == 1)
   {

     ctr <- (ctr+1)
     p[ctr] <- x     
     if (ctr == n)
     {
       p_1 <- (sum(p == 1) / n)
       p_2 <- (sum(p == 2) / n)
       p_3 <- (sum(p == 3) / n)
       p_4 <- (sum(p == 4) / n)
       return(list(p_1=p_1,p_2=p_2,p_3=p_3,p_4=p_4))
       
     }
   }
 }

}