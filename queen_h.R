queen_h <- function()
{
  #pick a sample 0, no hemophilia for queen, 1 for hemophilia
 q_h <- sample(0:1,1)
 
 repeat
 {
 if(q_h == 1)
 {
   p_h <- sample(0:1,3,replace=T)
   if(sum(p_h) == 0)
   {
     return(1)
   }
   else return(0)
 }
 q_h <- sample(0:1,1)
 }

}