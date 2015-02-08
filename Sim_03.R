#Simulation of problem(a) on page 242 0f AP Stats

Sim_03 <- function(prob,trails,max_cars)
{
 p <- vector(length=trails)
 for(i in 1:trails)
 {
  r <-  sample(1:100,max_cars,rep=T)
  p[i] <- (length(r[r<=14])/max_cars)

 }

return(p)

}