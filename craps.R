craps <- function()
{
   
  #Here are the game rules: You roll a pair of dice. If you get 7 or 11 (sum), then you win in the first shot
  #If you throw 2 or 3 or 12 in the first shot, you loose
  #If you get any other number in the first trial you will get the pick.
  #In case of pick, whetever number you get in the pick (which is the sum of 2 dice in the first shot and the sum is
  # NOT 7 or 11 or 2 or 3 or 12). If you get pick, then you keep on throwing the dice,
  #Till you either get pick or 7. If you get 7 before pick, then you will loose. If you get pick before 7 then u will 
  # win.
  
  #To call the function, use: mean(replicate(100000,craps()))
  
  throw_1 <- sum(sample(1:6,2,replace=T))
  if(throw_1 == 7 || throw_1 == 11) return(1)
  if(throw_1 == 2 || throw_1 == 3|| throw_1 == 12) return(0)
  repeat
  {
    throw_2 <- sum(sample(1:6,2,replace=T))
    if(throw_2 == 7) return(0)
    if(throw_2 == throw_1 ) return(1)
  }
    
}