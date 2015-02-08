Sim_02 <- function(questions,students,min_per)
{
  prob <- vector(length=1000)
for(l in 1:1000)
{  

  ans <- sample(1:5,questions,rep=TRUE);
#  print(ans)
  stud_ans <- sample(1:5,questions*students,rep=TRUE);
#  print(stud_ans)
  stud_corr_ans <- vector(length=questions);
  stud_corr_ans <- rep(0,students);
#  print(stud_corr_ans)

  j <- 1;
  k <- 1;

  for(i in 1:(questions*students))
  {
    if(j > questions) 
    { 
      j <- 1;
      k <- (k+1);
    }
       
    if(stud_ans[i] == ans[j]) stud_corr_ans[k] <- (stud_corr_ans[k] + 1);
    j <- (j+1);
  }
#print(stud_corr_ans[stud_corr_ans/questions >= (min_per/100)])
print("-**-")
print(length(stud_corr_ans[stud_corr_ans/questions >= (min_per/100)]))
print("-**-")
prob[l] <- length(stud_corr_ans[(stud_corr_ans/questions) >= (min_per/100)])

}
#return(stud_corr_ans);
return(prob)
}