coupon_problem <- function(n)
{
  #Identifies the average number of trials needed to pick n distinct values from a collection of n distinct items, with replacement
  #Doborow book: page 180/181, example
  
  i <- 0
  j <- 1
  a = vector(length=n)
  a[j] <- sample(1:n,1)
  remaining_coupons <- c(1:n)[-which(c(1:n) == a[j])]
  
  #print(a[j])
  #print(remaining_coupons)
  repeat
    {
      pick <- sample(remaining_coupons,1)
      i <- (i+1)
      
   #   print(pick) 
      
      #if(all(pick %in% a)) 
      if(any(pick == a)) 
      {
       next
      }
      else
      {
        j <- (j+1)
        a[j] <- pick
      }
      
      if(j == n) 
        {
        break
      }
    }
return(i)
}

#Let us suppose that, you draw a certain number of samples, say "t", and you want to check the average number of distinct coupons collected in the sample size of t
#For example, if we have 10 unique coupons, and how many average number of distinct coupons can we expect, if we draw 12 coupons. Here n=10, represents the distinct number of coupons/items
#t represents the current sample size, and our aim is to estimate the average number of items found in sample size of t.

coupon_problem_avg_found <- function(n,t)
{
  #n - represents the total items (distinct)
  #t - represents the total sample size, drawn from items...and our function returns the number of distinct items found in t
  
  x <- sample(1:n,t,replace=T)
  return(length(unique(x)))

}