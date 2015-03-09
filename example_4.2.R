example_4.2 <- function()
{
    x <- sample(1:52,52,replace=F)
    y <- sample(1:52,52,replace=F)
    if(any (x == y)) return (1)
    return (0)
}