#606 PROJECT 1
sales <- read.csv(file="sales.csv", header=TRUE, sep=",")
library("lubridate")

#To view a data.frame in excel format, use "fix(sales)" command

#fix(sales)

sales$ham.profit.actual <- ifelse(sales$demand.ham >= sales$available.ham, 
                                  ((sales$available.ham * 6.50) - 
                                     (sales$available.ham * 3.50)), 
                                  (sales$demand.ham * 6.50) - 
                                    (sales$available.ham * 3.50))


sales$ham.profit.missed <- ifelse(sales$demand.ham > sales$available.ham, 
                                  ((sales$demand.ham - sales$available.ham) * 6.50), 
                                  0)


sales$ham.non_sale_loss <- ifelse(sales$demand.ham < sales$available.ham, 
                                  ((sales$available.ham - sales$demand.ham) * 3.50), 
                                  0)


sales$turkey.profit.actual <- ifelse(sales$demand.turkey >= sales$available.turkey, 
                                     ((sales$available.turkey * 6.50) - 
                                        (sales$available.turkey * 4)), 
                                     (sales$demand.turkey * 6.50) - 
                                       (sales$available.turkey * 4))


sales$turkey.profit.missed <- ifelse(sales$demand.turkey > sales$available.turkey, 
                                     ((sales$demand.turkey - sales$available.turkey) * 6.50), 
                                     0)

sales$turkey.non_sale_loss <- ifelse(sales$demand.turkey < sales$available.turkey, 
                                     ((sales$available.turkey - sales$demand.turkey) * 4.0), 
                                     0)


sales$veggie.profit.actual <- ifelse(sales$demand.veggie >= sales$available.veggie, 
                                     ((sales$available.veggie * 5) - 
                                        (sales$available.veggie * 2.5)), 
                                     (sales$demand.veggie * 5) - 
                                       (sales$available.veggie * 2.5))

sales$veggie.profit.missed <- ifelse(sales$demand.veggie > sales$available.veggie, 
                                     ((sales$demand.veggie - sales$available.veggie) * 5), 
                                     0)


sales$veggie.non_sale_loss<- ifelse(sales$demand.veggie < sales$available.veggie, 
                                     ((sales$available.veggie - sales$demand.veggie) * 2.5), 
                                     0)



sales$month <- month(ymd(sales$date))
sales$day <- day(ymd(sales$date))
sales$wday <- wday(ymd(sales$date))
#sales
#sales_monday <- sales[sales$wday == 2,]
#sales_tuesday <- sales[sales$wday == 3,]
#sales_wednesday <- sales[sales$wday == 4,]
#sales_thursday <- sales[sales$wday == 5,]
#sales_friday <- sales[sales$wday == 6,]

#demand.ham.avg <- vector(length=nrow(sales_monday))
#dpois.ham <- vector(length=nrow(sales_monday))
#qpois.ham <- vector(length=nrow(sales_monday))
#ppois.ham_rev <- vector(length=nrow(sales_monday))

#for(i in 1:nrow(sales_monday))
#{
#  demand.ham.avg[i] <- sum(sales_monday$demand.ham[1:i])/i
#  #print(sales$demand.ham)
  
#  qpois.ham[i] <- qpois(0.6, demand.ham.avg[i],lower.tail = F) 
#  
#  
#}

#print(demand.ham.avg - sales_monday$demand.ham ) 
#print(qpois.ham) 

#Analyzing complete, not just for monday
#This looks promising, but 

demand.ham.avg <- vector(length=nrow(sales))
dpois.ham <- vector(length=nrow(sales))
qpois.ham <- vector(length=nrow(sales))
ppois.ham_rev <- vector(length=nrow(sales))

j <- 0

for(i in 1:nrow(sales))
{
   demand.ham.avg[i] <- sum(sales$demand.ham[1:i])/i
  }
    
  demand.ham.avg[i] <- sum(sales$demand.ham[1:i])/i
  
  
  #print(sales$demand.ham)
  #say summary(sales) will display the info
  #the demand.ham has the 1st q at 13.25
  #3rd quat at 18.75
  #diff of quats = 5.5
  #1st quat - 5.5 = 13.25 -5.5 = 7.75
  #3rd quat + 5.5 = 18.75 + 5.5 = 24.25
  #so we will consider any demand <= 8 and >= 24 as outliers
  
  qpois.ham[i] <- qpois(0.6, demand.ham.avg[i],lower.tail = T) 
  
  
}

print(demand.ham.avg - sales$demand.ham ) 
print(qpois.ham) 
print(sales$demand.ham)

print(qpois.ham - sales$demand.ham)
print(sales$available.ham - sales$demand.ham)

length(qpois.ham[(qpois.ham - sales$demand.ham) > 0] )
hist(qpois.ham, breaks = 5)
dummy <- data.frame(qpois=qpois.ham,demand.ham=sales$demand.ham,available.ham=sales$available.ham,demand.ham.avg = demand.ham.avg)
print(dummy)




sales$demand.ham.mod <- ifelse((sales$demand.ham >= 8 & sales$demand.ham <= 24), sales$demand.ham,0)

j <- 0
sales$demand.ham.avg <- sales$availability.ham

for(i in 2:nrow(sales))
{
#  if(sales$demand.ham[i] == 0) 
#    { 
#    j <- (j+1) 
#     
 #   }
  
  sales$demand.ham.avg[i] <- sum(sales$demand.ham[1:i-1])/(i - 1 - j)
  sales$demand.ham.avg_1[i] <- ((sales$demand.ham.avg[i] * 0.60) + (sales$demand.ham[i-1] * 0.40))
  qpois.ham$avg[i] <- qpois(0.5, sales$demand.ham.avg[i],lower.tail = F) 
  qpois.ham$avg_1[i] <- qpois(0.5, sales$demand.ham.avg_1[i],lower.tail = F) 
}

#qpois.ham <- data.frame(sales$demand.ham,sales$available.ham)
qpois.ham$avg[1] <- 14
qpois.ham$avg_1[1] <- 14
qpois.ham$profit.actual <- ifelse(sales$demand.ham >= sales$available.ham, 
                                  ((sales$available.ham * 6.50) - 
                                     (sales$available.ham * 3.50)), 
                                  (sales$demand.ham * 6.50) - 
                                    (sales$available.ham * 3.50))




qpois.ham$avg_pro <- ifelse((qpois.ham$sales.demand.ham >= qpois.ham$avg),
                            (6.5 * qpois.ham$avg) - (3.5 * qpois.ham$avg),
                            (6.5 * qpois.ham$sales.demand.ham) - (3.5 * qpois.ham$avg))

qpois.ham$avg_1_pro <- ifelse((qpois.ham$sales.demand.ham >= qpois.ham$avg_1),
                              (6.5 * qpois.ham$avg_1) - (3.5 * qpois.ham$avg_1),
                              (6.5 * qpois.ham$sales.demand.ham) - (3.5 * qpois.ham$avg_1))