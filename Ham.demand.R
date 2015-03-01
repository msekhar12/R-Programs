#606 PROJECT 1
sales_1 <- read.csv(file="sales.csv", header=TRUE, sep=",")
library("lubridate")

sales_1$wday <- wday(ymd(sales$date),label=T)

sales_1

#To get probabilities, irrespective of the week day:
data.frame(prop.table(table(sales_1$demand.ham)))

prop.table(table(sales_1$demand.ham,sales_1$wday))

hist(prop.table(table(sales_1$demand.ham,sales_1$wday))[,2])

cumsum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,2])
summary(prop.table(table(sales_1$demand.ham,sales_1$wday))[,2])

#The probability of week day can be found like this. This is needed to find the cond prob.
sum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,2])
sum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,3])
sum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,4])
sum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,5])
sum(prop.table(table(sales_1$demand.ham,sales_1$wday))[,6])

#To get the conditional probability by day (the following command will get the probability of ham demand, given the day as friday):
#P(ham.demand = x | day = FRIDAY) = P(DAY=FRIDAY AND ham.demand = x) / P(DAY = FRIDAY)
#where X is the demand number. For instance, if X = 11, then P(DAY = FRIDAY AND X=11) = 0.015384615
#P(FRIDAY) = 0.2. Therefore P(demand.ham = 11 | FRIDAY) = 0.015384615/0.2

prop.table(table(sales_1$demand.ham,sales_1$wday))[,6]/0.2

#To find all the conditional probabilities, cond on week day:
prop.table(table(sales_1$demand.ham,sales_1$wday))/0.2

data.frame((prop.table(table(sales_1$demand.ham,sales_1$wday))/0.2))

#To find marginal probabilities, use the following command. This will find the marginal probabilities horizintally for
#prop.table(table(sales_1$demand.ham,sales_1$wday))

margin.table(prop.table(table(sales_1$demand.ham,sales_1$wday)),1)

#The following command will find the marginal probabilities vertically
margin.table(prop.table(table(sales_1$demand.ham,sales_1$wday)),2)

#Let us convert the following command's o/p to dataframe:
#prop.table(table(sales_1$demand.ham,sales_1$wday))/0.2
#NOTE That the prob column in the following column, is the conditional probability of demand, given a week day.
week_day_prob <- data.frame((prop.table(table(sales_1$demand.ham,sales_1$wday))/0.2),stringsAsFactors=F)

#Rename the column names
names(week_day_prob) <- c("demand","week_day","prob")

#To verify, we have conditional probabilities based on a week day
sum(week_day_prob[week_day_prob$week_day == "Mon",]$prob)
sum(week_day_prob[week_day_prob$week_day == "Tues",]$prob)
sum(week_day_prob[week_day_prob$week_day == "Wed",]$prob)
sum(week_day_prob[week_day_prob$week_day == "Thurs",]$prob)
sum(week_day_prob[week_day_prob$week_day == "Fri",]$prob)

#Let us add another variable expected_val
# problem here...but resolved by using "levels" in as.numeric
week_day_prob$expected_val <- ( as.numeric(levels(week_day_prob$demand)) * (week_day_prob$prob))

#Number of ham burgers needed on a week day
sum(week_day_prob[week_day_prob$week_day == "Mon",]$expected_val)
sum(week_day_prob[week_day_prob$week_day == "Tues",]$expected_val)
sum(week_day_prob[week_day_prob$week_day == "Wed",]$expected_val)
sum(week_day_prob[week_day_prob$week_day == "Thurs",]$expected_val)
sum(week_day_prob[week_day_prob$week_day == "Fri",]$expected_val)

#Round
round(sum(week_day_prob[week_day_prob$week_day == "Mon",]$expected_val))
round(sum(week_day_prob[week_day_prob$week_day == "Tues",]$expected_val))
round(sum(week_day_prob[week_day_prob$week_day == "Wed",]$expected_val))
round(sum(week_day_prob[week_day_prob$week_day == "Thurs",]$expected_val))
round(sum(week_day_prob[week_day_prob$week_day == "Fri",]$expected_val))


#######################################################
##Now finding the probability by ignoring the week day#
#######################################################
data.frame(table(sales_1$demand.ham))
all_prob <- data.frame(prop.table(table(sales_1$demand.ham)))
names(all_prob) <- c("demand","freq")
sum(all_prob$demand * all_prob$freq)

#######################################################
#Let us evaluate the profit
#######################################################

#Adding a new variable
#new_avail_cond_by_wday
sales_1$new_avail_cond_by_wday <- 0
sales_1$new_avail_cond_by_wday[sales_1$wday=="Mon"] <- round(sum(week_day_prob[week_day_prob$week_day == "Mon",]$expected_val))
sales_1$new_avail_cond_by_wday[sales_1$wday=="Tues"] <- round(sum(week_day_prob[week_day_prob$week_day == "Tues",]$expected_val))
sales_1$new_avail_cond_by_wday[sales_1$wday=="Wed"] <- round(sum(week_day_prob[week_day_prob$week_day == "Wed",]$expected_val))
sales_1$new_avail_cond_by_wday[sales_1$wday=="Thurs"] <- round(sum(week_day_prob[week_day_prob$week_day == "Thurs",]$expected_val))
sales_1$new_avail_cond_by_wday[sales_1$wday=="Fri"] <- round(sum(week_day_prob[week_day_prob$week_day == "Fri",]$expected_val))

sales_1$cond_prob_profit <- (ifelse((sales_1$demand.ham <= sales_1$new_avail_cond_by_wday), ((sales_1$demand.ham * 6.50) - (sales_1$new_avail_cond_by_wday * 3.5)),
                             ((sales_1$new_avail_cond_by_wday * 6.50) - (sales_1$new_avail_cond_by_wday * 3.5)))
)


print(sum(sales_1$cond_prob_profit))

