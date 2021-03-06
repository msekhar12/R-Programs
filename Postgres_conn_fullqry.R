
library("RPostgreSQL")
library("sqldf")


## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## Open a connection
#con <- dbConnect(drv, dbname="flights")

con <- dbConnect(drv, dbname="flights",host="localhost",port=5432,user="postgres",password="PEPSI@cola27")

## Submits a statement
rs <- dbSendQuery(con, "select avg(dep_delay) as dep_delay, avg(arr_delay) arr_delay, carrier from flights group by carrier order by 2 desc"
)

## fetch all elements from the result set
flight_delays <- fetch(rs,n=-1)

## Submit and execute the query
dbGetQuery(con, "select * from R_packages")

## Closes the connection
dbDisconnect(con)

## Frees all the resources on the driver
dbUnloadDriver(drv)