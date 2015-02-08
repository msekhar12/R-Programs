
library("RPostgreSQL")
library("sqldf")


## loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")

## Open a connection
#con <- dbConnect(drv, dbname="flights")
con <- dbConnect(drv, dbname="flights",host="localhost",port=5432,user="postgres",password="PEPSI@cola27")

## Submits a statement
rs <- dbSendQuery(con, "select a.dep_delay, a.year, a.month, a.day,a.hour, b.temp,
dewp,
humid,
wind_dir,
wind_speed,
wind_gust,
precip,
pressure,
visib
from flights a,
weather b 
where a.year = b.year and a.month = b.month and a.day = b.day and a.hour = b.hour
")

## fetch all elements from the result set
flight_delays <- fetch(rs,n=-1)

## Submit and execute the query
dbGetQuery(con, "select * from R_packages")

## Closes the connection
dbDisconnect(con)

## Frees all the resources on the driver
dbUnloadDriver(drv)