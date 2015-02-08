
# install.packages("RPostgreSQL")
# Establish connection to PoststgreSQL using RPostgreSQL
Postgre_Tab_To_Data_Frame <- function(database,tabname)
{
library("RPostgreSQL")
library("sqldf")
drv <- dbDriver("PostgreSQL")
#con <- dbConnect(drv)# Simple version (localhost as default)
# Full version of connection seetting
con <- dbConnect(drv, dbname="flights",host="localhost",port=5432,user="postgres",password="PEPSI@cola27")

flights_1 <- dbReadTable(con, c("public",tabname))
# Equals to myTable <- sqldf("select * from tmp.test_tbl")
#flights <- sqldf("select * from public.flights")
head(flights_1)
#At the end of you script, do not forget to close the connection:
  # Close PostgreSQL connection 
  dbDisconnect(con)

}