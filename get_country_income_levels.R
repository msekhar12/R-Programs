#Install the XML Package, if needed
#install.packages("XML")

#This function will parse the data from the website: "http://www.iawp.org/joiniawp/countrylist.htm"
#and creates a file which can be loaded into a postgres table.

library("XML")



u <- "http://www.iawp.org/joiniawp/countrylist.htm"
tbls <- readHTMLTable(u,which=1,colClasses=c("integer","character","character","character","character"))
#We are interested in the following rows and columns only
tbls[6:213,c(3,4,7)]

#Cleanse the data 
tbls$class <- ifelse(grepl("Lower middle",tbls[,7]),'Lower Middle',tbls[,7])
tbls$class <- ifelse(grepl("High income",tbls[,7]),'High Income',tbls$class)
tbls$class <- ifelse(grepl("Upper middle",tbls[,7]),'Upper Middle',tbls$class)
tbls$class <- ifelse(grepl("Low income",tbls[,7]),'Low',tbls$class)
tbls[6:213,c(3,4,7,10)]

#Move the desired columns to another data frame
tbls_mod <- tbls[6:213,c(3,4,10)]
names(tbls_mod) <- c("Country", "Country_Code","Income Classification")

#Clean any unnecessary commas in col1
tbls_mod[,1] <- gsub("\\, The","",tbls_mod[,1])
tbls_mod[,1] <- gsub("\\,","",tbls_mod[,1])
write.csv(tbls_mod,file = "income_levels.csv", quote=F,row.names=F)