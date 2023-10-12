library(DBI)
library(RPostgres)
library(dplyr)
library(dbplyr)
library(readr)

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "tweets",
  host = "dapr.psu.edu",
  port = 5432,
  user = "api",
  password = "d@pru53r"
)

# find the tables in the database
tables <- dbListTables(con)
#list the fields
dbListFields(con,tables[2])

## get the legislator tweets sample
legis_twts <- dbGetQuery(con, "SELECT * FROM legislator_tweets WHERE created_date BETWEEN '2021-01-01' AND '2021-01-31'")


#save
saveRDS(legis_twts, "legis_twts.rds")


#start_tweets = "2021-01-01T00:00:00Z",
#end_tweets = "2021-02-01T00:00:00Z"

#query <- paste("SELECT * FROM legislator_tweets WHERE created_date BETWEEN '2021-01-01' AND '2022-12-31' AND text LIKE '%@AndresRomeroNM%'", handle_or, ";", sep  = "")

#run 
#twts_endo <- dbGetQuery(con,query)