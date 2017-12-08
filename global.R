library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), dbname="data.sqlite")
alltables <- dbListTables(con)
wild_fires <- dbGetQuery(con,'select * from Fires where FIRE_YEAR >= 2010')