library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), dbname="data.sqlite")
alltables <- dbListTables(con)
wild_fires <- dbGetQuery(con,'select * from Fires where FIRE_YEAR >= 2010')
wild_fires <- mutate(wild_fires,
  discovery_date = 
    as.Date(wild_fires$DISCOVERY_DATE-2451545, origin='2000-01-01'), 
  cont_date = as.Date(wild_fires$CONT_DATE-2451545, origin='2000-01-01'))

bthayill_315_theme <- theme_grey() +
  theme(axis.text = element_text(size = 9, color = "violetred4"),
        axis.line = element_line(color = "black"),
        text = element_text(size = 11, face = "bold.italic", 
                            color = "darkslateblue"))

cb_palette <- rev(c("#430A4B", "#1D5076", "#008781", "#64AE68", "#DBC363", "#FCDF80",
                "#FFC4BE"))