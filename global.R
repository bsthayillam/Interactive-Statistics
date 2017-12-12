library(tidyverse)
library(lubridate)
library(RSQLite)
library(leaflet)
library(tidyverse)
library(lubridate)
library(geojsonio)
library(RSQLite)
library(wordcloud)
library(tm)

con <- dbConnect(RSQLite::SQLite(), dbname="data.sqlite")
alltables <- dbListTables(con)
wild_fires <- dbGetQuery(con,'select * from Fires where FIRE_YEAR >= 2010')

# Convert Julian dates to normal calendar dates
wild_fires <- mutate(wild_fires,
  discovery_date = 
    as.Date(wild_fires$DISCOVERY_DATE-2451545, origin='2000-01-01'), 
  cont_date = as.Date(wild_fires$CONT_DATE-2451545, origin='2000-01-01'))

# Extract Months from fire discovery date
wild_fires <- mutate(wild_fires,
  discovery_month = month.abb[month(wild_fires$discovery_date)],
  discovery_year = year(wild_fires$discovery_date),
  discovery_day = day(wild_fires$discovery_date))

# Extract list of fire causes
fire_causes <- unique(wild_fires$STAT_CAUSE_DESCR)

bthayill_315_theme <- theme_grey() +
  theme(axis.text = element_text(size = 9, color = "violetred4"),
        axis.line = element_line(color = "black"),
        text = element_text(size = 11, face = "bold.italic", 
                            color = "darkslateblue"))

cb_palette <- rev(c("#430A4B", "#1D5076", "#008781", "#64AE68", "#DBC363", "#FCDF80",
                "#FFC4BE"))

# Geo data for U.S. Choropleth
state_vis_string <- paste0("https://raw.githubusercontent.com/python-visualization/",
                           "folium/master/examples/data/us-states.json")

states_geo = geojson_read(state_vis_string, method = "local", what = "sp")
state_info <- data.frame(name = as.character(states_geo$name) )
state_info$name <- as.character(state_info$name)

state_data <- data_frame(state.abb, state.name = state.name)

# Add categorical variable grouping together some of the fire causes.
human_cause <- c("Arson", "Fireworks", "Smoking", "Children", "Campfire")
nature_cause <- c("Lightning", "Debris Burning")
infrastructure_cause <- c("Equipment Use", "Powerline", "Railroad", "Structure")
unknown_cause <- c("Missing/Undefined", "Miscellaneous")
wild_fires <- mutate(wild_fires,
  cause_type = ifelse(STAT_CAUSE_DESCR %in% human_cause, "Human",
               ifelse(STAT_CAUSE_DESCR %in% nature_cause, "Nature", 
               ifelse(STAT_CAUSE_DESCR %in% infrastructure_cause, "Infrastructure",
                      "Undefined")))
)

years <- c(2010, 2011, 2012, 2013, 2014, 2015)
default_year = 2010
default_state = "CA"
default_month = "Jan"

states <- unique(wild_fires$STATE)


#fires_per_day dataframe
fires_per_day <- wild_fires %>% 
  group_by(discovery_date) %>% 
  summarize(n_fires = n())

hf1 <- function(tt, time_series, ww, weights = NULL) {
  if (ww > length(time_series))
    stop("Window width is greater than length of time series")
  if (is.null(weights)) weights <- rep(1/ww, ww)
  if (length(weights) != ww)
    stop("Weights should have the same length as the window width")
  if (tt < ww) return (NA)
  
  weights <- weights / sum(weights)
  return (sum(weights*time_series[(tt-ww+1):tt]))
}

hf2 <- function(time_series, ww, weights) {
  if(ww > length(time_series))
    stop("Window width is greater than length of time series")
  if (is.null(weights)) weights <- rep(1/ww, ww)
  if(length(weights) != ww)
    stop("Weights should have the same length as the window width")
  
  weights <- weights / sum(weights)
  all_average <- sapply(1:length(time_series), FUN = hf1,
                        time_series = time_series, ww = ww)
  return(all_average)
}

fires_weighted_average_16 <- hf2(fires_per_day$n_fires, 16, 
                                 weights = c(4,4,3,3,3,3,2,1,1,1,1,1,1,1,0.5,0.5))
fires_per_day$fires_weighted_average_16 <- fires_weighted_average_16

#season dataframe

temp_spring <- data.frame(
  start = as.Date(c('2010-03-01', '2011-03-01', '2012-03-01', 
                    '2013-03-01', '2014-03-01', '2015-03-01')),
  end = as.Date(c('2010-05-31', '2011-05-31', '2012-05-31', 
                  '2013-05-31', '2014-05-31', '2015-05-31'))
) 

temp_summer <- data.frame(
  start = as.Date(c('2010-06-01', '2011-06-01', '2012-06-01', 
                    '2013-06-01', '2014-06-01', '2015-06-01')),
  end = as.Date(c('2010-08-31', '2011-08-31', '2012-08-31', 
                  '2013-08-31', '2014-08-31', '2015-08-31'))
) 

temp_autumn <- data.frame(
  start = as.Date(c('2009-09-01','2010-09-01', '2011-09-01', '2012-09-01', 
                    '2013-09-01', '2014-09-01', '2015-09-01')),
  end = as.Date(c('2009-11-30','2010-11-30', '2011-11-3', '2012-11-30', 
                  '2013-11-30', '2014-11-30', '2015-11-30'))
) 

temp_winter <- data.frame(
  start = as.Date(c('2009-12-01','2010-12-01', '2011-12-01', '2012-12-01', 
                    '2013-12-01', '2014-12-01', '2015-12-01')),
  end = as.Date(c('2010-02-28','2011-02-28', '2012-02-29', '2013-02-28', 
                  '2014-02-28', '2015-02-28', '2015-12-31'))
) 


