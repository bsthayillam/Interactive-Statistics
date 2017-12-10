library(tidyverse)
library(ggplot2)
library(RSQLite)
library(shiny)
library(shinydashboard)
library(gridExtra)
library(tm)
library(SnowballC)
library(wordcloud)

con <- dbConnect(RSQLite::SQLite(), dbname="data.sqlite")
alltables <- dbListTables(con)
# wild_fires <- dbGetQuery(con,'select * from Fires where FIRE_YEAR >= 2010')
# wild_fires <- mutate(wild_fires,
#                      discovery_date = 
#                        as.Date(wild_fires$DISCOVERY_DATE-2451545, origin='2000-01-01'), 
#                      cont_date = as.Date(wild_fires$CONT_DATE-2451545, origin='2000-01-01'))

years <- c(2010, 2011, 2012, 2013, 2014, 2015)
default_year = 2010

bthayill_315_theme <- theme_grey() +
  theme(axis.text = element_text(size = 9, color = "violetred4"),
        axis.line = element_line(color = "black"),
        text = element_text(size = 11, face = "bold.italic", 
                            color = "darkslateblue"))

cb_palette <- rev(c("#430A4B", "#1D5076", "#008781", "#64AE68", "#DBC363", "#FCDF80",
                    "#FFC4BE", "#66CCFF", "#F4F442", "#FF0000", "#0730C4"))


ui <- dashboardPage(
  dashboardHeader(title = "Group 5, Lab 11"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Histogram", tabName = "histogram", icon = icon("dashboard")),
      menuItem("Word Cloud", tabName = "wordcloud", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "histogram",
              fluidRow(
                column(12, plotOutput("histogram_plot")),
                column(12,wellPanel(
                  selectInput(inputId = "year1",
                              label = "Year of the Fire: ",
                              choices = years,
                              selected = default_year))
              )
      )),
      tabItem(tabName = "wordcloud",
              fluidRow(
                column(12, plotOutput("wordcloud_plot")),
                column(12,wellPanel(
                  selectInput(inputId = "year2",
                              label = "Year of the Fire: ",
                              choices = years,
                              selected = default_year))
                ))
              )
      )
    )
  )


server <- shinyServer(function(input, output) {
  
  output$histogram_plot <- renderPlot({
    year <- input$year1
    wild_fires1 <- dbGetQuery(con, paste('select * from Fires where FIRE_YEAR =', as.character(year), sep=" "))
    histogram_plot <- ggplot(data = wild_fires1, aes(x = LATITUDE, fill = FIRE_SIZE_CLASS)) + 
      geom_histogram(bins = 50, color = "black") +
      scale_color_manual(values = cb_palette) +
      theme_bw() +
      labs(x = "Latitude", y = "Number of Fire", fill = "Size of Fire") +
      ggtitle("whatever")
    print(histogram_plot)
  })
  
  output$wordcloud_plot <- renderPlot({
    year <- input$year2
    wild_fires2 <- dbGetQuery(con, paste('select * from Fires where FIRE_YEAR =', as.character(year), sep=" "))
    Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
    wordcloud_plot <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette)
    print(wordcloud_plot)
  })
})

shinyApp(ui = ui, server = server)
