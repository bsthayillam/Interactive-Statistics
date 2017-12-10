library(shinythemes)
library(plotly)
library(threejs)
library(dygraphs)
library(leaflet)
  
navbarPage(
    title = "Wildfire Risks 2010-2015",
    
    theme = shinytheme("darkly"),
    
    tabPanel("Clustering",
             plotlyOutput(outputId = "dendrogram", height = "450px")
    ),
    
    tabPanel("Fire Size by State",
             sliderInput(inputId = "choro_year",
                         label = "Select Year",
                         min = 2010,
                         max = 2015,
                         step = 1,
                         value = 2010),
             leafletOutput(outputId = "choropleth", height = "450px")
    ),
    
    tabPanel("Time Series",
             plotlyOutput(outputId = "ts_plot", height = "450px")
    ),
    
    tabPanel("Text Analysis", 
             plotlyOutput(outputId = "txt_analysis", height = "450px")
    ),
    
    tabPanel("Scatterplot",
             plotlyOutput(outputId = "scatter", height = "450px")
    ),
    
    tabPanel("Proportional Histogram",
            column(12, plotOutput("histogram_plot")),
            column(12,wellPanel(
               selectInput(inputId = "year_input1",
                             label = "Year of the Fire: ",
                             choices = years,
                             selected = default_year))
               )
    ),
    
    tabPanel("Fires by State",
             plotlyOutput(outputId = "state_bar", height = "450px")
    ),
    
    tabPanel("Fire Size Across Months",
             selectInput(inputId = "month_bar_type",
                         label = "Bar Chart Type:",
                         choices = c("Stacked", "Side-by-Side", "Proportional"),
                         selected = "Stacked"),
             plotlyOutput(outputId = "month_bar", height = "450px")
    )
  )
  