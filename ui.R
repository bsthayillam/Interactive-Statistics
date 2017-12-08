library(shinythemes)
library(plotly)
library(threejs)
library(dygraphs)
  
navbarPage(
    title = "Wildfire Risks 2010-2015",
    
    theme = shinytheme("darkly"),
    
    tabPanel("Clustering",
             plotlyOutput(outputId = "dendrogram", height = "450px")
    ),
    
    tabPanel("Chloropleth",
             selectInput(inputId = "detail_level",
                         label = "State or County Level:",
                         choices = c("County", "State"),
                         selected = "State"),
             plotlyOutput(outputId = "choropleth", height = "450px")
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
             plotlyOutput(outputId = "histogram", height = "450px")
    ),
    
    tabPanel("Fires by State",
             plotlyOutput(outputId = "state_bar", height = "450px")
    ),
    
    tabPanel("Fire Size Across Months",
             selectInput(inputId = "month_bar_type",
                         label = "Bar Chart Type:",
                         choices = c("Stacked", "Side-by-Side", "Proportional"),
                         selected = "Stacked"),
             plotOutput(outputId = "month_bar", height = "450px")
    )
  )
  