library(shinythemes)
library(plotly)
library(threejs)
library(dygraphs)
  
navbarPage(
    title = "Wildfire Risks 2010-2015",
    
    theme = shinytheme("darkly"),
    
    tabPanel("Clustering",
             selectInput(inputId = "dend_month",
                         label = "Select Month",
                         choices = month.abb,
                         selected = "Jun"),
             selectInput(inputId = "dend_label",
                         label = "Select Leaf Label",
                         choices = c("Cause", "County", "Fire Size"),
                         selected = "Cause"),
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
    
<<<<<<< HEAD
    tabPanel("Scatterplot",
             plotlyOutput(outputId = "scatter", height = "450px")
    ),
    
    tabPanel("Proportional Histogram",
             selectInput(inputId = "fire_size_range",
                         "Fire Size Range:",
                         choices = c("Small", "Med", "Large", "Huge"),
                         selected = "Small"),
             selectInput(inputId = "hist_type",
                         label = "Histogram Chart Type:",
                         choices = c("Count", "Proportion"),
                         selected = "Count"),
             plotlyOutput(outputId = "histogram", height = "450px")
    tabPanel("Word Cloud",
            selectInput(inputId = "year_input2",
                        label = "Year of the Fire: ",
                        choices = years,
                        selected = default_year),
            plotOutput("wordcloud_plot")
    ),
    
    tabPanel("Proportional Histogram",
            selectInput(inputId = "year_input1",
                        label = "Year of the Fire: ",
                        choices = years,
                        selected = default_year),
            plotOutput("histogram_plot")
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
  
