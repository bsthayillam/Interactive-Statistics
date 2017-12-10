library(shinythemes)
library(plotly)
library(threejs)
library(dygraphs)
library(shinydashboard)
  
dashboardPage(
  dashboardHeader(title = "Wildfire Risks 2010-2015"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Clustering", tabName = "part_a", icon = icon("dashboard")),
      menuItem("Fire Size by State", tabName = "part_b", icon = icon("dashboard")),
      menuItem("Time Series", tabName = "part_c", icon = icon("dashboard")),
      menuItem("ScatterPlot", tabName = "part_d", icon = icon("dashboard")),
      menuItem("Proportional Histogram", tabName = "part_e", icon = icon("dashboard")),
      menuItem("Word Cloud", tabName = "part_f", icon = icon("dashboard")),
      menuItem("Proportional Histogram", tabName = "part_g", icon = icon("dashboard")),
      menuItem("Fire by State", tabName = "part_h", icon = icon("dashboard")),
      menuItem("Fire Size Across Month", tabName = "part_i", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "part_a",
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
    
    tabItem(tabName = "part_b",
             sliderInput(inputId = "choro_year",
                         label = "Select Year",
                         min = 2010,
                         max = 2015,
                         step = 1,
                         value = 2010),
             leafletOutput(outputId = "choropleth", height = "450px")
    ),
    
    tabItem(tabName = "part_c",
             plotlyOutput(outputId = "ts_plot", height = "450px")
    ),
    
    tabItem(tabName = "part_d",
             plotlyOutput(outputId = "scatter", height = "450px")
    ),
    
    tabItem(tabName = "part_e",
             selectInput(inputId = "fire_size_range",
                         "Fire Size Range:",
                         choices = c("Small", "Med", "Large", "Huge"),
                         selected = "Small"),
             selectInput(inputId = "hist_type",
                         label = "Histogram Chart Type:",
                         choices = c("Count", "Proportion"),
                         selected = "Count"),
             plotlyOutput(outputId = "histogram", height = "450px")
    ),
             
    tabItem(tabName = "part_f",
            selectInput(inputId = "year_input2",
                        label = "Year of the Fire: ",
                        choices = years,
                        selected = default_year),
            plotOutput("wordcloud_plot")
    ),
    
    tabItem(tabName = "part_g",
            selectInput(inputId = "year_input1",
                        label = "Year of the Fire: ",
                        choices = years,
                        selected = default_year),
            selectInput(inputId = "n_breaks",
                        label = "Number of bins in histogram (approximate):",
                        choices = c(10, 20, 35, 50),
                        selected = 20),
            checkboxInput(inputId = "individual_obs",
                          label = strong("Show individual observations"),
                          value = FALSE),
            checkboxInput(inputId = "density",
                          label = strong("Show density estimate"),
                          value = FALSE),
            conditionalPanel(condition = "input.density == true",
                             sliderInput(inputId = "bw_adjust",
                                         label = "Bandwidth adjustment:",
                                         min = 0.2, max = 2, value = 1, step = 0.2)),
            plotOutput("histogram_plot")
    ),
    
    tabItem(tabName = "part_h",
             selectInput(inputId = "state_bar_type",
                         label = "Bar Chart Type:",
                         choices = c("Stacked", "Side-by-Side", "Proportional"),
                         selected = "Stacked"),
             plotlyOutput(outputId = "state_bar")
    ),
    
    tabItem(tabName = "part_i",
             selectInput(inputId = "month_bar_type",
                         label = "Bar Chart Type:",
                         choices = c("Stacked", "Side-by-Side", "Proportional"),
                         selected = "Stacked"),
             plotlyOutput(outputId = "month_bar")
    ))
)
) 
