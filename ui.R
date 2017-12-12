library(shinythemes)
library(plotly)
library(threejs)
library(dygraphs)
library(shinydashboard)
  
dashboardPage(
  skin = "purple",
  dashboardHeader(title = "Wildfires 2010-2015"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Number of Fires Over Time", tabName = "part_c", icon = icon("dashboard")),
      menuItem("Fire Causes Over Time", tabName = "part_i", icon = icon("dashboard")),
      menuItem("Fires by State", tabName = "part_h", icon = icon("dashboard")),
      menuItem("Fire Size by State", tabName = "part_b", icon = icon("dashboard")),
      menuItem("Side by Side Plot: Fire Size by State", tabName = "part_g", icon = icon("dashboard")),
      menuItem("Clustering", tabName = "part_a", icon = icon("dashboard")),
      menuItem("Fire Size by Cause", tabName = "part_e", icon = icon("dashboard")),
      menuItem("Word Cloud of Cause by Month", tabName = "part_f", icon = icon("dashboard"))
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
             selectInput(inputId = "choro_month",
                         label = "Select Month: ",
                         choices = month.abb),
             leafletOutput(outputId = "choropleth", height = "450px")
    ),
    
    tabItem(tabName = "part_c",
            checkboxInput(inputId = "show_average",
                          label = strong("Show estimated average"),
                          value = FALSE),
            checkboxInput(inputId = "show_weighted_average",
                          label = strong("Show estimated weighted average"),
                          value = FALSE),
             plotOutput(outputId = "ts_plot", height = "450px")
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
            fluidRow(
              column(6,
                     selectInput(inputId = "month_input1",
                                 label = "Month of the Fire: ",
                                 choices = month,
                                 selected = default_month)
                     ),
              column(6,
                     selectInput(inputId = "month_input2",
                                 label = "Month of the Fire: ",
                                 choices = month,
                                 selected = default_month)
                     
                     )
            ),
            fluidRow(
              column(6, 
                     plotOutput(outputId = "wordcloud_plot1", width  = "500px",height = "400px")
              ),
              column(6,
                     plotOutput(outputId = "wordcloud_plot2", width  = "500px",height = "400px")
              ))
    ),
    tabItem(tabName = "part_g",
            fluidRow(
              column(6,
                     selectInput(inputId = "state_input1",
                                 label = "State (Left): ",
                                 choices = states,
                                 selected = default_state)
                     ),
              column(6,
                     selectInput(inputId = "state_input2",
                                 label = "State (right): ",
                                 choices = states,
                                 selected = default_state)
                     )
            ),
            fluidRow(
              column(6,
                     selectInput(inputId = "n_breaks1",
                                 label = "Number of bins in histogram (approximate):",
                                 choices = c(10, 20, 35, 50),
                                 selected = 20)
                     ),
              column(6,
                     selectInput(inputId = "n_breaks2",
                                 label = "Number of bins in histogram (approximate):",
                                 choices = c(10, 20, 35, 50),
                                 selected = 20)
                     )
            ),
            fluidRow(
              column(6,
                     checkboxInput(inputId = "individual_obs1",
                                   label = strong("Show individual observations"),
                                   value = FALSE)
                     ),
                     column(6,
                            checkboxInput(inputId = "individual_obs2",
                                          label = strong("Show individual observations"),
                                          value = FALSE)
              )
              ),
              
            fluidRow(
              column(6, 
                     plotOutput(outputId = "histogram_plot1", width  = "500px",height = "400px")
              ),
              
              column(6, 
                     plotOutput(outputId = "histogram_plot2", width  = "500px",height = "400px")
              ))
    ),
    
    tabItem(tabName = "part_i",
             selectInput(inputId = "fire_cause1",
                         label = "First Fire Cause:",
                         choices = fire_causes,
                         selected = "Debris Burning"),
            selectInput(inputId = "fire_cause2",
                        label = "Second Fire Cause:",
                        choices = fire_causes,
                        selected = "Arson"),
             plotOutput(outputId = "cause_ts")
    ))
)
) 
