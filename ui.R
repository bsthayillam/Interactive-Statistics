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
      menuItem("Frequency of Fires Over Time", tabName = "part_c", icon = icon("dashboard")),
      menuItem("Location of Fires", tabName = "part_e", icon = icon("dashboard")),
      menuItem("Fire Size Distribution by State", tabName = "part_g", icon = icon("dashboard")),
      menuItem("Average Fire Size by Region", tabName = "part_b", icon = icon("dashboard")),
      menuItem("Fire Duration", tabName = "part_d", icon = icon("dashboard")),
      menuItem("Fire Cause Distribution by State", tabName = "part_h", icon = icon("dashboard")),
      menuItem("Causes by Month", tabName = "part_f", icon = icon("dashboard")),
      menuItem("Fire Cause Freq. Over Time", tabName = "part_i", icon = icon("dashboard"))
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
            checkboxInput(inputId = "show_weighted_average",
                          label = strong("Show estimated weighted average"),
                          value = FALSE),
            selectInput(inputId = "show_season",
                        label = "Show Season:",
                        choices = c("Spring", "Summer", "Autumn", "Winter"),
                        selected = "Spring"),
             plotOutput(outputId = "ts_plot", height = "450px")
    ),
    
    tabItem(tabName = "part_d",
            selectInput(inputId = "region",
                        "US Region:",
                        choices = c("Far North", "Northeast", "Northwest",
                                    "Southeast", "Southwest"),
                        selected = "Southwest"),
            plotlyOutput(outputId = "scatter", height = "450px")
    ),
    tabItem(tabName = "part_e",
            selectInput(inputId = "fire_month",
                        label = "Select Month: ",
                        choices = month.abb,
                        selected = "Jan"),
            sliderInput(inputId = "choro_year",
                        label = "Select Year: ",
                        min = 2010,
                        max = 2015,
                        step = 1,
                        value = 2010),
            leafletOutput(outputId = "fire_location", height = "450px")
    ),
    tabItem(tabName = "part_f",
            fluidRow(
              column(6,
                     selectInput(inputId = "month_input1",
                                 label = "Month of the Fire: ",
                                 choices = month.abb,
                                 selected = default_month)
                     ),
              column(6,
                     selectInput(inputId = "month_input2",
                                 label = "Month of the Fire: ",
                                 choices = month.abb,
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
    
    tabItem(tabName = "part_h", 
            selectInput(inputId = "fire_state1",
                        label = "State(Left):",
                        choices = states,
                        selected = default_state),
            selectInput(inputId = "fire_state2",
                        label = "State(Right):",
                        choices = states,
                        selected = default_state),
            plotlyOutput(outputId = "state_bar")
            
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
