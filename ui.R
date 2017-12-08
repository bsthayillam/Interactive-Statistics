library(shinythemes)

bootstrapPage(
  
  theme = shinytheme("darkly"),
  
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
  
  sliderInput(inputId = "pt_adjust",
              label = "Point size adjustment:",
              min = 1, max = 5, value = 1, step = 0.5),
  
  sliderInput(inputId = "bw1",
              label = "Bandwidth 1 Adjustment:",
              min = 0, max = 3, value = 1.5, step = 0.1),
  
  sliderInput(inputId = "bw2",
              label = "Bandwidth 2 Adjustment:",
              min = 0, max = 3, value = 1.5, step = 0.1),
  
  plotOutput(outputId = "main_plot", height = "300px"),
  
  # Display this only if the density is shown
  conditionalPanel(condition = "input.density == true",
                   sliderInput(inputId = "bw_adjust",
                               label = "Bandwidth adjustment:",
                               min = 0.2, max = 2, value = 1, step = 0.2)
  )
  
)