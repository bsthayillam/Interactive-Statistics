library(tidyverse)
library(lubridate)

function(input, output) {
  
  output$dendrogram <- renderPlot({
    
  })
  
  output$choropleth <- renderPlot({
    
    state_data <- data_frame(state.abb, state.name = tolower(state.name))
    
    if (input$detail_level == "State") {
      state_avg <- wild_fires %>% group_by(STATE, COUNTY) %>% 
        summarize(avg_size = mean(FIRE_SIZE))
      state_avg <- left_join(state_avg, state_data, by = c("STATE" = "state.abb"))
    }
    
    else if (input$detail_level == "County") {
      county_avg <- wild_fires %>% group_by(STATE, COUNTY) %>% 
        summarize(avg_size = mean(FIRE_SIZE))
      county_avg <- left_join(state_avg, state_data, by = c("STATE" = "state.abb"))
      county_data <- map_data("county")
    }
    
  })
  
  output$ts_plot <- renderPlot({
    
  })
  
  output$txt_analysis <- renderPlot({
    
  })
  
  output$scatter <- renderPlot({
    
  })
  
  output$histogram <- renderPlot({
    
  })
  
  output$state_bar <- renderPlot({
    
  })
  
  output$month_bar <- renderPlot({
    
    if(input$month_bar_type == "Stacked") {
      ggplot(data = wild_fires, 
             aes(x = month.abb[month(wild_fires$discovery_date)], 
                 fill = FIRE_SIZE_CLASS)) + geom_bar() + bthayill_315_theme +
        scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Wild Fires by Month",
          subtitle = "A = Very Small --> G = Very Large",
          x = "Month",
          y = "Number of Wild Fires",
          fill = "Fire Size Class"
        )
    }
    else if(input$month_bar_type == "Side-by-Side") {
      ggplot(data = wild_fires, 
             aes(x = month.abb[month(wild_fires$discovery_date)], 
                 fill = FIRE_SIZE_CLASS)) + geom_bar(position = "dodge") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Wild Fires by Month",
          subtitle = "A = Very Small --> G = Very Large",
          x = "Month",
          y = "Number of Wild Fires",
          fill = "Fire Size Class"
        )
    }
    
    else if(input$month_bar_type == "Proportional") {
      ggplot(data = wild_fires, 
             aes(x = month.abb[month(wild_fires$discovery_date)], 
                 fill = FIRE_SIZE_CLASS)) + geom_bar(position = "fill") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Wild Fires by Month",
          subtitle = "A = Very Small --> G = Very Large",
          x = "Month",
          y = "Proportion of Wild Fires",
          fill = "Fire Size Class"
        )
    }
  })
}