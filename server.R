library(gapminder)
library(geojsonio)


library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(dendextend)
library(gridExtra)



function(input, output) {

  output$scatter <- renderPlotly({
    # Filter data by state.
    if (input$region == "Far North") {
      wild_fires_sub_state <- wild_fires_sub[FN_indices,]
    } else if (input$region == "Northeast") {
      wild_fires_sub_state <- wild_fires_sub[NE_indices,]
    } else if (input$region == "Northwest") {
      wild_fires_sub_state <- wild_fires_sub[NW_indices,]
    } else if (input$region == "Southeast") {
      wild_fires_sub_state <- wild_fires_sub[SE_indices,]
    } else { # Southwest
      wild_fires_sub_state <- wild_fires_sub[SW_indices,]
    }
    
    # Scatter plot.
    p <- ggplot(data = wild_fires_sub_state, aes(x = fire_length, y = FIRE_SIZE,
                                                 text = paste("state:", STATE))) + 
      geom_point(aes(color = season)) +
      bthayill_315_theme + scale_fill_manual(values = cb_palette) +
      labs(
        title = "Fire Size vs Number of Days Until Contained",
        x = "Number of Days Until Fire Contained",
        y = "Fire Size (Acres)",
        color = "Season"
      )
    
    p_plotly <- ggplotly(p, tooltip = "text")
    return(p_plotly)
  })
  
  output$fire_location <- renderLeaflet({
    wild_fires_by_month <- subset(wild_fires, 
                                  (discovery_month == input$fire_month) &
                                  (discovery_year == input$choro_year))
    
    pal <- colorFactor(c("#430A4B", "#1D5076", "#008781", "#64AE68"),
                       domain = c("Infrastructure", "Nature", "Human", "Unknown"))
    
    m_leaflet <- leaflet(wild_fires_by_month) %>% setView(-96, 47.5, 3) %>%
      addTiles() %>% 
      addCircleMarkers(~LONGITUDE, ~LATITUDE,
                       fillOpacity = 0.5,
                       radius = 6,
                       color = ~pal(cause_type),
                       stroke = FALSE) %>%
      addLegend(pal = pal, 
                values = ~wild_fires_by_month$cause_type,
                title = "Cause of Fire")
  })
  
output$choropleth <- renderLeaflet({
    state_avg <- subset(wild_fires, discovery_month == input$choro_month) %>% 
      group_by(STATE) %>% summarize(avg_size = round(mean(FIRE_SIZE), 2))
    state_avg <- left_join(state_avg, state_data, by = c("STATE" = "state.abb"))
    
    state_info <- state_info %>% left_join(state_avg,
                                           by = c("name" = "state.name"))
    
    pal <- colorBin("BuPu", domain = state_info$avg_size,
                    bins = quantile(
                      state_info$avg_size,
                      seq(0, 1, by = 0.2), na.rm = T))
    
    labels_states <- paste0(
      "<strong>", state_info$name, 
      "</strong><br/>Average Fire Size (Acres): ",
      state_info$avg_size) %>% lapply(htmltools::HTML)
    
    m_leaflet <- leaflet(states_geo) %>% setView(-96, 47.5, 3) %>%
      addTiles() %>% 
      addPolygons(fillColor = ~pal(state_info$avg_size),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels_states,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "16px",
                    direction = "auto")) %>%
      addLegend(pal = pal,
                values = ~state_info$avg_size,
                title = "Average Fire Size (Acres)")
})
  
  output$ts_plot <- renderPlot({
    
    ts_plot <- ggplot(fires_per_day, 
                      aes(x = discovery_date, y = n_fires)) + geom_line() + 
      scale_x_date() +
      labs(title = "Distribution of Wild Fires Over Time", 
           x = "Date", 
           y = "Numbers of Fire")
    
    if (input$show_weighted_average) {
      ts_plot <- ts_plot + 
        geom_line(aes(y = fires_weighted_average_16),
                  color = "red", size = 2, alpha = 0.8)
    }
    
    if (input$show_season == "Spring") {
      ts_plot <- ts_plot + 
        geom_rect(data = temp_spring, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, alpha = 0.4, fill = "lightpink") 
    }
    
    else if (input$show_season == "Summer") {
      ts_plot <- ts_plot +
        geom_rect(data = temp_summer, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, alpha = 0.4, fill = "yellow")
    }
    
    else if (input$show_season == "Autumn") {
      ts_plot <- ts_plot +
        geom_rect(data = temp_autumn, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, alpha = 0.4, fill = "orange") 
    }
    
    else if (input$show_season == "Winter") {
      ts_plot <- ts_plot +
        geom_rect(data = temp_winter, 
                  aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                  inherit.aes = FALSE, alpha = 0.4, fill = "lightblue")
    }
    
    
    print(ts_plot)
    
  })
  
  output$wordcloud_plot1 <- renderPlot({
    month <- input$month_input1
    wild_fires2 <- subset(wild_fires, discovery_month == month)
    Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
    wordcloud_plot <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette, font = 2)
    print(wordcloud_plot)
  })
    
  output$wordcloud_plot2 <- renderPlot({
    month <- input$month_input2
    wild_fires2 <- subset(wild_fires, discovery_month == month)
    Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
    wordcloud_plot2 <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette, font = 2)
    wordcloud_plot2
    
  })
  
  output$histogram_plot1 <- renderPlot({
    state <- input$state_input1
    wild_fires3 <- subset(wild_fires, STATE == state)
    histogram_plot1 <- ggplot(data = wild_fires3, aes(x = log(FIRE_SIZE))) + 
      geom_histogram(bins = as.numeric(input$n_breaks1), color = "black") +
      scale_fill_manual(values = cb_palette) +
      bthayill_315_theme +
      labs(x = "Log (Fire Size / acre)", y = "Number of Fire") +
      ggtitle("Fire Size in States")
    print(histogram_plot1)
    
    if (input$individual_obs1) {
      histogram_plot1 <- histogram_plot1 + geom_rug()
      print(histogram_plot1)
    }
  
  })
  
  output$histogram_plot2 <- renderPlot({
    state <- input$state_input2
    wild_fires4 <- subset(wild_fires, STATE == state)
    histogram_plot2 <- ggplot(data = wild_fires4, aes(x = log(FIRE_SIZE))) + 
      geom_histogram( bins = as.numeric(input$n_breaks2),color = "black") +
      scale_fill_manual(values = cb_palette) +
      bthayill_315_theme +
      labs(x = "Log (Fire Size / acre)", y = "Number of Fire") +
      ggtitle("Fire Size in States")
    print(histogram_plot2)
    
    if (input$individual_obs2) {
      histogram_plot2 <- histogram_plot2 + geom_rug()
      print(histogram_plot2)
    }
    
  })
  
  output$state_bar <- renderPlotly({
    
    wildfire_sub_state1 <- subset(wild_fires, STATE == input$fire_state1)
    wildfire_sub_state2 <- subset(wild_fires, STATE == input$fire_state2)
    
    state_bar_plot1 <- ggplot(data = wildfire_sub_state1, 
                              aes(x = STAT_CAUSE_DESCR)) + 
      geom_bar(fill = "orange", color = "black") + 
      bthayill_315_theme +
      labs(title = "Distribution of Wild Fires Causes",
           x = "Causes of Fire",
           y = "Number of Fire") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
            axis.text.y = element_text(size = 7))
    
    state_bar_plot2 <- ggplot(data = wildfire_sub_state2, 
                              aes(x = STAT_CAUSE_DESCR)) +
      geom_bar(fill = "lightblue", color = "black") + 
      bthayill_315_theme +
      labs(title = "Distribution of Wild Fires Causes",
           x = "Causes of Fire",
           y = "Number of Fire") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9),
            axis.text.y = element_text(size = 7))
    
    subplot(state_bar_plot1, state_bar_plot2) 
  })
  
  output$cause_ts <- renderPlot({
    
    wildfire_sub1 <- subset(wild_fires, STAT_CAUSE_DESCR == input$fire_cause1)
    sub1_per_day <- wildfire_sub1 %>% 
      group_by(discovery_date) %>% 
      summarize(n_fires = n())
    wildfire_sub2 <- subset(wild_fires, STAT_CAUSE_DESCR == input$fire_cause2)
    sub2_per_day <- wildfire_sub2 %>% 
      group_by(discovery_date) %>% 
      summarize(n_fires = n())
    
    moving_average <- function(tt, time_series, ww) {
      #  Throw an error if the window width is too big
      if (ww > length(time_series))  
        stop("Window width is greater than length of time series")
      
      #  If the window width is greater than the time point, return NA
      if (tt < ww)  return(NA)
      
      return(mean(time_series[(tt-ww+1):tt]))
    }
    
    get_moving_averages <- function(time_series, ww) {
      #  Throw an error if the window width is too big
      if (ww > length(time_series))  
        stop("Window width is greater than length of time series")
      
      moving_avgs = c()
      for (i in 1:length(time_series))
      {
        moving_avgs[i] <- moving_average(i, time_series, ww)
      }
      return(moving_avgs)
    }
    
    sub1_avg_14 <- get_moving_averages(sub1_per_day$n_fires, 14)
    sub2_avg_14 <- get_moving_averages(sub2_per_day$n_fires, 14)
    sub1_per_day <- mutate(sub1_per_day, avg_14 = sub1_avg_14)
    sub2_per_day <- mutate(sub2_per_day, avg_14 = sub2_avg_14)
    
    sub1_ts_plot <- ggplot(sub1_per_day, aes(x = discovery_date, y = n_fires)) + 
      geom_line() + scale_x_date() + bthayill_315_theme +
      geom_line(aes(y = avg_14), color = "blue", size = 1.2, alpha = 0.8) +
      labs(title = paste("Distribution of Wild Fires Over Time for", 
                         input$fire_cause1), 
           x = "Date", 
           y = "Number of Fires")
    
    sub2_ts_plot <- ggplot(sub2_per_day, aes(x = discovery_date, y = n_fires)) + 
      geom_line() + scale_x_date() + bthayill_315_theme +
      geom_line(aes(y = avg_14), color = "blue", size = 1.2, alpha = 0.8) +
      labs(title = paste("Distribution of Wild Fires Over Time for", 
                         input$fire_cause2), 
           x = "Date", 
           y = "Number of Fires")
    
    grid.arrange(sub1_ts_plot, sub2_ts_plot, nrow = 2, ncol = 1)
  })
}

