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
  
  output$dendrogram <- renderPlot({
    # TODO(Christina): Undo hardcoding of month
    month = input$dend_month
    month = "Oct"
    
    # Only get fire data for May 2015.
    fires_may15 <- wild_fires[(wild_fires$discovery_month == month) & 
                                (wild_fires$discovery_year == 2015) &
                                (wild_fires$STATE == "CO"), ]
    # Create distance matrix using scaled continuous variables.
    fires_cont <- fires_may15[c("FIRE_SIZE", "LATITUDE", "LONGITUDE", "DISCOVERY_DATE")]
    fires_cont_scale <- scale(fires_cont, center = TRUE, scale = TRUE)
    dist_fires <- dist(fires_cont_scale)
    
    # Cluster.
    hc_fires_complete <- hclust(dist_fires, method = "complete")
    labels_complete <- cutree(hc_fires_complete, k = 4)
    get_colors <- function(x, palette = rev(cb_palette)) palette[match(x, unique(x))]
    
    # Create dendrogram.
    dend <- as.dendrogram(hc_fires_complete)
    dend_colors <- dend %>%
      set("labels_col", get_colors(labels_complete), order_value = TRUE) %>%
      set("labels_cex", 0.5)
    
    if (input$dend_label == "Cause") {
      dend_labels <- dend_colors %>%
        set("labels", fires_may15$STAT_CAUSE_DESCR, order_value = TRUE)
    } else if (input$dend_label == "County") {
      dend_labels <- dend_colors %>%
        set("labels", fires_may15$COUNTY, order_value = TRUE)
    } else if (input$dend_label == "Fire Size") {
      dend_labels <- dend_colors %>%
        set("labels", fires_may15$FIRE_SIZE, order_value = TRUE)
    }
    # TODO(Christina): Undo hardcoding of dendrogram label.
    dend_labels <- dend_colors %>%
      set("labels", fires_may15$FIRE_SIZE_CLASS, order_value = TRUE)
    
    ggplot(dend_labels, horiz = T) + bthayill_315_theme +
      labs(
        title = "2015 Colorado Fires",
        x = "Pairwise Euclidean Distance"
    )
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
    
    m_leaflet <- leaflet(states) %>% setView(-96, 47.5, 3) %>%
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
    
    fires_per_day <- wild_fires %>% 
      group_by(discovery_date) %>% 
      summarize(n_fires = n())
    
    hf1 <- function(tt, time_series, ww) {
      if (ww > length(time_series))
        stop("Window width is greater than length of time series")
      if (tt < ww) return(NA)
      else (return (mean(time_series[tt-ww+1]:tt)))
    }
    
    hf2 <- function(time_series, ww) {
      if (ww > length(time_series))
        stop("Window width is greater than length of time series")
      all_average <- sapply(1:length(time_series), FUN = hf1,
                            time_series = time_series, ww = ww)
      return(all_average)
    }
    
    hf3 <- function(tt, time_series, ww, weights = NULL) {
      if (ww > length(time_series))
        stop("Window width is greater than length of time series")
      if (is.null(weights)) weights <- rep(1/ww, ww)
      if (length(weights) != ww)
        stop("Weights should have the same length as the window width")
      if (tt < ww) return (NA)
      
      weights <- weights / sum(weights)
      return (sum(weights*time_series[(tt-ww+1):tt]))
    }
    
    hf4 <- function(time_series, ww, weights) {
      if(ww > length(time_series))
        stop("Window width is greater than length of time series")
      if (is.null(weights)) weights <- rep(1/ww, ww)
      if(length(weights) != ww)
        stop("Weights should have the same length as the window width")
      
      weights <- weights / sum(weights)
      all_average <- sapply(1:length(time_series), FUN = hf3,
                            time_series = time_series, ww = ww)
      return(all_average)
    }
    
    fires_average_16 <- hf2(fires_per_day$n_fires, 16)
    fires_per_day$fires_average_16 <- fires_average_16
    fires_weighted_average_16 <- hf4(fires_per_day$n_fires, 16, 
                                     weights = c(4,4,3,3,3,3,2,1,1,1,1,1,1,1,0.5,0.5))
    fires_per_day$fires_weighted_average_16 <- fires_weighted_average_16
    
    ts_plot <- ggplot(fires_per_day, aes(x = discovery_date, y = n_fires)) + geom_line() + 
      scale_x_date() + bthayill_315_theme +
      labs(title = "Distribution of Wild Fires Over Time", 
           x = "Date", 
           y = "Numbers of Fire")
    
    
    if (input$show_average) {
      ts_plot <- ts_plot + 
        geom_line(aes(y = fires_average_16), 
                  color = "blue", size = 1, alpha = 0.8)
    }
    
    if (input$show_weighted_average) {
      ts_plot <- ts_plot +
        geom_line(aes(y = fires_weighted_average_16),
                  color = "red", size = 2, alpha = 0.8)
    }
    
    print(ts_plot)
    
  })
  
  output$histogram <- renderPlotly({
    if (input$hist_type == "Count") { pos <- "stack" }
    else if (input$hist_type == "Proportion") { pos <- "fill"}
    
    if (input$fire_size_range == "Small") {
      small_indices = wild_fires$FIRE_SIZE_CLASS %in% c("A","B")
      wild_fires_small = wild_fires[small_indices,]
      ggplot(data = wild_fires_small, aes(x = FIRE_SIZE, fill = cause_type)) + 
        geom_histogram(binwidth = 1, position = pos, color = "black") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Fire Size",
          x = "Acres",
          y = "Number of Wild Fires",
          fill = "Cause of Fire"
        ) + theme(axis.text.y = element_text(angle = -45, hjust = 1))
    }
    
    # TODO(Christina): Ask Matey is there a way to change the x-limits.
    else if (input$fire_size_range == "Med") {
      med_indices = wild_fires$FIRE_SIZE_CLASS %in% c("C", "D", "E")
      wild_fires_med = wild_fires[med_indices,]
      
      ggplot(data = wild_fires_med, aes(x = FIRE_SIZE, fill = cause_type)) + 
        geom_histogram(binwidth = 100, position = pos, color = "black") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Fire Size",
          x = "Acres",
          y = "Number of Wild Fires",
          fill = "Cause of Fire"
        ) + theme(axis.text.y = element_text(angle = -45, hjust = 1))
    }
    
    else if (input$fire_size_range == "Large") {
      large_indices = wild_fires$FIRE_SIZE_CLASS %in% c("F")
      wild_fires_large = wild_fires[large_indices,]
      
      ggplot(data = wild_fires_large, aes(x = FIRE_SIZE, fill = cause_type)) + 
        geom_histogram(binwidth = 500, position = pos, color = "black") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Fire Size",
          x = "Acres",
          y = "Number of Wild Fires",
          fill = "Cause of Fire"
        ) + theme(axis.text.y = element_text(angle = -45, hjust = 1))
    }
    
    else if (input$fire_size_range == "Huge") {
      huge_indices = wild_fires$FIRE_SIZE_CLASS %in% c("G")
      wild_fires_huge = wild_fires[huge_indices,]
      
      ggplot(data = wild_fires_huge, aes(x = FIRE_SIZE, fill = cause_type)) + 
        geom_histogram(binwidth = 75000, position = pos, color = "black") + 
        bthayill_315_theme + scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Fire Size",
          x = "Acres",
          y = "Number of Wild Fires",
          fill = "Cause of Fire"
        ) 
    }
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
      geom_histogram(bins = 40, color = "black") +
      scale_fill_manual(values = cb_palette) +
      bthayill_315_theme +
      labs(x = "Log Fire Size", y = "Number of Fire") +
      ggtitle("Occurance of Fire at Different Latitude by Size")
    histogram_plot1
  
  })
  
  output$histogram_plot2 <- renderPlot({
    state <- input$state_input2
    wild_fires4 <- subset(wild_fires, STATE == state)
    histogram_plot2 <- ggplot(data = wild_fires4, aes(x = log(FIRE_SIZE))) + 
      geom_histogram(bins = 40,color = "black") +
      scale_fill_manual(values = cb_palette) +
      bthayill_315_theme +
      labs(x = "Log Fire Size", y = "Number of Fire") +
      ggtitle("Occurance of Fire at Different Latitude by Size")

    histogram_plot2
    
  })
  
  output$state_bar <- renderPlotly({
    if(input$state_bar_type == "Stacked") {
      ggplot(data = wild_fires,
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) + 
        geom_bar(color = "black") + bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5), 
              axis.text.y = element_text(size = 5)) +
        labs(title = "Distribution of Wild Fires by State",
                          x = "State",
                          y = "Number of Wild Fires",
                          fill = "Wild Fire Causes")
    }
    
    else if(input$state_bar_type == "Side-by-Side") {
      ggplot(data = wild_fires,
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) +
        geom_bar(position = "dodge", color = "black") + bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5), 
              axis.text.y = element_text(size = 5)) +
        labs(title = "Distribution of Wild Fires by State",
                                            x = "State",
                                            y = "Number of Wild Fires",
                                            fill = "Wild Fire Causes")
    }
    
    else if(input$state_bar_type == "Proportional") {
      ggplot(data = wild_fires, 
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) + 
        geom_bar(position = "fill", na.rm = TRUE, color = "black") + 
        bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5), 
              axis.text.y = element_text(size = 5)) +
        labs(title = "Distribution of Wild Fires by State",
                                           x = "State",
                                           y = "Number of Wild Fires",
                                           fill = "Wild Fire Causes")
    }
    
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
