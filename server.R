library(gapminder)
library(geojsonio)


library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)
library(dendextend)



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
    
    state_avg <- subset(wild_fires, FIRE_YEAR == input$choro_year) %>% 
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
    
  })
  
  output$histogram <- renderPlotly({
    if (input$hist_type == "Count") { pos <- "stack" }
    else if (input$hist_type == "Proportion") { pos <- "fill"}
    
    if (input$fire_size_range == "Small") {
      small_indices = wild_fires$FIRE_SIZE_CLASS %in% c("A","B")
      wild_fires_small = wild_fires[small_indices,]
      ggplot(data = wild_fires_small, aes(x = FIRE_SIZE, fill = cause_type)) + 
        geom_histogram(binwidth = 1, position = pos) + bthayill_315_theme +
        scale_fill_manual(values = cb_palette) +
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
        geom_histogram(binwidth = 100, position = pos) + bthayill_315_theme +
        scale_fill_manual(values = cb_palette) +
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
        geom_histogram(binwidth = 500, position = pos) + bthayill_315_theme +
        scale_fill_manual(values = cb_palette) +
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
        geom_histogram(binwidth = 75000, position = pos) + bthayill_315_theme +
        scale_fill_manual(values = cb_palette) +
        labs(
          title = "Distribution of Fire Size",
          x = "Acres",
          y = "Number of Wild Fires",
          fill = "Cause of Fire"
        ) 
    }
})
  output$wordcloud_plot <- renderPlot({
    year <- input$year_input2
    wild_fires2 <- subset(wild_fires, FIRE_YEAR == as.numeric(year))
    Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
    wordcloud_plot <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette, font = 2)
    print(wordcloud_plot)
  })
  
  output$histogram_plot <- renderPlot({
    year <- input$year_input1
    wild_fires1 <- subset(wild_fires, FIRE_YEAR == as.numeric(year))
    histogram_plot <- ggplot(data = wild_fires1, aes(x = LATITUDE)) + 
      geom_histogram(bins =as.numeric(input$n_breaks),color = "black", aes(y = ..density.., fill = FIRE_SIZE_CLASS)) +
      scale_fill_manual(values = cb_palette) +
      bthayill_315_theme +
      labs(x = "Latitude", y = "Number of Fire", fill = "Size of Fire") +
      ggtitle("Occurance of Fire at Different Latitude by Size")
    
    if (input$individual_obs) {
      histogram_plot <- histogram_plot + geom_rug()
    }
    
    if (input$density) {
      histogram_plot <- histogram_plot + geom_density(adjust = input$bw_adjust, color = "blue")
    }
    print(histogram_plot)
    
  })
  
  output$state_bar <- renderPlotly({
    if(input$state_bar_type == "Stacked") {
      ggplot(data = wild_fires,
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) + 
        geom_bar() + bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
        labs(title = "Distribution of Wild Fires by State",
                          x = "State",
                          y = "Number of Wild Fires",
                          fill = "Wild Fire Causes")
    }
    
    else if(input$state_bar_type == "Side-by-Side") {
      ggplot(data = wild_fires,
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) +
        geom_bar(position = "dodge") + bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
        labs(title = "Distribution of Wild Fires by State",
                                            x = "State",
                                            y = "Number of Wild Fires",
                                            fill = "Wild Fire Causes")
    }
    
    else if(input$state_bar_type == "Proportional") {
      ggplot(data = wild_fires, 
             aes(x = STATE, fill = STAT_CAUSE_DESCR)) + 
        geom_bar(position = "fill", na.rm = TRUE) + bthayill_315_theme +
        theme(axis.text.x = element_text(angle = 40, hjust = 1)) +
        labs(title = "Distribution of Wild Fires by State",
                                           x = "State",
                                           y = "Number of Wild Fires",
                                           fill = "Wild Fire Causes")
    }
    
  })
  
  output$month_bar <- renderPlotly({
    
    if(input$month_bar_type == "Stacked") {
      ggplot(data = wild_fires, 
             aes(x = discovery_month, fill = FIRE_SIZE_CLASS)) + 
        geom_bar() + bthayill_315_theme +
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
             aes(x = discovery_month, fill = FIRE_SIZE_CLASS)) + 
        geom_bar(position = "dodge") + bthayill_315_theme + 
        scale_fill_manual(values = cb_palette) +
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
             aes(x = discovery_month, fill = FIRE_SIZE_CLASS)) + 
        geom_bar(position = "fill") + bthayill_315_theme + 
        scale_fill_manual(values = cb_palette) +
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
