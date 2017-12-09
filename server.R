library(gapminder)
library(geojsonio)

library(shiny)
library(plotly)
library(leaflet)
library(dygraphs)
library(DT)

function(input, output) {
  
  output$dendrogram <- renderPlot({
    
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
  
  output$txt_analysis <- renderPlot({
    
  })
  
  output$scatter <- renderPlot({
    
  })
  
  output$histogram <- renderPlot({
    
  })
  
  output$state_bar <- renderPlot({
    
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