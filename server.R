function(input, output) {
  
  output$main_plot <- renderPlot({
    histogram <- ggplot(faithful, aes(x = eruptions, y = ..density..)) + 
      geom_histogram(bins = as.numeric(input$n_breaks), fill = "white", color = "black") + 
      labs(x = "Duration (minutes)", title = "Geyser eruption duration") + theme_bw()
    
    scatter <- ggplot(faithful, aes(x = waiting, y = eruptions)) + 
      geom_point(size = input$pt_adjust) +
      labs(
        title = "Distribution of Old Faithful Eruptions and Waiting Time",
        x = "Waiting Time (mins)",
        y = "Eruptions Time (mins)"
      )
    
    density <- ggplot(faithful, aes(x = waiting, y = eruptions)) + 
      stat_density2d(h = c(input$bw1, input$bw2), aes(fill = ..density..),
                     geom = "tile", contour = F) +
      scale_fill_gradient2(low = "#430A4B", mid = "#64AE68", midpoint = 0.015, 
                           high = "#FFC4BE") +
      labs(
        title = "Distribution of Old Faithful Eruptions and Waiting Time",
        x = "Waiting Time (mins)",
        y = "Eruptions Time (mins)"
      )
    
    if (input$individual_obs) {
      histogram <- histogram + geom_rug()
    }
    
    if (input$density) {
      histogram <- histogram + geom_density(adjust = input$bw_adjust, color = "blue")
    }
    
    print(histogram)
    # print(scatter)
    # print(density)
  }) 
}