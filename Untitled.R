tabItem(tabName = "part_f",
        checkboxInput(inputId = "compare",
                      label = strong("Compare Two Months"),
                      value = FALSE),
        selectInput(inputId = "month_input2",
                    label = "Month of the Fire (Left): ",
                    choices = month,
                    selected = default_month),
        conditionalPanel(condition = "input.compare == true",
                         selectInput(inputId = "month_input2",
                                     label = "Month of the Fire (Right):",
                                     choices = month,
                                     selected = default_month)),
        plotOutput("wordcloud_plot")
),


output$wordcloud_plot <- renderPlot({
  month <- input$month_input2
  wild_fires2 <- subset(wild_fires, discovery_month == month)
  Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
  wordcloud_plot <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette, font = 2)
  print(wordcloud_plot)
  
  if (input$compare) {
    month <- input$month_input2
    wild_fires2 <- subset(wild_fires, discovery_month == month)
    Corpus <- Corpus(VectorSource(wild_fires2$STAT_CAUSE_DESCR))
    wordcloud_plot2 <- wordcloud(Corpus, min.freq = 10, random.order = FALSE, colors = cb_palette, font = 2)
    attach(mtcars)
    par(mfrow=c(1, 2))
    wordcloud_plot
    wordcloud_plot2
  }
  
})
