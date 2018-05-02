library(shiny)
library(ggplot2)
library(plotly)

shinyServer(
  function(input, output) {
    
    ###
    # "Žodžių debesis" diagrama
    ###

    bookTerms <- reactive({
      getTermMatrixPerBook(input$book, input$quantity)
    })

    wordcloud_rep <- repeatable(wordcloud)

    output$wordcloud <- renderPlot({
      v <- bookTerms()
      wordcloud_rep(
        names(v),
        v,
        scale=c(4, 0.5),
        min.freq = 1,
        max.words=input$quantity,
        colors=brewer.pal(8,"Dark2")
      )
    })

    ###
    # "Žodžio vartojimas per skyriu" diagrama
    ###

    perChapterDataFrame <- reactive({
      getPerChapterDataFrame(input$book, input$quantity)
    })

    output$usage <- renderPlotly({
      df <- perChapterDataFrame()
      ggplot(
        df,
        aes(
          x=chapter,
          y=count,
          fill=word)
        ) +
        geom_bar(stat="identity")  +
        #facet_wrap kad būtų tarpai tarp lentelių
        facet_wrap(~ word) +
        theme(
          legend.position="none"
        )
    })

    
    ###
    # "Žodžių statistiniai duomenys" diagrama
    ###
    
    allWords <- reactive({
      getTermMatrixPerBook(input$book, input$quantity)
    })
    output$statistics <- renderTable(
      allWords(), rownames = TRUE, colnames = FALSE, bordered = TRUE, hover = TRUE, digits = 0
    )
  }
)
