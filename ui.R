library(shiny)
library(plotly)

shinyUI(
  fluidPage(
    titlePanel("Knygų analizė"),
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "book",
              "Pasirinkite knygą:",
              choices = books
            ),
            hr(),
            sliderInput(
              "quantity",
              "Kiekis dažniausių žodžių:",
              min = 1,
              max = 150,
              value = 6
            )
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Žodžių debesis",
                plotOutput("wordcloud")
            ),
              tabPanel("Žodžio vartojimas per skyriu",
                plotlyOutput("usage")
              ),
              tabPanel("Žodžių statistiniai duomenys",
                tableOutput("statistics")
              )
            )
          )
        )
      )
)
